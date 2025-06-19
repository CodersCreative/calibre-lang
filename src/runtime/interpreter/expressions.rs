use core::panic;
use std::{cell::RefCell, collections::HashMap, mem::discriminant, rc::Rc};

use crate::{
    ast::{NodeType, RefMutability},
    runtime::{
        interpreter::{InterpreterErr, evaluate},
        scope::{
            Scope, ScopeErr,
            variables::{get_var, resolve_var},
        },
        values::{RuntimeType, RuntimeValue, ValueErr, helper::Map},
    },
};

pub fn evaluate_identifier(
    identifier: &str,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    Ok(get_var(scope, identifier).clone()?)
}

pub fn evaluate_member_expression(
    exp: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::MemberExpression {
        object,
        property,
        is_computed,
    } = exp
    {
        let object = evaluate(*object, scope.clone())?;
        let prop = evaluate(*property, scope.clone())?;
        if let RuntimeValue::Struct(map, t) = object {
            if let RuntimeValue::Str(x) = prop {
                return Ok(map.0.get(&x).unwrap().clone());
            } else {
                Err(InterpreterErr::ExpectedType(prop, RuntimeType::Str))
            }
        } else {
            Err(InterpreterErr::ExpectedType(
                prop,
                RuntimeType::Struct(None),
            ))
        }
    } else {
        Err(InterpreterErr::NotImplemented(exp))
    }
}
pub fn evaluate_binary_expression(
    exp: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::BinaryExpression {
        left,
        right,
        operator,
    } = exp
    {
        let left = evaluate(*left, scope.clone())?;
        let right = evaluate(*right, scope.clone())?;

        Ok(operator.handle(left, right)?)
    } else {
        Err(InterpreterErr::NotImplemented(exp))
    }
}
pub fn evaluate_boolean_expression(
    exp: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::BooleanExpression {
        left,
        right,
        operator,
    } = exp
    {
        let left = evaluate(*left, scope.clone())?;
        let right = evaluate(*right, scope.clone())?;

        Ok(operator.handle(left, right)?)
    } else {
        Err(InterpreterErr::NotImplemented(exp))
    }
}

pub fn evaluate_comparison_expression(
    exp: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::ComparisonExpression {
        left,
        right,
        operator,
    } = exp
    {
        let left = evaluate(*left, scope.clone())?;
        let right = evaluate(*right, scope.clone())?;

        Ok(operator.handle(left, right))
    } else {
        Err(InterpreterErr::NotImplemented(exp))
    }
}
pub fn evaluate_assignment_expression(
    node: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::AssignmentExpression { identifier, value } = node {
        if let NodeType::Identifier(identifier) = *identifier {
            let value = evaluate(*value, scope.clone())?;
            scope.borrow_mut().assign_var(identifier, &value);
            return Ok(value);
        } else {
            Err(InterpreterErr::AssignNonVariable(*identifier))
        }
    } else {
        Err(InterpreterErr::NotImplemented(node))
    }
}

pub fn evaluate_struct_expression(
    obj: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    let mut properties = HashMap::new();

    if let NodeType::StructLiteral(props) = obj {
        for (k, v) in props {
            let value = if let Some(value) = v {
                evaluate(value, scope.clone())?
            } else {
                get_var(scope.clone(), &k)?
            };

            properties.insert(k, value);
        }
    }

    Ok(RuntimeValue::Struct(Map(properties), None))
}

pub fn evaluate_list_expression(
    obj: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    let mut values = Vec::new();

    if let NodeType::ListLiteral(vals) = obj {
        for val in vals.iter() {
            values.push(evaluate(val.clone(), scope.clone())?);
        }
    }

    let t = if values.len() > 0 {
        let t = discriminant(&values[0]);
        let filtered: Vec<&RuntimeValue> =
            values.iter().filter(|x| discriminant(*x) == t).collect();
        if values.len() == filtered.len() {
            Some(values[0].clone().into())
        } else {
            None
        }
    } else {
        None
    };

    Ok(RuntimeValue::List {
        data: values,
        data_type: Box::new(t),
    })
}
pub fn evaluate_call_expression(
    exp: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::CallExpression(caller, arguments) = exp {
        let func = evaluate(*caller.clone(), scope.clone())?;

        match func {
            RuntimeValue::Function {
                identifier,
                parameters,
                body,
                return_type,
                is_async,
            } => {
                let scope = Rc::new(RefCell::new(Scope::new(Some(scope.clone()))));

                for (i, (k, v, m)) in parameters.iter().enumerate() {
                    match m {
                        RefMutability::MutRef | RefMutability::Ref => {
                            if let NodeType::Identifier(x) = &arguments[i] {
                                let (env, name) = resolve_var(scope.clone(), x)?;

                                if m == &RefMutability::MutRef
                                    && env.borrow().constants.contains_key(&name)
                                {
                                    return Err(InterpreterErr::MutRefNonMut(
                                        env.borrow().constants.get(&name).unwrap().clone(),
                                    ));
                                }

                                let var = get_var(env, &name)?;
                                let x = name.clone();
                                if var.is_type(scope.clone(), v.clone()) {
                                    scope.borrow_mut().alias.insert(k.to_string(), x);
                                    scope.borrow_mut().push_var(
                                        k.to_string(),
                                        &RuntimeValue::Null,
                                        match m {
                                            RefMutability::MutRef | RefMutability::MutValue => true,
                                            _ => false,
                                        },
                                    );
                                } else {
                                    return Err(InterpreterErr::UnexpectedType(var));
                                }
                            } else {
                                return Err(InterpreterErr::RefNonVar(arguments[0].clone()));
                            }
                        }
                        _ => {
                            let arg = evaluate(arguments[i].clone(), scope.clone())?
                                .into_type(scope.clone(), v.clone())?;
                            scope.borrow_mut().push_var(
                                k.to_string(),
                                &arg,
                                match m {
                                    RefMutability::MutRef | RefMutability::MutValue => true,
                                    _ => false,
                                },
                            )?;
                        }
                    }
                }

                let mut result: RuntimeValue = RuntimeValue::Null;
                for statement in &body.0 {
                    result = evaluate(statement.clone(), scope.clone())?;
                }

                if let Some(t) = return_type {
                    return Ok(result.into_type(scope, t.clone())?);
                } else {
                    return Ok(RuntimeValue::Null);
                }
            }
            RuntimeValue::List { data, data_type } if arguments.len() == 1 => {
                match evaluate(arguments[0].clone(), scope)? {
                    RuntimeValue::Integer(i) if arguments.len() == 1 => {
                        return Ok(data
                            .get(i as usize)
                            .expect("Tried to get index that is larger than list size")
                            .clone());
                    }
                    _ => return Err(InterpreterErr::IndexNonList(arguments[0].clone())),
                }
            }
            RuntimeValue::NativeFunction(_) => {
                let mut evaluated_arguments = Vec::new();

                for arg in arguments.iter() {
                    evaluated_arguments.push(evaluate(arg.clone(), scope.clone())?);
                }

                return Ok(func.call_native(evaluated_arguments, scope));
            }
            _ => {}
        }

        if let NodeType::Identifier(caller) = *caller.clone() {
            if let Ok(scope_b) = resolve_var(scope, &caller) {
                if scope_b.0.borrow().variables.contains_key(&scope_b.1) {
                    if arguments.len() <= 0 {
                        return Ok(get_var(scope_b.0, &scope_b.1)?);
                    } else if arguments.len() == 1 {
                        scope_b.0.borrow_mut().assign_var(
                            caller,
                            &evaluate(arguments[0].clone(), scope_b.0.clone())?,
                        );
                        return Ok(RuntimeValue::Null);
                    } else {
                        return Err(InterpreterErr::SetterArgs(arguments));
                    }
                } else if let Some(var) = scope_b.0.borrow().constants.get(&scope_b.1) {
                    match var {
                        NativeFunctions => {}
                        _ => {
                            if arguments.len() <= 0 {
                                return Ok(get_var(scope_b.0.clone(), &scope_b.1)?);
                            } else {
                                return Err(InterpreterErr::Value(ValueErr::Scope(
                                    ScopeErr::AssignConstant(scope_b.1),
                                )));
                            }
                        }
                    }
                }
            }
        }
        panic!("Cannot call non-variable or function value, {:?}", func);
    } else {
        Err(InterpreterErr::NotImplemented(exp))
    }
}
