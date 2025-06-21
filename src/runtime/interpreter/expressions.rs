use core::panic;
use std::{cell::RefCell, collections::HashMap, mem::discriminant, rc::Rc};

use crate::{
    ast::{NodeType, RefMutability},
    runtime::{
        interpreter::{InterpreterErr, evaluate},
        scope::{
            Scope, ScopeErr, StopValue,
            enums::get_enum,
            structs::get_struct_function,
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

pub fn assign_member_expression(
    member: NodeType,
    value: RuntimeValue,
    scope: Rc<RefCell<Scope>>,
) -> Result<(), InterpreterErr> {
    match member {
        NodeType::MemberExpression {
            object, property, ..
        } => {
            let mut object_val = evaluate(*object.clone(), scope.clone())?;
            match *property {
                NodeType::MemberExpression { .. } => {
                    assign_member_expression(*property, value, scope)
                }
                NodeType::Identifier(prop) => {
                    if let RuntimeValue::Struct(ref mut map, _) = object_val {
                        if let Some(field) = map.0.get_mut(&prop) {
                            *field = value;
                            if let NodeType::Identifier(obj_name) = *object {
                                scope.borrow_mut().assign_var(&obj_name, object_val)?;
                            }
                            Ok(())
                        } else {
                            Err(InterpreterErr::Value(ValueErr::Scope(ScopeErr::Variable(
                                prop,
                            ))))
                        }
                    } else if let RuntimeValue::List { ref mut data, .. } = object_val {
                        let idx_val = evaluate(NodeType::Identifier(prop.clone()), scope.clone())?
                            .into_type(scope.clone(), RuntimeType::Integer)?;
                        if let RuntimeValue::Integer(idx) = idx_val {
                            if let Some(elem) = data.get_mut(idx as usize) {
                                *elem = value;
                                if let NodeType::Identifier(obj_name) = *object {
                                    scope.borrow_mut().assign_var(&obj_name, object_val)?;
                                }
                                Ok(())
                            } else {
                                Err(InterpreterErr::IndexNonList(NodeType::Identifier(prop)))
                            }
                        } else {
                            Err(InterpreterErr::IndexNonList(NodeType::Identifier(prop)))
                        }
                    } else {
                        Err(InterpreterErr::UnexpectedType(object_val))
                    }
                }
                _ => Err(InterpreterErr::NotImplemented(*property)),
            }
        }
        _ => Err(InterpreterErr::NotImplemented(member)),
    }
}

pub fn evaluate_member_expression(
    exp: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    match exp {
        NodeType::MemberExpression {
            object, property, ..
        } => {
            let object_val = evaluate(*object.clone(), scope.clone())?;
            match *property {
                NodeType::MemberExpression { .. } => evaluate_member_expression(*property, scope),
                NodeType::Identifier(ref prop) => {
                    if let RuntimeValue::Struct(ref map, _) = object_val {
                        if let Some(val) = map.0.get(prop) {
                            Ok(val.clone())
                        } else {
                            Err(InterpreterErr::Value(ValueErr::Scope(ScopeErr::Variable(
                                prop.clone(),
                            ))))
                        }
                    } else if let RuntimeValue::List { ref data, .. } = object_val {
                        let idx_val = evaluate(NodeType::Identifier(prop.clone()), scope.clone())?
                            .into_type(scope.clone(), RuntimeType::Integer)?;
                        if let RuntimeValue::Integer(idx) = idx_val {
                            data.get(idx as usize).cloned().ok_or_else(|| {
                                InterpreterErr::IndexNonList(NodeType::Identifier(prop.clone()))
                            })
                        } else {
                            Err(InterpreterErr::IndexNonList(NodeType::Identifier(
                                prop.clone(),
                            )))
                        }
                    } else {
                        Err(InterpreterErr::UnexpectedType(object_val))
                    }
                }
                NodeType::CallExpression(caller, args) => {
                    if let NodeType::Identifier(ref method_name) = *caller {
                        if let RuntimeValue::Struct(_, Some(ref struct_name)) = object_val {
                            if let Ok(val) =
                                get_struct_function(scope.clone(), struct_name, method_name)
                            {
                                let mut arguments = vec![NodeType::Identifier(struct_name.clone())];
                                arguments.extend(*args);
                                return evaluate_function(scope, val.0, arguments);
                            }
                        }
                    }
                    panic!()
                }
                _ => Err(InterpreterErr::NotImplemented(*property)),
            }
        }
        _ => Err(InterpreterErr::NotImplemented(exp)),
    }
}

pub fn evaluate_not<'a>(
    exp: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::NotExpression { value } = exp {
        let value = evaluate(*value, scope.clone())?;

        match value {
            RuntimeValue::Bool(x) => Ok(RuntimeValue::Bool(!x)),
            RuntimeValue::Integer(x) => Ok(RuntimeValue::Integer(-x)),
            RuntimeValue::Float(x) => Ok(RuntimeValue::Float(-x)),
            RuntimeValue::Range(f, t) => Ok(RuntimeValue::Range(t, f)),
            RuntimeValue::List {
                mut data,
                data_type,
            } => {
                data.reverse();
                Ok(RuntimeValue::List { data, data_type })
            }
            _ => Err(InterpreterErr::UnexpectedType(value)),
        }
    } else {
        Err(InterpreterErr::NotImplemented(exp))
    }
}

pub fn evaluate_enum_expression(
    exp: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::EnumExpression {
        identifier,
        value,
        data,
    } = exp
    {
        let enm_class = get_enum(scope.clone(), &identifier)?;
        if let Some((i, enm)) = enm_class.iter().enumerate().find(|x| &x.1.0 == &value) {
            let mut new_data_vals = HashMap::new();
            if let Some(properties) = &enm.1 {
                let mut data_vals = HashMap::new();
                if let Some(data) = data {
                    for (k, v) in data {
                        let value = if let Some(value) = v {
                            evaluate(value, scope.clone())?
                        } else {
                            get_var(scope.clone(), &k)?
                        };

                        data_vals.insert(k, value);
                    }
                }

                for property in properties {
                    if let Some(val) = data_vals.get(property.0) {
                        new_data_vals.insert(
                            property.0.clone(),
                            val.into_type(scope.clone(), property.1.clone())?,
                        );
                    } else {
                        panic!();
                    }
                }
            }

            let data = if new_data_vals.is_empty() {
                None
            } else {
                Some(Map(new_data_vals))
            };

            Ok(RuntimeValue::Enum(identifier, i, data))
        } else {
            Err(InterpreterErr::UnexpectedEnumItem(identifier, value))
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

pub fn evaluate_range_expression(
    exp: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::RangeDeclaration {
        from,
        to,
        inclusive,
    } = exp
    {
        if let RuntimeValue::Integer(from) = evaluate(*from.clone(), scope.clone())?
            .into_type(scope.clone(), RuntimeType::Integer)?
        {
            if let RuntimeValue::Integer(to) = evaluate(*to.clone(), scope.clone())?
                .into_type(scope.clone(), RuntimeType::Integer)?
            {
                let to = if inclusive { to + 1 } else { to };

                Ok(RuntimeValue::Range(from as i32, to as i32))
            } else {
                Err(InterpreterErr::NotImplemented(*to))
            }
        } else {
            Err(InterpreterErr::NotImplemented(*from))
        }
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
            let _ = scope.borrow_mut().assign_var(&identifier, value.clone())?;
            return Ok(value);
        }
        if let NodeType::MemberExpression { .. } = *identifier {
            let value = evaluate(*value, scope.clone())?;
            let _ = assign_member_expression(*identifier, value.clone(), scope)?;
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

pub fn get_new_scope(
    scope: Rc<RefCell<Scope>>,
    parameters: Vec<(String, RuntimeType, RefMutability)>,
    arguments: Vec<NodeType>,
) -> Result<Rc<RefCell<Scope>>, InterpreterErr> {
    let new_scope = Rc::new(RefCell::new(Scope::new(Some(scope.clone()))));

    for (i, (k, v, m)) in parameters.iter().enumerate() {
        match m {
            RefMutability::MutRef | RefMutability::Ref => {
                if let NodeType::Identifier(x) = &arguments[i] {
                    let (env, name) = resolve_var(new_scope.clone(), x)?;

                    if m == &RefMutability::MutRef && env.borrow().constants.contains_key(&name) {
                        return Err(InterpreterErr::MutRefNonMut(
                            env.borrow().constants.get(&name).unwrap().clone(),
                        ));
                    }
                    let var = get_var(env, &name)?;
                    let x = name.clone();
                    if var.is_type(scope.clone(), v.clone()) {
                        new_scope.borrow_mut().alias.insert(k.to_string(), x);
                        let _ = new_scope.borrow_mut().push_var(
                            k.to_string(),
                            &RuntimeValue::Null,
                            match m {
                                RefMutability::MutRef | RefMutability::MutValue => true,
                                _ => false,
                            },
                        )?;
                    } else {
                        return Err(InterpreterErr::UnexpectedType(var));
                    }
                } else {
                    return Err(InterpreterErr::RefNonVar(arguments[0].clone()));
                }
            }
            _ => {
                let arg = evaluate(arguments[i].clone(), new_scope.clone())?
                    .into_type(new_scope.clone(), v.clone())?;
                new_scope.borrow_mut().push_var(
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

    Ok(new_scope)
}

pub fn evaluate_scope(
    node: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::ScopeDeclaration { body } = node {
        let new_scope = get_new_scope(scope, Vec::new(), Vec::new())?;

        let mut result: RuntimeValue = RuntimeValue::Null;
        for statement in body.into_iter() {
            if let Some(StopValue::Return) = new_scope.borrow().stop {
                break;
            } else if let NodeType::Return { value } = statement {
                result = evaluate(*value.clone(), new_scope.clone())?;
                new_scope.borrow_mut().stop = Some(StopValue::Return);
                break;
            } else {
                result = evaluate(statement, new_scope.clone())?;
            }
        }

        Ok(result)
    } else {
        panic!()
    }
}

pub fn evaluate_function(
    scope: Rc<RefCell<Scope>>,
    func: RuntimeValue,
    arguments: Vec<NodeType>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let RuntimeValue::Function {
        identifier,
        parameters,
        body,
        return_type,
        is_async,
    } = func
    {
        let new_scope = get_new_scope(scope, parameters, arguments)?;

        let mut result: RuntimeValue = RuntimeValue::Null;
        for statement in &body.0 {
            if let Some(StopValue::Return) = new_scope.borrow().stop {
                break;
            } else if let NodeType::Return { value } = statement {
                result = evaluate(*value.clone(), new_scope.clone())?;
                break;
            } else {
                result = evaluate(statement.clone(), new_scope.clone())?;
            }
        }

        if let Some(t) = return_type {
            return Ok(result.into_type(new_scope, t.clone())?);
        } else {
            return Ok(RuntimeValue::Null);
        }
    } else {
        panic!()
    }
}

pub fn evaluate_call_expression(
    exp: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::CallExpression(caller, arguments) = exp {
        let func = evaluate(*caller.clone(), scope.clone())?;

        match func {
            RuntimeValue::Function { .. } => {
                return evaluate_function(scope, func, *arguments);
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
                        let _ = scope_b.0.borrow_mut().assign_var(
                            &caller,
                            evaluate(arguments[0].clone(), scope_b.0.clone())?,
                        )?;
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
