use core::panic;
use std::{cell::RefCell, collections::HashMap, mem::discriminant, rc::Rc};

use crate::{
    ast::{NodeType, RefMutability},
    runtime::{
        interpreter::evaluate,
        scope::{
            Scope,
            variables::{get_var, resolve_var, safe_resolve_var},
        },
        values::{RuntimeType, RuntimeValue, helper::Map},
    },
};

pub fn evaluate_identifier(identifier: &str, scope: Rc<RefCell<Scope>>) -> RuntimeValue {
    get_var(scope, identifier).clone()
}

pub fn evaluate_member_expression(exp: NodeType, scope: Rc<RefCell<Scope>>) -> RuntimeValue {
    if let NodeType::MemberExpression {
        object,
        property,
        is_computed,
    } = exp
    {
        let object = evaluate(*object, scope.clone());
        let prop = evaluate(*property, scope.clone());
        println!("mem {:?} , {:?}", &object, &prop);
        if let RuntimeValue::Struct(map, t) = object {
            if let RuntimeValue::Str(x) = prop {
                return map.0.get(&x).unwrap().clone();
            } else {
                panic!("Variable is not a string");
            }
        } else {
            panic!("Variable is not a map");
        }
    } else {
        panic!("Tried to evaluate non-binary-expression node using evaluate_binary_expression.")
    }
}
pub fn evaluate_binary_expression(exp: NodeType, scope: Rc<RefCell<Scope>>) -> RuntimeValue {
    if let NodeType::BinaryExpression {
        left,
        right,
        operator,
    } = exp
    {
        let left = evaluate(*left, scope.clone());
        let right = evaluate(*right, scope.clone());

        operator.handle(left, right)
    } else {
        panic!("Tried to evaluate non-binary-expression node using evaluate_binary_expression.")
    }
}
pub fn evaluate_boolean_expression(exp: NodeType, scope: Rc<RefCell<Scope>>) -> RuntimeValue {
    if let NodeType::BooleanExpression {
        left,
        right,
        operator,
    } = exp
    {
        let left = evaluate(*left, scope.clone());
        let right = evaluate(*right, scope.clone());

        operator.handle(left, right)
    } else {
        panic!("Tried to evaluate non-binary-expression node using evaluate_binary_expression.")
    }
}

pub fn evaluate_comparison_expression(exp: NodeType, scope: Rc<RefCell<Scope>>) -> RuntimeValue {
    if let NodeType::ComparisonExpression {
        left,
        right,
        operator,
    } = exp
    {
        let left = evaluate(*left, scope.clone());
        let right = evaluate(*right, scope.clone());

        operator.handle(left, right)
    } else {
        panic!("Tried to evaluate non-binary-expression node using evaluate_binary_expression.")
    }
}
pub fn evaluate_assignment_expression(node: NodeType, scope: Rc<RefCell<Scope>>) -> RuntimeValue {
    if let NodeType::AssignmentExpression { identifier, value } = node {
        if let NodeType::Identifier(identifier) = *identifier {
            let value = evaluate(*value, scope.clone());
            scope.borrow_mut().assign_var(identifier, &value);
            value
        } else {
            panic!(
                "Tried to evaluate non-assignment-expression node using evaluate_assignment_expression."
            )
        }
    } else {
        panic!(
            "Tried to evaluate non-assignment-expression node using evaluate_assignment_expression."
        )
    }
}

pub fn evaluate_struct_expression(obj: NodeType, scope: Rc<RefCell<Scope>>) -> RuntimeValue {
    let mut properties = HashMap::new();

    if let NodeType::StructLiteral(props) = obj {
        for (k, v) in props {
            let value = if let Some(value) = v {
                evaluate(value, scope.clone())
            } else {
                get_var(scope.clone(), &k)
            };

            properties.insert(k, value);
        }
    }

    RuntimeValue::Struct(Map(properties), None)
}

pub fn evaluate_list_expression(obj: NodeType, scope: Rc<RefCell<Scope>>) -> RuntimeValue {
    let mut values = Vec::new();

    if let NodeType::ListLiteral(vals) = obj {
        values = vals
            .iter()
            .map(|v| evaluate(v.clone(), scope.clone()))
            .collect();
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

    RuntimeValue::List {
        data: values,
        data_type: Box::new(t),
    }
}
pub fn evaluate_call_expression(exp: NodeType, scope: Rc<RefCell<Scope>>) -> RuntimeValue {
    if let NodeType::CallExpression(caller, arguments) = exp {
        let evaluated_arguments: Vec<RuntimeValue> = arguments
            .iter()
            .map(|x| evaluate(x.clone(), scope.clone()))
            .collect();

        let func = evaluate(*caller.clone(), scope.clone());

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
                    println!("{:?}", m);
                    match m {
                        RefMutability::MutRef | RefMutability::Ref => {
                            if let NodeType::Identifier(x) = &arguments[i] {
                                let (env, name) = resolve_var(scope.clone(), x);
                                // let env_b = env.borrow_mut();
                                if m == &RefMutability::MutRef
                                    && env.borrow().constants.contains_key(&name)
                                {
                                    panic!("Cannot mutably reference a non-mutable value");
                                }
                                let var = get_var(env, &name).clone();
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
                                    panic!("Variable is of wrong type");
                                }
                            } else {
                                panic!("Can only reference a variable");
                            }
                        }
                        _ => {
                            let arg = evaluated_arguments[i].into_type(scope.clone(), v.clone());
                            scope.borrow_mut().push_var(
                                k.to_string(),
                                &arg,
                                match m {
                                    RefMutability::MutRef | RefMutability::MutValue => true,
                                    _ => false,
                                },
                            );
                        }
                    }
                }

                let mut result: RuntimeValue = RuntimeValue::Null;
                for statement in &body.0 {
                    result = evaluate(statement.clone(), scope.clone());
                }

                if let Some(t) = return_type {
                    return result.into_type(scope, t.clone());
                } else {
                    return RuntimeValue::Null;
                }
            }
            RuntimeValue::List { data, data_type } if arguments.len() == 1 => {
                match evaluated_arguments[0] {
                    RuntimeValue::Integer(i) if arguments.len() == 1 => {
                        return data
                            .get(i as usize)
                            .expect("Tried to get index that is larger than list size")
                            .clone();
                    }
                    _ => panic!("Cannot index with value other than int"),
                }
            }
            RuntimeValue::NativeFunction(_) => return func.call_native(evaluated_arguments, scope),
            _ => {}
        }

        if let NodeType::Identifier(caller) = *caller.clone() {
            if let Some(scope) = safe_resolve_var(scope, &caller) {
                if scope.0.borrow().variables.contains_key(&scope.1) {
                    if arguments.len() <= 0 {
                        return get_var(scope.0, &scope.1).clone();
                    } else if arguments.len() == 1 {
                        scope
                            .0
                            .borrow_mut()
                            .assign_var(caller, &evaluated_arguments[0]);
                        return RuntimeValue::Null;
                    } else {
                        panic!(
                            "Setters cant have more than one value or function has same identifier as variable."
                        );
                    }
                } else if let Some(var) = scope.0.borrow().constants.get(&scope.1) {
                    match var {
                        NativeFunctions => {}
                        _ => {
                            if arguments.len() <= 0 {
                                return get_var(scope.0.clone(), &scope.1).clone();
                            } else {
                                panic!("Cannot set constant");
                            }
                        }
                    }
                }
            }
        }
        panic!("Cannot call non-variable or function value, {:?}", func);
    } else {
        panic!(
            "Tried to evaluate non-assignment-expression node using evaluate_assignment_expression."
        )
    }
}
