use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{NodeType, RefMutability, comparison::is_equal},
    runtime::{
        interpreter::{
            InterpreterErr, evaluate,
            expressions::{
                call::evaluate_function,
                scope::{evaluate_scope, get_new_scope_with_values},
            },
            statements::comparisons::is_value_in,
        },
        scope::{
            Object, Scope, ScopeErr,
            objects::get_object,
            variables::{get_global_scope, get_stop},
        },
        values::{
            RuntimeType, RuntimeValue,
            helper::{ObjectType, StopValue},
        },
    },
};

fn match_inner_pattern(
    pattern: &NodeType,
    value: &RuntimeValue,
    scope: Rc<RefCell<Scope>>,
    conditionals: &[NodeType],
    body: &[NodeType],
) -> Option<Result<RuntimeValue, InterpreterErr>> {
    match pattern {
        NodeType::Identifier(x)
            if x.trim() == "_"
                && handle_conditionals(scope.clone(), conditionals.to_vec()).unwrap() =>
        {
            Some(evaluate_scope(
                NodeType::ScopeDeclaration {
                    body: body.to_vec(),
                },
                scope,
            ))
        }
        NodeType::EnumExpression {
            identifier,
            data,
            value: enm_value,
        } => {
            if let RuntimeValue::Enum(iden, val, dat) = value.clone() {
                let Object::Enum(enm) = get_object(scope.clone(), &iden).unwrap() else {
                    return None;
                };
                let Some(index) = enm.iter().position(|x| &x.0 == enm_value) else {
                    return None;
                };

                if index != val {
                    return None;
                }

                let Some(data) = data else {
                    if !handle_conditionals(scope.clone(), conditionals.to_vec()).unwrap() {
                        return None;
                    }
                    return Some(evaluate_scope(
                        NodeType::ScopeDeclaration {
                            body: body.to_vec(),
                        },
                        scope,
                    ));
                };

                let Some(dat) = dat else {
                    if !handle_conditionals(scope.clone(), conditionals.to_vec()).unwrap() {
                        return None;
                    }
                    return Some(evaluate_scope(
                        NodeType::ScopeDeclaration {
                            body: body.to_vec(),
                        },
                        scope,
                    ));
                };

                if identifier != &iden {
                    return None;
                };
                let mut new_scope = None;
                match data {
                    ObjectType::Map(map) => {
                        if let ObjectType::Map(m) = dat {
                            new_scope = Some(
                                get_new_scope_with_values(
                                    scope.clone(),
                                    m.into_iter()
                                        .filter(|(k, _)| map.contains_key(k))
                                        .map(|(k, v)| {
                                            if let Some(NodeType::Identifier(k)) =
                                                map.get(&k).unwrap()
                                            {
                                                (k.to_string(), v, RefMutability::MutValue)
                                            } else {
                                                (k.to_string(), v, RefMutability::MutValue)
                                            }
                                        })
                                        .collect(),
                                )
                                .unwrap(),
                            );
                        }
                    }
                    ObjectType::Tuple(list) => {
                        if let ObjectType::Tuple(lst) = dat {
                            let list: Vec<String> = list
                                .into_iter()
                                .map(|x| {
                                    let NodeType::Identifier(y) = x.clone().unwrap() else {
                                        panic!()
                                    };
                                    y
                                })
                                .collect();
                            new_scope = Some(
                                get_new_scope_with_values(
                                    scope.clone(),
                                    lst.into_iter()
                                        .enumerate()
                                        .map(|(i, v)| (list[i].clone(), v, RefMutability::MutValue))
                                        .collect(),
                                )
                                .unwrap(),
                            );
                        } else if let ObjectType::Map(map) = dat {
                            let list: Vec<String> = list
                                .into_iter()
                                .map(|x| {
                                    let NodeType::Identifier(y) = x.clone().unwrap() else {
                                        panic!()
                                    };
                                    y
                                })
                                .collect();

                            new_scope = Some(
                                get_new_scope_with_values(
                                    scope.clone(),
                                    map.into_iter()
                                        .filter(|x| list.contains(&x.0))
                                        .map(|(k, v)| (k, v, RefMutability::MutValue))
                                        .collect(),
                                )
                                .unwrap(),
                            );
                        }
                    }
                }
                if let Some(new_scope) = new_scope {
                    if !handle_conditionals(new_scope.clone(), conditionals.to_vec()).unwrap() {
                        return None;
                    }
                    let mut result: RuntimeValue = RuntimeValue::Null;
                    let global = get_global_scope(new_scope.clone());
                    for statement in body {
                        if let Some(StopValue::Return) = global.borrow().stop {
                            break;
                        } else {
                            result = evaluate(statement.clone(), new_scope.clone()).unwrap();
                        }
                    }

                    Some(Ok(result))
                } else {
                    None
                }
            } else {
                None
            }
        }
        NodeType::CallExpression(callee, args) => {
            if let NodeType::Identifier(variant) = &**callee {
                match (variant.as_str(), value) {
                    ("Some", RuntimeValue::Option(Some(inner), _)) => {
                        if let Some(arg_pat) = args.get(0) {
                            match match_inner_pattern(
                                &arg_pat.0,
                                inner,
                                scope.clone(),
                                conditionals,
                                body,
                            ) {
                                Some(Ok(x)) => Some(Ok(x)),
                                Some(Err(InterpreterErr::ExpectedFunctions)) => None,
                                Some(Err(e)) => Some(Err(e)),
                                None => None,
                            }
                        } else {
                            None
                        }
                    }
                    ("None", RuntimeValue::Option(None, _)) => Some(evaluate_scope(
                        NodeType::ScopeDeclaration {
                            body: body.to_vec(),
                        },
                        scope,
                    )),
                    ("Ok", RuntimeValue::Result(Ok(inner), _)) => {
                        if let Some(arg_pat) = args.get(0) {
                            match match_inner_pattern(
                                &arg_pat.0,
                                inner,
                                scope.clone(),
                                conditionals,
                                body,
                            ) {
                                Some(Ok(x)) => Some(Ok(x)),
                                Some(Err(InterpreterErr::ExpectedFunctions)) => None,
                                Some(Err(e)) => Some(Err(e)),
                                None => None,
                            }
                        } else {
                            None
                        }
                    }
                    ("Err", RuntimeValue::Result(Err(inner), _)) => {
                        if let Some(arg_pat) = args.get(0) {
                            match match_inner_pattern(
                                &arg_pat.0,
                                inner,
                                scope.clone(),
                                conditionals,
                                body,
                            ) {
                                Some(Ok(x)) => Some(Ok(x)),
                                Some(Err(InterpreterErr::ExpectedFunctions)) => None,
                                Some(Err(e)) => Some(Err(e)),
                                None => None,
                            }
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            } else {
                None
            }
        }
        NodeType::Identifier(var_name) => {
            let new_scope = get_new_scope_with_values(
                scope.clone(),
                vec![(var_name.clone(), value.clone(), RefMutability::MutValue)],
            )
            .ok()?;
            if handle_conditionals(new_scope.clone(), conditionals.to_vec()).ok()? {
                Some(evaluate_scope(
                    NodeType::ScopeDeclaration {
                        body: body.to_vec(),
                    },
                    new_scope,
                ))
            } else {
                None
            }
        }
        _ => {
            if let Ok(x) = evaluate(pattern.clone(), scope.clone()) {
                if (is_equal(x.clone(), value.clone(), scope.clone())
                    || is_value_in(value.clone(), x.clone(), scope.clone()))
                    && handle_conditionals(scope.clone(), conditionals.to_vec()).ok()?
                {
                    Some(evaluate_scope(
                        NodeType::ScopeDeclaration {
                            body: body.to_vec(),
                        },
                        scope,
                    ))
                } else {
                    None
                }
            } else {
                None
            }
        }
    }
}

fn match_pattern(
    pattern: &NodeType,
    value: &RuntimeValue,
    scope: Rc<RefCell<Scope>>,
    conditionals: &[NodeType],
    body: &[NodeType],
) -> Option<Result<RuntimeValue, InterpreterErr>> {
    use crate::ast::NodeType;
    match evaluate(pattern.clone(), scope.clone()) {
        Ok(x)
            if (is_equal(x.clone(), value.clone(), scope.clone())
                || is_value_in(value.clone(), x.clone(), scope.clone()))
                && handle_conditionals(scope.clone(), conditionals.to_vec()).unwrap() =>
        {
            return Some(evaluate_scope(
                NodeType::ScopeDeclaration {
                    body: body.to_vec(),
                },
                scope,
            ));
        }
        Ok(_) => None,
        Err(_) => match_inner_pattern(pattern, value, scope, conditionals, body),
    }
}

pub fn evaluate_match_statement(
    declaration: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::MatchDeclaration { value, patterns } = declaration {
        let value = evaluate(*value, scope.clone())?;

        for (pattern, conditionals, body) in patterns {
            if let Some(result) =
                match_pattern(&pattern, &value, scope.clone(), &conditionals, &body)
            {
                match result {
                    Ok(x) => return Ok(x),
                    Err(InterpreterErr::ExpectedFunctions) => continue,
                    Err(e) => return Err(e),
                }
            }
        }
        Ok(RuntimeValue::Null)
    } else {
        Err(InterpreterErr::NotImplemented(declaration))
    }
}

pub fn handle_conditionals(
    scope: Rc<RefCell<Scope>>,
    conditionals: Vec<NodeType>,
) -> Result<bool, InterpreterErr> {
    let mut result = true;

    for condition in conditionals.into_iter() {
        if let RuntimeValue::Bool(value) = evaluate(condition, scope.clone())? {
            result = result && value;
        }
    }

    Ok(result)
}
