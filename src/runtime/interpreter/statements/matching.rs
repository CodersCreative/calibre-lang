use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{NodeType, RefMutability},
    runtime::{
        interpreter::{
            InterpreterErr, evaluate,
            expressions::{
                call::evaluate_function,
                scope::{evaluate_scope, get_new_scope_with_values},
            },
        },
        scope::{Object, Scope, ScopeErr, objects::get_object},
        values::{
            RuntimeType, RuntimeValue,
            helper::{ObjectType, StopValue},
        },
    },
};

pub fn evaluate_match_statement(
    declaration: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::MatchDeclaration { value, patterns } = declaration {
        let value = evaluate(*value, scope.clone())?;

        for (pattern, conditionals, body) in patterns {
            match evaluate(pattern.clone(), scope.clone()) {
                Ok(x)
                    if x == value && handle_conditionals(scope.clone(), conditionals.clone())? =>
                {
                    return evaluate_scope(NodeType::ScopeDeclaration { body }, scope);
                }
                Err(e) => match pattern {
                    NodeType::Identifier(x) if x.trim() == "_" => {
                        return evaluate_scope(NodeType::ScopeDeclaration { body }, scope);
                    }
                    NodeType::CallExpression(x, y) => {
                        if let NodeType::Identifier(x) = *x {
                            let handle_val =
                                |body : Vec<NodeType>, x: RuntimeValue| -> Result<RuntimeValue, InterpreterErr> {
                                    if let Some((NodeType::Identifier(y), _)) = y.get(0) {
                                        let new_scope = get_new_scope_with_values(
                                            scope.clone(),
                                            vec![(y.clone(), x, RefMutability::MutValue)],
                                        )?;

                                        if handle_conditionals(
                                            new_scope.clone(),
                                            conditionals.clone(),
                                        )? {
                                            return evaluate_scope(
                                                NodeType::ScopeDeclaration { body },
                                                new_scope,
                                            );
                                        }
                                    } else if handle_conditionals(
                                        scope.clone(),
                                        conditionals.clone(),
                                    )? {
                                        return evaluate_scope(
                                            NodeType::ScopeDeclaration { body },
                                            scope.clone(),
                                        );
                                    }

                                    Err(InterpreterErr::ExpectedFunctions)
                                };

                            match x.trim() {
                                "None" => match value {
                                    RuntimeValue::Option(None, _)
                                        if handle_conditionals(
                                            scope.clone(),
                                            conditionals.clone(),
                                        )? =>
                                    {
                                        return evaluate_scope(
                                            NodeType::ScopeDeclaration { body },
                                            scope,
                                        );
                                    }
                                    _ => {}
                                },
                                "Some" => match &value {
                                    RuntimeValue::Option(Some(x), _) => {
                                        match handle_val(body, *x.clone()) {
                                            Ok(x) => return Ok(x),
                                            Err(InterpreterErr::ExpectedFunctions) => continue,
                                            Err(x) => return Err(x),
                                        }
                                    }
                                    _ => {}
                                },
                                "Ok" => match &value {
                                    RuntimeValue::Result(Ok(x), _) => {
                                        match handle_val(body, *x.clone()) {
                                            Ok(x) => return Ok(x),
                                            Err(InterpreterErr::ExpectedFunctions) => continue,
                                            Err(x) => return Err(x),
                                        }
                                    }
                                    _ => {}
                                },
                                "Err" => match &value {
                                    RuntimeValue::Result(Err(x), _) => {
                                        match handle_val(body, *x.clone()) {
                                            Ok(x) => return Ok(x),
                                            Err(InterpreterErr::ExpectedFunctions) => continue,
                                            Err(x) => return Err(x),
                                        }
                                    }
                                    _ => {}
                                },
                                _ => {}
                            }
                        }
                    }
                    NodeType::EnumExpression {
                        identifier,
                        data,
                        value: enm_value,
                    } => {
                        if let RuntimeValue::Enum(iden, val, dat) = value.clone() {
                            let Object::Enum(enm) = get_object(scope.clone(), &iden)? else {
                                continue;
                            };
                            let Some(index) = enm.iter().position(|x| x.0 == enm_value) else {
                                continue;
                            };

                            if index != val {
                                continue;
                            }

                            let Some(data) = data else {
                                if !handle_conditionals(scope.clone(), conditionals.clone())? {
                                    continue;
                                }
                                return evaluate_scope(NodeType::ScopeDeclaration { body }, scope);
                            };

                            let Some(dat) = dat else {
                                if !handle_conditionals(scope.clone(), conditionals.clone())? {
                                    continue;
                                }
                                return evaluate_scope(NodeType::ScopeDeclaration { body }, scope);
                            };

                            if identifier != iden {
                                continue;
                            };
                            let mut new_scope = None;
                            match data {
                                ObjectType::Map(map) => {
                                    if let ObjectType::Map(m) = dat {
                                        new_scope = Some(get_new_scope_with_values(
                                            scope.clone(),
                                            m.into_iter()
                                                .map(|(k, v)| {
                                                    println!("{:?} - {:?}", k, map);
                                                    let Some(NodeType::Identifier(k)) =
                                                        map.get(&k).unwrap()
                                                    else {
                                                        panic!()
                                                    };
                                                    (k.to_string(), v, RefMutability::MutValue)
                                                })
                                                .collect(),
                                        )?);
                                    }
                                }
                                ObjectType::Tuple(list) => {
                                    if let ObjectType::Tuple(lst) = dat {
                                        let list: Vec<String> = list
                                            .into_iter()
                                            .map(|x| {
                                                let NodeType::Identifier(y) = x.unwrap() else {
                                                    panic!()
                                                };
                                                y
                                            })
                                            .collect();
                                        new_scope = Some(get_new_scope_with_values(
                                            scope.clone(),
                                            lst.into_iter()
                                                .enumerate()
                                                .map(|(i, v)| {
                                                    (list[i].clone(), v, RefMutability::MutValue)
                                                })
                                                .collect(),
                                        )?);
                                    } else if let ObjectType::Map(map) = dat {
                                        let list: Vec<String> = list
                                            .into_iter()
                                            .map(|x| {
                                                let NodeType::Identifier(y) = x.unwrap() else {
                                                    panic!()
                                                };
                                                y
                                            })
                                            .collect();

                                        new_scope = Some(get_new_scope_with_values(
                                            scope.clone(),
                                            map.into_iter()
                                                .filter(|x| list.contains(&x.0))
                                                .map(|(k, v)| (k, v, RefMutability::MutValue))
                                                .collect(),
                                        )?);
                                    }
                                }
                            }
                            if let Some(new_scope) = new_scope {
                                if !handle_conditionals(new_scope.clone(), conditionals.clone())? {
                                    continue;
                                }
                                let mut result: RuntimeValue = RuntimeValue::Null;
                                for statement in &body {
                                    if let Some(StopValue::Return) = new_scope.borrow().stop {
                                        break;
                                    } else if let NodeType::Return { value } = statement {
                                        result = evaluate(*value.clone(), new_scope.clone())?;
                                        break;
                                    } else {
                                        result = evaluate(statement.clone(), new_scope.clone())?;
                                    }
                                }

                                return Ok(result);
                            }
                        }
                    }
                    _ => {}
                },
                _ => {}
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

    for condition in conditionals.iter() {
        if let RuntimeValue::Bool(value) = evaluate(condition.clone(), scope.clone())? {
            result = result && value;
        }
    }

    Ok(result)
}
