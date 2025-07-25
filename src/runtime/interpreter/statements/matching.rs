use std::{cell::RefCell, panic, rc::Rc};

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
            children::get_next_scope,
            links::progress,
            objects::get_object,
            variables::{get_global_scope, get_stop, get_var},
        },
        values::{
            RuntimeType, RuntimeValue,
            helper::{ObjectType, StopValue, VarType},
        },
    },
};

fn match_inner_pattern(
    pattern: &NodeType,
    value: &RuntimeValue,
    mutability: &RefMutability,
    mut path: Vec<String>,
    mut scope: Rc<RefCell<Scope>>,
    conditionals: &[NodeType],
) -> Option<Result<Rc<RefCell<Scope>>, InterpreterErr>> {
    match pattern {
        NodeType::Identifier(x)
            if x.trim() == "_"
                && handle_conditionals(scope.clone(), conditionals.to_vec()).unwrap() =>
        {
            return Some(Ok(scope.clone()));
        }
        NodeType::MemberExpression { path: p } => {
            if let (NodeType::Identifier(main), _) = &p[0] {
                if let Ok(_) = get_next_scope(&scope, main) {
                    return match_inner_pattern(
                        &p[1].0,
                        value,
                        mutability,
                        path,
                        scope,
                        conditionals,
                    );
                }

                if let (NodeType::CallExpression(val, args), _) = &p[1] {
                    if let NodeType::Identifier(val) = *val.clone() {
                        return match_inner_pattern(
                            &NodeType::EnumExpression {
                                identifier: main.clone(),
                                value: val,
                                data: Some(ObjectType::Tuple(
                                    args.into_iter().map(|x| Some(x.0.clone())).collect(),
                                )),
                            },
                            value,
                            mutability,
                            path,
                            scope,
                            conditionals,
                        );
                    }
                }
            }

            None
        }
        NodeType::EnumExpression {
            identifier,
            data,
            value: enm_value,
        } => {
            if let RuntimeValue::Enum(iden, val, dat) = value.clone() {
                let Object::Enum(enm) = get_object(&scope, &iden).unwrap() else {
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
                    return Some(Ok(scope.clone()));
                };

                let Some(dat) = dat else {
                    if !handle_conditionals(scope.clone(), conditionals.to_vec()).unwrap() {
                        return None;
                    }
                    return Some(Ok(scope.clone()));
                };

                if identifier != &iden {
                    return None;
                };
                let mut new_scope = None;

                match data {
                    ObjectType::Map(map) => {
                        if let ObjectType::Map(m) = dat {
                            let values = m
                                .into_iter()
                                .filter(|(k, _)| map.contains_key(k))
                                .filter_map(|(k, v)| {
                                    let path = [path.clone(), vec![k.clone()]].concat();
                                    match map.get(&k).unwrap() {
                                        Some(NodeType::Identifier(k)) => match &mutability {
                                            RefMutability::Ref | RefMutability::MutRef => Some((
                                                k.to_string(),
                                                RuntimeValue::Link(path, (&v).into()),
                                                mutability.clone(),
                                            )),
                                            _ => Some((k.to_string(), v, mutability.clone())),
                                        },
                                        Some(node) => {
                                            let mut value = value.clone();

                                            if let Some(Ok(s)) = match_inner_pattern(
                                                node,
                                                progress(&mut value, &k.to_string()).unwrap(),
                                                mutability,
                                                path.clone(),
                                                scope.clone(),
                                                conditionals,
                                            ) {
                                                scope = s;
                                            };

                                            None
                                        }
                                        None => match &mutability {
                                            RefMutability::Ref | RefMutability::MutRef => Some((
                                                k.to_string(),
                                                RuntimeValue::Link(path, (&v).into()),
                                                mutability.clone(),
                                            )),
                                            _ => Some((k.to_string(), v, mutability.clone())),
                                        },
                                    }
                                })
                                .collect();

                            new_scope = Some(get_new_scope_with_values(&scope, values).unwrap());
                        }
                    }
                    ObjectType::Tuple(list) => {
                        if let ObjectType::Tuple(lst) = dat {
                            let list: Vec<Option<String>> = list
                                .into_iter()
                                .enumerate()
                                .map(|(i, x)| match x {
                                    Some(NodeType::Identifier(y)) => Some(y.clone()),
                                    Some(node) => {
                                        let mut value = value.clone();
                                        if let Some(Ok(s)) = match_inner_pattern(
                                            node,
                                            progress(&mut value, &i.to_string()).unwrap(),
                                            mutability,
                                            [path.clone(), vec![i.to_string()]].concat(),
                                            scope.clone(),
                                            conditionals,
                                        ) {
                                            scope = s;
                                        };
                                        None
                                    }
                                    _ => None,
                                })
                                .collect();

                            new_scope = Some(
                                get_new_scope_with_values(
                                    &scope,
                                    lst.into_iter()
                                        .enumerate()
                                        .filter(|(i, _)| i < &list.len() && list[*i].is_some())
                                        .map(|(i, v)| match &mutability {
                                            RefMutability::Ref | RefMutability::MutRef => {
                                                let path =
                                                    [path.clone(), vec![i.to_string()]].concat();
                                                (
                                                    list[i].clone().unwrap(),
                                                    RuntimeValue::Link(path, (&v).into()),
                                                    mutability.clone(),
                                                )
                                            }
                                            _ => (list[i].clone().unwrap(), v, mutability.clone()),
                                        })
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
                                    &scope,
                                    map.into_iter()
                                        .filter(|x| list.contains(&x.0))
                                        .map(|(k, v)| match &mutability {
                                            RefMutability::MutRef | RefMutability::Ref => {
                                                let path =
                                                    [path.clone(), vec![k.to_string()]].concat();
                                                (
                                                    k.to_string(),
                                                    RuntimeValue::Link(path, (&v).into()),
                                                    mutability.clone(),
                                                )
                                            }
                                            _ => (k.to_string(), v, mutability.clone()),
                                        })
                                        .collect(),
                                )
                                .unwrap(),
                            );
                        }
                    }
                }

                if let Some(new_scope) = new_scope {
                    Some(Ok(new_scope))
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
                            path.push("Some".to_string());
                            match match_inner_pattern(
                                &arg_pat.0,
                                inner,
                                mutability,
                                path,
                                scope,
                                conditionals,
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
                    (part, RuntimeValue::Str(value)) if part == "Prefix" || part == "Suffix" => {
                        if let (Some(pattern), name) = (args.get(0), args.get(1)) {
                            if let Ok(pattern) = evaluate(pattern.0.clone(), &scope) {
                                let RuntimeValue::Str(pattern) =
                                    pattern.into_type(&scope, &RuntimeType::Str).unwrap()
                                else {
                                    panic!()
                                };

                                if let Some(value) = if part == "Prefix" {
                                    value.strip_prefix(&pattern)
                                } else {
                                    value.strip_suffix(&pattern)
                                } {
                                    let vars = if let Some((NodeType::Identifier(name), _)) = name {
                                        vec![(
                                            name.clone(),
                                            RuntimeValue::Str(value.to_string()),
                                            match mutability {
                                                RefMutability::MutRef | RefMutability::MutValue => {
                                                    RefMutability::MutValue
                                                }
                                                _ => RefMutability::Value,
                                            },
                                        )]
                                    } else {
                                        Vec::new()
                                    };

                                    let new_scope = get_new_scope_with_values(&scope, vars).ok()?;
                                    if handle_conditionals(new_scope.clone(), conditionals.to_vec())
                                        .ok()?
                                    {
                                        Some(Ok(new_scope))
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    }
                    ("None", RuntimeValue::Option(None, _)) => Some(Ok(scope.clone())),
                    ("Ok", RuntimeValue::Result(Ok(inner), _)) => {
                        path.push("Ok".to_string());
                        if let Some(arg_pat) = args.get(0) {
                            match match_inner_pattern(
                                &arg_pat.0,
                                inner,
                                mutability,
                                path,
                                scope,
                                conditionals,
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
                    ("Let", data) => {
                        if let Some((NodeType::Identifier(name), _)) = args.get(0) {
                            let new_scope = get_new_scope_with_values(
                                &scope,
                                vec![(name.clone(), value.clone(), RefMutability::MutValue)],
                            )
                            .ok()?;
                            if handle_conditionals(new_scope.clone(), conditionals.to_vec()).ok()? {
                                Some(Ok(scope.clone()))
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    }
                    ("Err", RuntimeValue::Result(Err(inner), _)) => {
                        path.push("Err".to_string());
                        if let Some(arg_pat) = args.get(0) {
                            match match_inner_pattern(
                                &arg_pat.0,
                                inner,
                                mutability,
                                path,
                                scope,
                                conditionals,
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
                &scope,
                match &mutability {
                    RefMutability::MutRef | RefMutability::Ref => {
                        vec![(
                            var_name.clone(),
                            RuntimeValue::Link(path.clone(), value.into()),
                            mutability.clone(),
                        )]
                    }
                    _ => vec![(var_name.clone(), value.clone(), mutability.clone())],
                },
            )
            .ok()?;
            if handle_conditionals(new_scope.clone(), conditionals.to_vec()).ok()? {
                Some(Ok(scope.clone()))
            } else {
                None
            }
        }
        _ => {
            if let Ok(x) = evaluate(pattern.clone(), &scope) {
                if (is_equal(&x, value, &scope) || is_value_in(value, &x, &scope))
                    && handle_conditionals(scope.clone(), conditionals.to_vec()).ok()?
                {
                    Some(Ok(scope.clone()))
                } else {
                    None
                }
            } else {
                None
            }
        }
    }
}

pub fn match_pattern(
    pattern: &NodeType,
    value: &RuntimeValue,
    mutability: &RefMutability,
    path: Vec<String>,
    scope: &Rc<RefCell<Scope>>,
    conditionals: &[NodeType],
    body: &[NodeType],
) -> Option<Result<RuntimeValue, InterpreterErr>> {
    use crate::ast::NodeType;
    match evaluate(pattern.clone(), scope) {
        Ok(x)
            if (is_equal(&x, value, scope) || is_value_in(value, &x, scope))
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
        Err(_) => {
            if let Some(Ok(scope)) = match_inner_pattern(
                pattern,
                value,
                mutability,
                path,
                scope.clone(),
                conditionals,
            ) {
                return Some(evaluate_scope(
                    NodeType::ScopeDeclaration {
                        body: body.to_vec(),
                    },
                    &scope,
                ));
            } else {
                None
            }
        }
    }
}

pub fn evaluate_match_statement(
    declaration: NodeType,
    scope: &Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::MatchDeclaration {
        value,
        patterns,
        mutability,
    } = declaration
    {
        let path = match &*value {
            NodeType::Identifier(x) => vec![x.clone()],
            _ => Vec::new(),
        };

        let value = match (&mutability, *value.clone()) {
            (RefMutability::MutRef, NodeType::Identifier(identifier)) => {
                let var = get_var(scope, &identifier)?;

                match var.1 {
                    VarType::Mutable(_) => var.0,
                    _ => return Err(InterpreterErr::MutRefNonMut(var.0)),
                }
            }
            (RefMutability::Ref, NodeType::Identifier(identifier)) => {
                get_var(scope, &identifier)?.0
            }
            (RefMutability::Ref | RefMutability::MutRef, _) => {
                return Err(InterpreterErr::MutRefNonMut(evaluate(*value, scope)?));
            }
            _ => evaluate(*value, scope)?,
        };

        for (pattern, conditionals, body) in patterns {
            if let Some(result) = match_pattern(
                &pattern,
                &value,
                &mutability,
                path.clone(),
                scope,
                &conditionals,
                &body,
            ) {
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
        if let RuntimeValue::Bool(value) = evaluate(condition, &scope)? {
            result = result && value;
        }
    }

    Ok(result)
}
