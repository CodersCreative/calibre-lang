use std::panic;

use crate::{
    ast::{NodeType, RefMutability},
    runtime::{
        interpreter::InterpreterErr,
        scope::{
             links::progress,  Environment, Type
        },
        values::{
            helper::{ObjectType, VarType}, RuntimeType, RuntimeValue
        },
    },
};

impl Environment {
    fn match_inner_pattern(
        &mut self,
        mut scope: u64,
        pattern: &NodeType,
        value: &RuntimeValue,
        mutability: &RefMutability,
        mut path: Vec<String>,
        conditionals: &[NodeType],
    ) -> Option<Result<u64, InterpreterErr>> {
        match pattern {
            NodeType::Identifier(x)
                if x.trim() == "_"
                    && self.handle_conditionals(&scope, conditionals.to_vec()).unwrap() =>
            {
                return Some(Ok(scope.clone()));
            }
            NodeType::MemberExpression { path: p } => {
                if let (NodeType::Identifier(main), _) = &p[0] {
                    if let Ok(_) = self.get_next_scope(scope, main) {
                        return self.match_inner_pattern(
                            scope,
                            &p[1].0,
                            value,
                            mutability,
                            path,
                            conditionals,
                        );
                    }

                    if let (NodeType::CallExpression(val, args), _) = &p[1] {
                        if let NodeType::Identifier(val) = *val.clone() {
                            return self.match_inner_pattern(
                                scope,
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
                if let RuntimeValue::Enum(obj_scope, iden, val, dat) = value.clone() {
                    let Type::Enum(enm) = self.get_object(&obj_scope, &iden).unwrap() else {
                        return None;
                    };
                    let Some(index) = enm.iter().position(|x| &x.0 == enm_value) else {
                        return None;
                    };

                    if index != val {
                        return None;
                    }

                    let Some(data) = data else {
                        if !self.handle_conditionals(&scope, conditionals.to_vec()).unwrap() {
                            return None;
                        }
                        return Some(Ok(scope.clone()));
                    };

                    let Some(dat) = dat else {
                        if !self.handle_conditionals(&scope, conditionals.to_vec()).unwrap() {
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
                                                    RuntimeValue::Link(obj_scope, path, (&v).into()),
                                                    mutability.clone(),
                                                )),
                                                _ => Some((k.to_string(), v, mutability.clone())),
                                            },
                                            Some(node) => {
                                                let mut value = value.clone();

                                                if let Some(Ok(s)) = self.match_inner_pattern(
                                                    scope,
                                                    node,
                                                    progress(&mut value, &k.to_string()).unwrap(),
                                                    mutability,
                                                    path.clone(),
                                                    conditionals,
                                                ) {
                                                    scope = s;
                                                };

                                                None
                                            }
                                            None => match &mutability {
                                                RefMutability::Ref | RefMutability::MutRef => Some((
                                                    k.to_string(),
                                                    RuntimeValue::Link(obj_scope, path, (&v).into()),
                                                    mutability.clone(),
                                                )),
                                                _ => Some((k.to_string(), v, mutability.clone())),
                                            },
                                        }
                                    })
                                    .collect();

                                new_scope = Some(self.get_new_scope_with_values(&scope, values).unwrap());
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
                                            if let Some(Ok(s)) = self.match_inner_pattern(
                                                scope,
                                                node,
                                                progress(&mut value, &i.to_string()).unwrap(),
                                                mutability,
                                                [path.clone(), vec![i.to_string()]].concat(),
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
                                    self.get_new_scope_with_values(
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
                                                        RuntimeValue::Link(obj_scope, path, (&v).into()),
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
                                    self.get_new_scope_with_values(
                                        &scope,
                                        map.into_iter()
                                            .filter(|x| list.contains(&x.0))
                                            .map(|(k, v)| match &mutability {
                                                RefMutability::MutRef | RefMutability::Ref => {
                                                    let path =
                                                        [path.clone(), vec![k.to_string()]].concat();
                                                    (
                                                        k.to_string(),
                                                        RuntimeValue::Link(obj_scope, path, (&v).into()),
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
                                match self.match_inner_pattern(
                                    scope,
                                    &arg_pat.0,
                                    inner,
                                    mutability,
                                    path,
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
                                if let Ok(pattern) = self.evaluate(&scope, pattern.0.clone()) {
                                    let RuntimeValue::Str(pattern) =
                                        pattern.into_type(self, &scope, &RuntimeType::Str).unwrap()
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

                                        let new_scope = self.get_new_scope_with_values(&scope, vars).ok()?;
                                        if self.handle_conditionals(&new_scope, conditionals.to_vec())
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
                                match self.match_inner_pattern(
                                    scope,
                                    &arg_pat.0,
                                    inner,
                                    mutability,
                                    path,
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
                                let new_scope = self.get_new_scope_with_values(
                                    &scope,
                                    vec![(name.clone(), value.clone(), RefMutability::MutValue)],
                                )
                                .ok()?;
                                if self.handle_conditionals(&new_scope, conditionals.to_vec()).ok()? {
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
                                match self.match_inner_pattern(
                                    scope,
                                    &arg_pat.0,
                                    inner,
                                    mutability,
                                    path,
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
                let new_scope = self.get_new_scope_with_values(
                    &scope,
                    match &mutability {
                        RefMutability::MutRef | RefMutability::Ref => {
                            vec![(
                                var_name.clone(),
                                RuntimeValue::Link(scope, path.clone(), value.into()),
                                mutability.clone(),
                            )]
                        }
                        _ => vec![(var_name.clone(), value.clone(), mutability.clone())],
                    },
                )
                .ok()?;
                if self.handle_conditionals(&new_scope, conditionals.to_vec()).ok()? {
                    Some(Ok(scope.clone()))
                } else {
                    None
                }
            }
            _ => {
                if let Ok(x) = self.evaluate(&scope, pattern.clone()) {
                    if (self.is_equal(&scope, &x, value) || self.is_value_in(&scope, value, &x))
                        && self.handle_conditionals(&scope, conditionals.to_vec()).ok()?
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
        &mut self,
        scope: &u64,
        pattern: &NodeType,
        value: &RuntimeValue,
        mutability: &RefMutability,
        path: Vec<String>,
        conditionals: &[NodeType],
        body: NodeType,
    ) -> Option<Result<RuntimeValue, InterpreterErr>> {
        
        match self.evaluate(scope, pattern.clone()) {
            Ok(x)
                if (self.is_equal(scope, &x, value) || self.is_value_in(scope, value, &x))
                    && self.handle_conditionals(scope, conditionals.to_vec()).unwrap() =>
            {
                return Some(self.evaluate(
                    scope,
                    body,
                ));
            }
            Ok(_) => None,
            Err(_) => {
                if let Some(Ok(scope)) = self.match_inner_pattern(
                    *scope,
                    pattern,
                    value,
                    mutability,
                    path,
                    conditionals,
                ) {
                    return Some(self.evaluate(
                        &scope,
                        body,
                    ));
                } else {
                    None
                }
            }
        }
    }

    pub fn evaluate_match_statement(
        &mut self,
        scope: &u64,
        declaration: NodeType,
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
                    let var = self.get_var(scope, &identifier)?;

                    match var.var_type {
                        VarType::Mutable => var.value.clone(),
                        _ => return Err(InterpreterErr::MutRefNonMut(var.value.clone())),
                    }
                }
                (RefMutability::Ref, NodeType::Identifier(identifier)) => {
                    self.get_var(scope, &identifier)?.value.clone()
                }
                (RefMutability::Ref | RefMutability::MutRef, _) => {
                    return Err(InterpreterErr::MutRefNonMut(self.evaluate(scope, *value)?.clone()));
                }
                _ => self.evaluate(scope, *value)?,
            };

            for (pattern, conditionals, body) in patterns {
                if let Some(result) = self.match_pattern(
                    scope,
                    &pattern,
                    &value,
                    &mutability,
                    path.clone(),
                    &conditionals,
                    *body,
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
        &mut self,
        scope: &u64,
        conditionals: Vec<NodeType>,
    ) -> Result<bool, InterpreterErr> {
        let mut result = true;

        for condition in conditionals.into_iter() {
            if let RuntimeValue::Bool(value) = self.evaluate(scope, condition)? {
                result = result && value;
            }
        }

        Ok(result)
    }
}

