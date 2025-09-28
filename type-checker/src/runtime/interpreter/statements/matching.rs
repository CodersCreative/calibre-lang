use calibre_parser::ast::{NodeType, ObjectType, RefMutability, VarType};

use crate::runtime::{
    interpreter::InterpreterErr,
    scope::{Environment, Type},
    values::{RuntimeType},
};
use std::panic;

impl Environment {
    fn match_inner_pattern(
        &mut self,
        mut scope: u64,
        pattern: &NodeType,
        value: &RuntimeValue,
        mutability: &RefMutability,
        mut path: Vec<String>,
    ) -> Option<Result<u64, InterpreterErr>> {
        match pattern {
            NodeType::Identifier(x) if x.trim() == "_" => {
                return Some(Ok(scope.clone()));
            }
            NodeType::MemberExpression { path: p } => {
                if let (NodeType::Identifier(main), _) = &p[0] {
                    if let Ok(scope) = self.get_next_scope(scope, main) {
                        return self.match_inner_pattern(scope, &p[1].0, value, mutability, path);
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
                            );
                        }
                    }

                    if p.len() <= 2 {
                        if let Ok(x) = self.evaluate(&scope, pattern.clone()) {
                            if self.is_equal(&scope, &x, value)
                                || self.is_value_in(&scope, value, &x)
                                || x.is_type(self, &scope, &value.into())
                            {
                                return Some(Ok(scope));
                            }
                        } else if let Ok(Type::Enum(_)) = self.get_object_type(&scope, &main) {
                            if let NodeType::Identifier(val) = &p[1].0 {
                                return self.match_inner_pattern(
                                    scope,
                                    &NodeType::EnumExpression {
                                        identifier: main.clone(),
                                        value: val.to_string(),
                                        data: None,
                                    },
                                    value,
                                    mutability,
                                    path,
                                );
                            }
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
                if let Ok(RuntimeValue::Enum(obj_scope, iden, val, dat)) =
                    value.clone().unwrap_val(self, &scope)
                {
                    let Type::Enum(enm) = self.get_object_type(&scope, &iden).unwrap() else {
                        return None;
                    };

                    let Some(index) = enm.iter().position(|x| &x.0 == enm_value) else {
                        return None;
                    };

                    if index != val || identifier != &iden {
                        return None;
                    }

                    let Some(data) = data else {
                        return Some(Ok(scope.clone()));
                    };

                    let Some(dat) = dat else {
                        return Some(Ok(scope.clone()));
                    };

                    let mut new_scope = None;

                    match data {
                        ObjectType::Map(map) => {
                            if let ObjectType::Map(m) = dat {
                                let values: Vec<(String, RuntimeValue, RefMutability)> = m
                                    .into_iter()
                                    .filter(|(k, _)| map.contains_key(k))
                                    .filter_map(|(k, v)| {
                                        let path = [path.clone(), vec![k.clone()]].concat();
                                        match map.get(&k).unwrap() {
                                            Some(NodeType::Identifier(k)) => match &mutability {
                                                RefMutability::Ref | RefMutability::MutRef => {
                                                    Some((
                                                        k.to_string(),
                                                        RuntimeValue::Link(
                                                            obj_scope,
                                                            path,
                                                            (&v).into(),
                                                        ),
                                                        mutability.clone(),
                                                    ))
                                                }
                                                _ => Some((
                                                    k.to_string(),
                                                    v.clone(),
                                                    mutability.clone(),
                                                )),
                                            },
                                            Some(node) => {
                                                if let Some(Ok(s)) = self.match_inner_pattern(
                                                    scope,
                                                    node,
                                                    progress(&value, &k.to_string()).unwrap(),
                                                    mutability,
                                                    path.clone(),
                                                ) {
                                                    scope = s;
                                                    None
                                                } else {
                                                    Some((
                                                        String::from("__failed__"),
                                                        RuntimeValue::Null,
                                                        RefMutability::Value,
                                                    ))
                                                }
                                            }
                                            None => match &mutability {
                                                RefMutability::Ref | RefMutability::MutRef => {
                                                    Some((
                                                        k.to_string(),
                                                        RuntimeValue::Link(
                                                            obj_scope,
                                                            path,
                                                            (&v).into(),
                                                        ),
                                                        mutability.clone(),
                                                    ))
                                                }
                                                _ => Some((
                                                    k.to_string(),
                                                    v.clone(),
                                                    mutability.clone(),
                                                )),
                                            },
                                        }
                                    })
                                    .collect();

                                if values.iter().filter(|x| x.1 == RuntimeValue::Null).count() > 0 {
                                    return None;
                                }

                                new_scope =
                                    Some(self.get_new_scope_with_values(&scope, values).unwrap());
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
                                            let value = progress(&value, &i.to_string()).unwrap();
                                            let result = self.match_inner_pattern(
                                                scope,
                                                node,
                                                value,
                                                mutability,
                                                [path.clone(), vec![i.to_string()]].concat(),
                                            );
                                            if let Some(Ok(s)) = result {
                                                scope = s;
                                                None
                                            } else {
                                                Some(String::from("__failed__"))
                                            }
                                        }
                                        _ => None,
                                    })
                                    .collect();

                                if list.contains(&Some(String::from("__failed__"))) {
                                    return None;
                                }

                                new_scope = Some(
                                    self.get_new_scope_with_values(
                                        &scope,
                                        lst.into_iter()
                                            .enumerate()
                                            .filter(|(i, _)| i < &list.len() && list[*i].is_some())
                                            .map(|(i, v)| match &mutability {
                                                RefMutability::Ref | RefMutability::MutRef => {
                                                    let path = [path.clone(), vec![i.to_string()]]
                                                        .concat();
                                                    (
                                                        list[i].clone().unwrap(),
                                                        RuntimeValue::Link(
                                                            obj_scope,
                                                            path,
                                                            (&v).into(),
                                                        ),
                                                        mutability.clone(),
                                                    )
                                                }
                                                _ => (
                                                    list[i].clone().unwrap(),
                                                    v.clone(),
                                                    mutability.clone(),
                                                ),
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
                                                    let path = [path.clone(), vec![k.to_string()]]
                                                        .concat();
                                                    (
                                                        k.to_string(),
                                                        RuntimeValue::Link(
                                                            obj_scope,
                                                            path,
                                                            (&v).into(),
                                                        ),
                                                        mutability.clone(),
                                                    )
                                                }
                                                _ => (k.to_string(), v.clone(), mutability.clone()),
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
                                match self
                                    .match_inner_pattern(scope, &arg_pat.0, inner, mutability, path)
                                {
                                    Some(Ok(x)) => Some(Ok(x)),
                                    Some(Err(InterpreterErr::ExpectedFunctions)) => None,
                                    Some(Err(e)) => Some(Err(e)),
                                    None => None,
                                }
                            } else {
                                None
                            }
                        }
                        (part, RuntimeValue::Str(value))
                            if part == "Prefix" || part == "Suffix" =>
                        {
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
                                        let vars =
                                            if let Some((NodeType::Identifier(name), _)) = name {
                                                vec![(
                                                    name.clone(),
                                                    RuntimeValue::Str(value.to_string()),
                                                    match mutability {
                                                        RefMutability::MutRef
                                                        | RefMutability::MutValue => {
                                                            RefMutability::MutValue
                                                        }
                                                        _ => RefMutability::Value,
                                                    },
                                                )]
                                            } else {
                                                Vec::new()
                                            };

                                        let new_scope =
                                            self.get_new_scope_with_values(&scope, vars).ok()?;
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
                        }
                        ("None", RuntimeValue::Option(None, _)) => Some(Ok(scope.clone())),
                        ("Ok", RuntimeValue::Result(Ok(inner), _)) => {
                            path.push("Ok".to_string());
                            if let Some(arg_pat) = args.get(0) {
                                match self
                                    .match_inner_pattern(scope, &arg_pat.0, inner, mutability, path)
                                {
                                    Some(Ok(x)) => Some(Ok(x)),
                                    Some(Err(InterpreterErr::ExpectedFunctions)) => None,
                                    Some(Err(e)) => Some(Err(e)),
                                    None => None,
                                }
                            } else {
                                None
                            }
                        }
                        ("Let", _) => {
                            if let Some((NodeType::Identifier(name), _)) = args.get(0) {
                                let new_scope = self
                                    .get_new_scope_with_values(
                                        &scope,
                                        vec![(
                                            name.clone(),
                                            value.clone(),
                                            RefMutability::MutValue,
                                        )],
                                    )
                                    .ok()?;
                                Some(Ok(new_scope.clone()))
                            } else {
                                None
                            }
                        }
                        ("Err", RuntimeValue::Result(Err(inner), _)) => {
                            path.push("Err".to_string());
                            if let Some(arg_pat) = args.get(0) {
                                match self
                                    .match_inner_pattern(scope, &arg_pat.0, inner, mutability, path)
                                {
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
                let new_scope = self
                    .get_new_scope_with_values(
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
                Some(Ok(new_scope.clone()))
            }
            _ => {
                if let Ok(x) = self.evaluate(&scope, pattern.clone()) {
                    if self.is_equal(&scope, &x, value)
                        || self.is_value_in(&scope, value, &x)
                        || x.is_type(self, &scope, &value.into())
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
        match match self.evaluate(scope, pattern.clone()) {
            Ok(x) => match x.unwrap_val(self, scope){
                Ok(x) => Ok(x),
                Err(e) => Err(e.into())
            },
            Err(e) => Err(e)
        } {
            Ok(x)
                if (self.is_equal(scope, &x, value)
                    || self.is_value_in(scope, value, &x)
                    || x.is_type(self, &scope, &value.into()))
                    && self
                        .handle_conditionals(scope, conditionals.to_vec())
                        .unwrap() =>
            {
                return Some(self.evaluate(scope, body));
            }
            Ok(_) => None,
            Err(_) => {
                if let Some(Ok(s)) =
                    self.match_inner_pattern(*scope, pattern, value, mutability, path)
                {
                    let mut res = if self.handle_conditionals(&s, conditionals.to_vec()).unwrap() {
                        Some(self.evaluate(&s, body))
                    } else {
                        None
                    };

                    if scope != &s {
                        self.remove_scope(&s);
                        if let Some(Ok(r)) = res {
                            res = Some(r.unwrap_links_val(self, &s, Some(s)).map_err(|e| e.into()))
                        }
                    }

                    res
                } else {
                    None
                }
            }
        }
    }

    pub fn evaluate_match_function(
        &mut self,
        scope: &u64,
        mutability: &RefMutability,
        value: NodeType,
        patterns: Vec<(NodeType, Vec<NodeType>, Box<NodeType>)>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        let path = match &value {
            NodeType::Identifier(x) => vec![x.clone()],
            _ => Vec::new(),
        };

        let value = match (&mutability, value.clone()) {
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
                return Err(InterpreterErr::MutRefNonMut(
                    self.evaluate(scope, value)?.clone(),
                ));
            }
            _ => self.evaluate(scope, value)?,
        };

        let value = value.unwrap_links_val(self, scope, None)?;

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
