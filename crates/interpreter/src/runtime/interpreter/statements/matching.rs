use calibre_common::environment::{Location, Type};
use calibre_parser::ast::{Node, NodeType, ObjectType, RefMutability, VarType};

use crate::runtime::{
    interpreter::InterpreterErr,
    scope::InterpreterEnvironment,
    values::{RuntimeType, RuntimeValue},
};
use std::panic;

impl InterpreterEnvironment {
    fn match_inner_pattern(
        &mut self,
        mut scope: u64,
        pattern: &Node,
        value: &RuntimeValue,
        mut path: Vec<String>,
    ) -> Option<Result<u64, InterpreterErr>> {
        match pattern.node_type.clone() {
            NodeType::Identifier(x) if x.trim() == "_" => {
                return Some(Ok(scope.clone()));
            }
            NodeType::MemberExpression { path: p } => {
                if let (NodeType::Identifier(main), _) = (&p[0].0.node_type, p[0].1) {
                    if let Ok(scope) = self.get_next_scope(scope, main) {
                        return self.match_inner_pattern(scope, &p[1].0, value, path);
                    }

                    if let (NodeType::CallExpression(val, args), _) = (&p[1].0.node_type, p[1].1) {
                        if let NodeType::Identifier(val) = val.node_type.clone() {
                            return self.match_inner_pattern(
                                scope,
                                &Node::new(
                                    NodeType::EnumExpression {
                                        identifier: main.clone(),
                                        value: val,
                                        data: Some(ObjectType::Tuple(
                                            args.into_iter().map(|x| Some(x.0.clone())).collect(),
                                        )),
                                    },
                                    p[0].0.span,
                                ),
                                value,
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
                            if let NodeType::Identifier(val) = &p[1].0.node_type {
                                return self.match_inner_pattern(
                                    scope,
                                    &Node::new(
                                        NodeType::EnumExpression {
                                            identifier: main.clone(),
                                            value: val.to_string(),
                                            data: None,
                                        },
                                        p[0].0.span,
                                    ),
                                    value,
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
                if let RuntimeValue::Enum(obj_scope, iden, val, dat) = value.clone() {
                    let Type::Enum(enm) = self.get_object_type(&scope, &iden).unwrap() else {
                        return None;
                    };

                    let Some(index) = enm.iter().position(|x| x.0 == enm_value) else {
                        return None;
                    };

                    if index != val || identifier != iden {
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
                                let values: Vec<(String, RuntimeValue, Option<Location>)> = m
                                    .into_iter()
                                    .filter(|(k, _)| map.contains_key(k))
                                    .filter_map(|(k, v)| {
                                        let path = [path.clone(), vec![k.clone()]].concat();
                                        match map.get(&k).unwrap() {
                                            Some(node) => match &node.node_type {
                                                NodeType::Identifier(k) => Some((
                                                    k.to_string(),
                                                    v.clone(),
                                                    self.get_location(&scope, node.span),
                                                )),
                                                _ => {
                                                    if let Some(Ok(s)) = self.match_inner_pattern(
                                                        scope,
                                                        node,
                                                        &self
                                                            .progress_var(&value, &k.to_string())
                                                            .unwrap(),
                                                        path.clone(),
                                                    ) {
                                                        scope = s;
                                                        None
                                                    } else {
                                                        Some((
                                                            String::from("__failed__"),
                                                            RuntimeValue::Null,
                                                            self.get_location(&scope, node.span),
                                                        ))
                                                    }
                                                }
                                            },
                                            None => Some((
                                                k.to_string(),
                                                v.clone(),
                                                self.get_location(&scope, pattern.span),
                                            )),
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
                                        Some(node) => match node.node_type {
                                            NodeType::Identifier(y) => Some(y.clone()),
                                            _ => {
                                                let value = self
                                                    .progress_var(&value, &i.to_string())
                                                    .unwrap();
                                                let result = self.match_inner_pattern(
                                                    scope,
                                                    &node,
                                                    &value,
                                                    [path.clone(), vec![i.to_string()]].concat(),
                                                );
                                                if let Some(Ok(s)) = result {
                                                    scope = s;
                                                    None
                                                } else {
                                                    Some(String::from("__failed__"))
                                                }
                                            }
                                        },

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
                                            .map(|(i, v)| {
                                                (
                                                    list[i].clone().unwrap(),
                                                    v.clone(),
                                                    self.get_location(&scope, pattern.span),
                                                )
                                            })
                                            .collect(),
                                    )
                                    .unwrap(),
                                );
                            } else if let ObjectType::Map(map) = dat {
                                let list: Vec<String> = list
                                    .into_iter()
                                    .map(|x| {
                                        let NodeType::Identifier(y) = x.clone().unwrap().node_type
                                        else {
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
                                            .map(|(k, v)| {
                                                (
                                                    k.to_string(),
                                                    v.clone(),
                                                    self.get_location(&scope, pattern.span),
                                                )
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
                if let NodeType::Identifier(variant) = callee.node_type {
                    match (variant.as_str(), value) {
                        ("Some", RuntimeValue::Option(Some(inner), _)) => {
                            if let Some(arg_pat) = args.get(0) {
                                path.push("Some".to_string());
                                match self.match_inner_pattern(scope, &arg_pat.0, inner, path) {
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
                                    let RuntimeValue::Str(pattern) = pattern else {
                                        panic!()
                                    };

                                    if let Some(value) = if part == "Prefix" {
                                        value.strip_prefix(&pattern)
                                    } else {
                                        value.strip_suffix(&pattern)
                                    } {
                                        let vars = if let Some((name, _)) = name {
                                            if let NodeType::Identifier(name) = &name.node_type {
                                                vec![(
                                                    name.clone(),
                                                    RuntimeValue::Str(value.to_string()),
                                                    self.current_location.clone(),
                                                )]
                                            } else {
                                                Vec::new()
                                            }
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
                                match self.match_inner_pattern(scope, &arg_pat.0, inner, path) {
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
                            if let Some((name, _)) = args.get(0) {
                                if let NodeType::Identifier(name) = &name.node_type {
                                    let new_scope = self
                                        .get_new_scope_with_values(
                                            &scope,
                                            vec![(
                                                name.clone(),
                                                value.clone(),
                                                self.get_location(&scope, pattern.span),
                                            )],
                                        )
                                        .ok()?;
                                    Some(Ok(new_scope.clone()))
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
                                match self.match_inner_pattern(scope, &arg_pat.0, inner, path) {
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
                        vec![(
                            var_name.clone(),
                            value.clone(),
                            self.get_location(&scope, pattern.span),
                        )],
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
        pattern: &Node,
        value: &RuntimeValue,
        path: Vec<String>,
        conditionals: &[Node],
        body: Node,
    ) -> Option<Result<RuntimeValue, InterpreterErr>> {
        match self.evaluate(scope, pattern.clone()) {
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
                if let Some(Ok(s)) = self.match_inner_pattern(*scope, pattern, value, path) {
                    let mut res = if self.handle_conditionals(&s, conditionals.to_vec()).unwrap() {
                        Some(self.evaluate(&s, body))
                    } else {
                        None
                    };

                    if scope != &s {
                        self.remove_scope(&s);
                        if let Some(Ok(r)) = res {
                            res = Some(Ok(r))
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
        value: Node,
        patterns: Vec<(Node, Vec<Node>, Box<Node>)>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        let path = match &value.node_type {
            NodeType::Identifier(x) => vec![x.clone()],
            _ => Vec::new(),
        };

        let value = self.evaluate(scope, value)?;

        for (pattern, conditionals, body) in patterns {
            if let Some(result) =
                self.match_pattern(scope, &pattern, &value, path.clone(), &conditionals, *body)
            {
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
        conditionals: Vec<Node>,
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
