use calibre_parser::ast::{Node, NodeType, ObjectType};

use crate::runtime::{interpreter::InterpreterErr, scope::CheckerEnvironment, values::RuntimeType};

pub enum MembrExprPathRes {
    Value(RuntimeType),
    Path(Vec<String>),
}

impl CheckerEnvironment {
    pub fn get_member_expression_path(
        &mut self,
        scope: &u64,
        og_path: Vec<(Node, bool)>,
    ) -> Result<MembrExprPathRes, InterpreterErr> {
        let mut path = Vec::new();
        if let (NodeType::Identifier(x), false) = (&og_path[0].0.node_type, &og_path[0].1) {
            if let Ok(s) = self.get_next_scope(*scope, &x) {
                if let Ok(x) = self.evaluate(&s, og_path[1].0.clone()) {
                    return Ok(MembrExprPathRes::Value(x));
                }
                match self.get_member_expression_path(&s, og_path[1..].to_vec()) {
                    Ok(x) => return Ok(x),
                    _ => {}
                }
            }
        }

        for (node, computed) in og_path.into_iter() {
            match &node.node_type {
                NodeType::MemberExpression { path: p } => {
                    match self.get_member_expression_path(scope, p.to_vec())? {
                        MembrExprPathRes::Value(x) => return Ok(MembrExprPathRes::Value(x)),
                        MembrExprPathRes::Path(mut p) => path.append(&mut p),
                    }
                    continue;
                }
                _ if computed => {}
                NodeType::Identifier(x) => {
                    path.push(x.to_string());
                    continue;
                }
                NodeType::FloatLiteral(x) => {
                    path.push(x.to_string());
                    continue;
                }
                NodeType::IntLiteral(x) => {
                    path.push(x.to_string());
                    continue;
                }
                _ => {}
            }

            match self.evaluate(&scope, node.clone()) {
                Ok(value) => match value {
                    RuntimeType::Int
                    | RuntimeType::Float
                    | RuntimeType::Str
                    | RuntimeType::Char => {
                        return Ok(MembrExprPathRes::Value(RuntimeType::Dynamic));
                    }
                    x => return Ok(MembrExprPathRes::Value(x.clone())),
                },
                Err(e) if path.len() == 1 => match node.node_type {
                    NodeType::Identifier(value) => {
                        return Ok(MembrExprPathRes::Value(self.evaluate(
                            scope,
                            Node::new(
                                NodeType::EnumExpression {
                                    identifier: path.remove(0),
                                    value,
                                    data: None,
                                },
                                node.span,
                            ),
                        )?));
                    }
                    NodeType::CallExpression(value_node, mut args) => match value_node.node_type {
                        NodeType::Identifier(value) => {
                            return Ok(MembrExprPathRes::Value(
                                match self.evaluate(
                                    scope,
                                    Node::new(
                                        NodeType::EnumExpression {
                                            identifier: path[0].clone(),
                                            value: value.to_string(),
                                            data: Some(ObjectType::Tuple(
                                                args.clone()
                                                    .into_iter()
                                                    .map(|x| Some(x.0.clone()))
                                                    .collect(),
                                            )),
                                        },
                                        value_node.span,
                                    ),
                                ) {
                                    Ok(x) => x,
                                    Err(e) => {
                                        if let Ok(x) = self.get_function(scope, &path[0], &value) {
                                            self.evaluate_function(scope, x.0.clone(), args)?
                                        } else {
                                            let obj = match match self.get_var(scope, &path[0]) {
                                                Ok(x) => x.value.clone(),
                                                _ => return Err(e),
                                            } {
                                                RuntimeType::Struct(_, p, _) => p.unwrap().clone(),
                                                RuntimeType::Enum(_, p, _) => p.clone(),
                                                _ => return Err(e),
                                            };

                                            if let Ok(x) = self.get_function(scope, &obj, &value) {
                                                if x.2 {
                                                    args.insert(
                                                        0,
                                                        (
                                                            Node::new(
                                                                NodeType::Identifier(
                                                                    path[0].clone(),
                                                                ),
                                                                value_node.span,
                                                            ),
                                                            None,
                                                        ),
                                                    );
                                                }

                                                self.evaluate_function(scope, x.0.clone(), args)?
                                            } else {
                                                return Err(e);
                                            }
                                        }
                                    }
                                },
                            ));
                        }
                        _ => return Err(e),
                    },
                    _ => return Err(e),
                },
                Err(e) => return Err(e),
            }
        }

        Ok(MembrExprPathRes::Path(path))
    }

    pub fn assign_member_expression(
        &mut self,
        scope: &u64,
        member: Node,
        value: RuntimeType,
    ) -> Result<RuntimeType, InterpreterErr> {
        let typ = self.evaluate_member_expression(scope, member)?;
        if value.is_type(&typ) {
            Ok(RuntimeType::Ref(Box::new(value)))
        } else {
            panic!()
        }
    }

    pub fn evaluate_member_expression(
        &mut self,
        scope: &u64,
        exp: Node,
    ) -> Result<RuntimeType, InterpreterErr> {
        match exp.node_type {
            NodeType::MemberExpression { path: og_path } => {
                let mut path = match self.get_member_expression_path(scope, og_path)? {
                    MembrExprPathRes::Path(x) => x,
                    MembrExprPathRes::Value(x) => return Ok(x),
                };

                match self.get_member_ref(scope, &path) {
                    Ok(RuntimeType::Ref(t)) => Ok(*t),
                    Ok(x) => Ok(x),
                    Err(_) if path.len() == 2 => {
                        return self.evaluate(
                            scope,
                            Node::new(
                                NodeType::EnumExpression {
                                    identifier: path.remove(0),
                                    value: path.remove(0),
                                    data: None,
                                },
                                exp.span,
                            ),
                        );
                    }
                    Err(e) => Err(e),
                }
            }
            _ => Err(InterpreterErr::NotImplemented(exp.node_type)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::scope::Variable;
    use crate::runtime::values::RuntimeValue;
    use calibre_parser::ast::{NodeType, VarType};
    use std::path::PathBuf;
    use std::str::FromStr;

    fn get_new_env() -> (Environment, u64) {
        let mut env = Environment::new();
        let scope = env.new_scope_with_stdlib(None, PathBuf::from_str("./main.cl").unwrap(), None);
        (env, scope)
    }

    #[test]
    fn test_assign_member_expression_struct_field() {
        let (mut env, scope) = get_new_env();
        let mut map = std::collections::HashMap::new();
        map.insert("field".to_string(), RuntimeValue::Int(1));
        let struct_val = RuntimeValue::Struct(scope, None, ObjectType::Map(map.clone()));
        env.push_var(
            &scope,
            "obj".to_string(),
            Variable {
                value: struct_val.clone(),
                var_type: VarType::Mutable,
            },
        )
        .unwrap();

        let member = NodeType::MemberExpression {
            path: vec![
                (NodeType::Identifier("obj".to_string()), false),
                (NodeType::Identifier("field".to_string()), false),
            ],
        };
        env.assign_member_expression(&scope, member, RuntimeValue::Int(42))
            .unwrap();

        let updated = env.get_var(&scope, "obj").unwrap().value.clone();
        if let RuntimeValue::Struct(_, _, ObjectType::Map(m)) = updated {
            assert_eq!(m.get("field"), Some(&RuntimeValue::Int(42)));
        } else {
            panic!("Expected struct value");
        }
    }

    #[test]
    fn test_assign_member_expression_list_index() {
        let (mut env, scope) = get_new_env();
        let list_val = RuntimeValue::List {
            data: vec![RuntimeValue::Int(1), RuntimeValue::Int(2)],
            data_type: Box::new(Some(crate::runtime::values::RuntimeType::Int)),
        };
        env.push_var(
            &scope,
            "lst".to_string(),
            Variable {
                value: list_val.clone(),
                var_type: VarType::Mutable,
            },
        )
        .unwrap();

        let member = NodeType::MemberExpression {
            path: vec![
                (NodeType::Identifier("lst".to_string()), false),
                (NodeType::IntLiteral(1), false),
            ],
        };

        env.assign_member_expression(&scope, member, RuntimeValue::Int(99))
            .unwrap();

        let updated = env.get_var(&scope, "lst").unwrap().value.clone();
        if let RuntimeValue::List { data, .. } = updated {
            assert_eq!(data[1], RuntimeValue::Int(99));
        } else {
            panic!("Expected list value");
        }
    }

    #[test]
    fn test_evaluate_member_expression_struct_field() {
        let (mut env, scope) = get_new_env();
        let mut map = std::collections::HashMap::new();
        map.insert("foo".to_string(), RuntimeValue::Int(123));
        let struct_val = RuntimeValue::Struct(scope, None, ObjectType::Map(map.clone()));

        env.push_var(
            &scope,
            "obj".to_string(),
            Variable {
                value: struct_val.clone(),
                var_type: VarType::Mutable,
            },
        )
        .unwrap();

        let member = NodeType::MemberExpression {
            path: vec![
                (NodeType::Identifier("obj".to_string()), false),
                (NodeType::Identifier("foo".to_string()), false),
            ],
        };

        let result = env
            .evaluate_member_expression(&scope, member)
            .unwrap()
            .unwrap_val(&env, &scope)
            .unwrap();
        assert_eq!(result, RuntimeValue::Int(123));
    }

    #[test]
    fn test_evaluate_member_expression_list_index() {
        let (mut env, scope) = get_new_env();
        let list_val = RuntimeValue::List {
            data: vec![RuntimeValue::Int(10), RuntimeValue::Int(20)],
            data_type: Box::new(Some(crate::runtime::values::RuntimeType::Int)),
        };

        env.push_var(
            &scope,
            "lst".to_string(),
            Variable {
                value: list_val.clone(),
                var_type: VarType::Mutable,
            },
        )
        .unwrap();

        let member = NodeType::MemberExpression {
            path: vec![
                (NodeType::Identifier("lst".to_string()), false),
                (NodeType::IntLiteral(1), false),
            ],
        };
        let result = env
            .evaluate_member_expression(&scope, member)
            .unwrap()
            .unwrap_val(&env, &scope)
            .unwrap();
        assert_eq!(result, RuntimeValue::Int(20));
    }

    #[test]
    fn test_evaluate_member_expression_struct_field_not_found() {
        let (mut env, scope) = get_new_env();
        let map = std::collections::HashMap::new();
        let struct_val = RuntimeValue::Struct(scope, None, ObjectType::Map(map));

        env.push_var(
            &scope,
            "obj".to_string(),
            Variable {
                value: struct_val.clone(),
                var_type: VarType::Mutable,
            },
        )
        .unwrap();

        let member = NodeType::MemberExpression {
            path: vec![
                (NodeType::Identifier("obj".to_string()), false),
                (NodeType::Identifier("missing".to_string()), false),
            ],
        };

        let result = env.evaluate_member_expression(&scope, member);
        assert!(result.is_err());
    }
}
