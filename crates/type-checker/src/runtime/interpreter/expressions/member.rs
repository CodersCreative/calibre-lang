use std::fmt::Display;

use calibre_common::errors::ScopeErr;
use calibre_parser::ast::{Node, NodeType, ObjectType};

use crate::runtime::{interpreter::InterpreterErr, scope::CheckerEnvironment, values::RuntimeType};

#[derive(Clone, Debug)]
pub enum MemberPathType {
    Dot(String),
    Computed(String),
}

impl Display for MemberPathType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let txt = match self {
            Self::Dot(x) => x.clone(),
            Self::Computed(x) => x.clone(),
        };
        write!(f, "{}", txt)
    }
}
pub enum MembrExprPathRes {
    Value(RuntimeType),
    Path(Vec<MemberPathType>),
}

impl CheckerEnvironment {
    pub fn get_scope_member_scope_path(
        &mut self,
        scope: &u64,
        mut path: Vec<Node>,
    ) -> Result<(u64, Node), InterpreterErr> {
        if let NodeType::Identifier(x) = &path[0].node_type {
            if let Ok(s) = self.get_next_scope(*scope, &x) {
                if path.len() <= 2 {
                    return Ok((s, path.remove(1)));
                }

                match self.get_scope_member_scope_path(&s, path[1..].to_vec()) {
                    Ok(x) => return Ok(x),
                    _ => {}
                }
            }
        }

        Err(ScopeErr::Scope(format!("{:?}", path[0].node_type)).into())
    }

    pub fn evaluate_scope_member_expression(
        &mut self,
        scope: &u64,
        path: Vec<Node>,
    ) -> Result<RuntimeType, InterpreterErr> {
        let (s, node) = self.get_scope_member_scope_path(scope, path)?;
        self.evaluate(&s, node)
    }

    pub fn get_member_expression_path(
        &mut self,
        scope: &u64,
        og_path: Vec<(Node, bool)>,
    ) -> Result<MembrExprPathRes, InterpreterErr> {
        let mut path: Vec<MemberPathType> = Vec::new();

        for (node, computed) in og_path.into_iter() {
            if !computed {
                match &node.node_type {
                    NodeType::Identifier(x) => {
                        path.push(MemberPathType::Dot(x.to_string()));
                    }
                    NodeType::IntLiteral(x) => {
                        path.push(MemberPathType::Dot(x.to_string()));
                    }
                    _ => return Err(InterpreterErr::UnexpectedNode(node.node_type)),
                }

                continue;
            }

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
                    path.push(MemberPathType::Computed(x.to_string()));
                    continue;
                }
                NodeType::FloatLiteral(x) => {
                    path.push(MemberPathType::Computed(x.to_string()));
                    continue;
                }
                NodeType::IntLiteral(x) => {
                    path.push(MemberPathType::Computed(x.to_string()));
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
                                    identifier: path.remove(0).to_string(),
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
                                            identifier: path[0].clone().to_string(),
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
                                        if let Ok(pointer) =
                                            self.get_object_pointer(scope, &path[0].to_string())
                                        {
                                            let x = self
                                                .get_function(&pointer, &value)
                                                .map(|x| x.0.clone())?;
                                            self.evaluate_function(scope, x, args)?
                                        } else if let Ok(pointer) =
                                            self.get_var_pointer(scope, &path[0].to_string())
                                        {
                                            let obj = match match self.get_var(&pointer) {
                                                Ok(x) => x.value.clone(),
                                                _ => return Err(e),
                                            } {
                                                RuntimeType::Struct(p, _) => p.unwrap().clone(),
                                                RuntimeType::Enum(p, _) => p.clone(),
                                                _ => return Err(e),
                                            };

                                            if let Ok(x) = self.get_function(&obj, &value) {
                                                if x.2 {
                                                    args.insert(
                                                        0,
                                                        (
                                                            Node::new(
                                                                NodeType::Identifier(
                                                                    path[0].to_string(),
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
                                        } else {
                                            return Err(e);
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
                                    identifier: path.remove(0).to_string(),
                                    value: path.remove(0).to_string(),
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
