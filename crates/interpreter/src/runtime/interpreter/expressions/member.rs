use std::fmt::{Debug, Display};

use calibre_mir::ast::{MiddleNode, MiddleNodeType};
use calibre_parser::ast::{ObjectMap, ObjectType};

use crate::runtime::{
    interpreter::InterpreterErr, scope::InterpreterEnvironment, values::RuntimeValue,
};

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

#[derive(Clone, Debug)]
pub enum MembrExprPathRes {
    Value(RuntimeValue),
    Path(Vec<MemberPathType>),
}

impl InterpreterEnvironment {
    fn handle_call_expr_in_path(
        &mut self,
        scope: &u64,
        path: Vec<MemberPathType>,
        value_node: Box<MiddleNode>,
        mut args: Vec<(MiddleNode, Option<MiddleNode>)>,
    ) -> Result<MembrExprPathRes, InterpreterErr> {
        match value_node.node_type {
            MiddleNodeType::Identifier(value) => {
                return Ok(MembrExprPathRes::Value(
                    match self.evaluate(
                        scope,
                        MiddleNode::new(
                            MiddleNodeType::EnumExpression {
                                identifier: path[0].clone().to_string().into(),
                                value: value.to_string().into(),
                                data: Some(
                                    args.clone()
                                        .into_iter()
                                        .map(|x| x.0.clone())
                                        .collect::<Vec<_>>()
                                        .into(),
                                ),
                            },
                            value_node.span,
                        ),
                    ) {
                        Ok(x) => x,
                        Err(e) => {
                            if let Ok(x) = self
                                .get_function(&path[0].to_string(), &value)
                                .map(|x| x.0.clone())
                            {
                                let func = self.evaluate(scope, x)?;
                                self.evaluate_function(scope, func, args)?
                            } else {
                                let obj = match match self.get_var(&path[0].to_string()) {
                                    Ok(x) => x.value.clone(),
                                    _ => return Err(e),
                                } {
                                    RuntimeValue::Aggregate(Some(p), _) => p.clone(),
                                    RuntimeValue::Enum(p, _, _) => p.clone(),
                                    _ => return Err(e),
                                };

                                if let Ok(x) = self.get_function(&obj, &value) {
                                    if x.2 {
                                        println!("{:?}", path[0]);
                                        args.insert(
                                            0,
                                            (
                                                MiddleNode::new(
                                                    MiddleNodeType::Identifier(
                                                        path[0].to_string().into(),
                                                    ),
                                                    value_node.span,
                                                ),
                                                None,
                                            ),
                                        );
                                    }

                                    let func = self.evaluate(scope, x.0.clone())?;
                                    self.evaluate_function(scope, func, args)?
                                } else {
                                    return Err(e);
                                }
                            }
                        }
                    },
                ));
            }
            _ => panic!(),
        }
    }

    pub fn get_member_expression_path(
        &mut self,
        scope: &u64,
        og_path: Vec<(MiddleNode, bool)>,
    ) -> Result<MembrExprPathRes, InterpreterErr> {
        let mut path: Vec<MemberPathType> = Vec::new();

        for (node, computed) in og_path.into_iter() {
            if !computed {
                match &node.node_type {
                    MiddleNodeType::Identifier(x) => {
                        path.push(MemberPathType::Dot(x.to_string()));
                    }
                    MiddleNodeType::IntLiteral(x) => {
                        path.push(MemberPathType::Dot(x.to_string()));
                    }
                    MiddleNodeType::CallExpression(value_node, args) => {
                        return self.handle_call_expr_in_path(
                            scope,
                            path,
                            value_node.clone(),
                            args.clone(),
                        );
                    }
                    MiddleNodeType::MemberExpression { path: p } => {
                        match self.get_member_expression_path(scope, p.to_vec())? {
                            MembrExprPathRes::Value(x) => return Ok(MembrExprPathRes::Value(x)),
                            MembrExprPathRes::Path(mut p) => path.append(&mut p),
                        }
                        continue;
                    }
                    _ => return Err(InterpreterErr::UnexpectedNode(node.node_type)),
                }

                continue;
            }

            match &node.node_type {
                MiddleNodeType::MemberExpression { path: p } => {
                    match self.get_member_expression_path(scope, p.to_vec())? {
                        MembrExprPathRes::Value(x) => return Ok(MembrExprPathRes::Value(x)),
                        MembrExprPathRes::Path(mut p) => path.append(&mut p),
                    }
                    continue;
                }
                _ if computed => {}
                MiddleNodeType::Identifier(x) => {
                    path.push(MemberPathType::Computed(x.to_string()));
                    continue;
                }
                MiddleNodeType::FloatLiteral(x) => {
                    path.push(MemberPathType::Computed(x.to_string()));
                    continue;
                }
                MiddleNodeType::IntLiteral(x) => {
                    path.push(MemberPathType::Computed(x.to_string()));
                    continue;
                }
                _ => {}
            }

            match self.evaluate(&scope, node.clone()) {
                Ok(value) => path.push(MemberPathType::Computed(match value {
                    RuntimeValue::Int(x) => x.to_string(),
                    RuntimeValue::Float(x) => x.to_string(),
                    RuntimeValue::Str(x) => x.to_string(),
                    RuntimeValue::Char(x) => x.to_string(),
                    x => return Ok(MembrExprPathRes::Value(x.clone())),
                })),
                Err(e) if path.len() == 1 => match node.node_type {
                    MiddleNodeType::Identifier(value) => {
                        return Ok(MembrExprPathRes::Value(self.evaluate(
                            scope,
                            MiddleNode::new(
                                MiddleNodeType::EnumExpression {
                                    identifier: path.remove(0).to_string().into(),
                                    value,
                                    data: None,
                                },
                                node.span,
                            ),
                        )?));
                    }
                    MiddleNodeType::CallExpression(value_node, args) => {
                        return self.handle_call_expr_in_path(
                            scope,
                            path,
                            value_node.clone(),
                            args.clone(),
                        );
                    }
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
        member: MiddleNode,
        value: RuntimeValue,
    ) -> Result<(), InterpreterErr> {
        match member.node_type {
            MiddleNodeType::MemberExpression { path: og_path } => {
                let path = match self.get_member_expression_path(scope, og_path)? {
                    MembrExprPathRes::Path(x) => x,
                    MembrExprPathRes::Value(_) => return Ok(()),
                };

                let RuntimeValue::Ref(pointer, _) = self.get_member_ref(&path)? else {
                    unreachable!()
                };

                let _ = self.assign_var_from_ref_pointer(&pointer, value)?;

                Ok(())
            }
            _ => unreachable!(),
        }
    }

    pub fn evaluate_member_expression(
        &mut self,
        scope: &u64,
        exp: MiddleNode,
    ) -> Result<RuntimeValue, InterpreterErr> {
        match exp.node_type {
            MiddleNodeType::MemberExpression { path: og_path } => {
                let mut path = match self.get_member_expression_path(scope, og_path)? {
                    MembrExprPathRes::Path(x) => x,
                    MembrExprPathRes::Value(x) => return Ok(x),
                };

                match self.get_member_ref(&path) {
                    Ok(RuntimeValue::Ref(pointer, _)) => {
                        let mut value = &self.get_var(&pointer)?.value;
                        while let RuntimeValue::Ref(pointer, _) = value {
                            value = &self.get_var(&pointer)?.value;
                        }
                        Ok(value.clone())
                    }
                    Ok(x) => Ok(x),
                    Err(_) if path.len() == 2 => {
                        return self.evaluate(
                            scope,
                            MiddleNode::new(
                                MiddleNodeType::EnumExpression {
                                    identifier: path.remove(0).to_string().into(),
                                    value: path.remove(0).to_string().into(),
                                    data: None,
                                },
                                exp.span,
                            ),
                        );
                    }
                    Err(e) => Err(e),
                }
            }
            _ => unreachable!(),
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
