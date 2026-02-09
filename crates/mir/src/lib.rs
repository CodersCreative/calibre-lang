use ast::{MiddleNode, MiddleNodeType};
use calibre_parser::{
    ast::{
        CallArg, DestructurePattern, Node, NodeType, ParserDataType, ParserInnerType, ParserText,
        PotentialDollarIdentifier, PotentialGenericTypeIdentifier, PotentialNewType,
        binary::BinaryOperator,
    },
    lexer::Span,
};

use crate::errors::MiddleErr;
use environment::*;

pub mod ast;
pub mod environment;
pub mod errors;
pub mod infer;
pub mod native;
pub mod translate;

impl MiddleEnvironment {
    fn emit_destructure_statements(
        &self,
        tmp_ident: &PotentialDollarIdentifier,
        pattern: &DestructurePattern,
        span: Span,
        is_declaration: bool,
    ) -> Vec<Node> {
        let mut out = Vec::new();
        match pattern {
            DestructurePattern::Tuple(bindings) => {
                let has_tail = bindings.iter().any(|b| b.is_none());
                let mut head = Vec::new();
                let mut tail = Vec::new();
                let mut in_tail = false;
                for binding in bindings {
                    if binding.is_none() {
                        in_tail = true;
                        continue;
                    }
                    if in_tail {
                        tail.push(binding);
                    } else {
                        head.push(binding);
                    }
                }

                let total_tail = tail.len() as i64;
                for (idx, entry) in head.into_iter().enumerate() {
                    if let Some((var_type, name)) = entry {
                        let member = if has_tail {
                            let index_node = Node::new(span, NodeType::IntLiteral(idx as i64));
                            Node::new(
                                span,
                                NodeType::MemberExpression {
                                    path: vec![
                                        (
                                            Node::new(
                                                span,
                                                NodeType::Identifier(
                                                    PotentialGenericTypeIdentifier::Identifier(
                                                        tmp_ident.clone(),
                                                    ),
                                                ),
                                            ),
                                            false,
                                        ),
                                        (index_node, true),
                                    ],
                                },
                            )
                        } else {
                            let index_ident = PotentialDollarIdentifier::Identifier(
                                ParserText::from(idx.to_string()),
                            );
                            Node::new(
                                span,
                                NodeType::MemberExpression {
                                    path: vec![
                                        (
                                            Node::new(
                                                span,
                                                NodeType::Identifier(
                                                    PotentialGenericTypeIdentifier::Identifier(
                                                        tmp_ident.clone(),
                                                    ),
                                                ),
                                            ),
                                            false,
                                        ),
                                        (
                                            Node::new(
                                                span,
                                                NodeType::Identifier(
                                                    PotentialGenericTypeIdentifier::Identifier(
                                                        index_ident,
                                                    ),
                                                ),
                                            ),
                                            false,
                                        ),
                                    ],
                                },
                            )
                        };
                        if is_declaration {
                            out.push(Node::new(
                                span,
                                NodeType::VariableDeclaration {
                                    var_type: var_type.clone(),
                                    identifier: name.clone(),
                                    data_type: PotentialNewType::DataType(ParserDataType::from(
                                        ParserInnerType::Auto(None),
                                    )),
                                    value: Box::new(member),
                                },
                            ));
                        } else {
                            out.push(Node::new(
                                span,
                                NodeType::AssignmentExpression {
                                    identifier: Box::new(Node::new(
                                        span,
                                        NodeType::Identifier(
                                            PotentialGenericTypeIdentifier::Identifier(
                                                name.clone(),
                                            ),
                                        ),
                                    )),
                                    value: Box::new(member),
                                },
                            ));
                        }
                    }
                }

                for (i, entry) in tail.into_iter().enumerate() {
                    if let Some((var_type, name)) = entry {
                        let len_call = Node::new(
                            span,
                            NodeType::CallExpression {
                                string_fn: None,
                                generic_types: Vec::new(),
                                caller: Box::new(Node::new(
                                    span,
                                    NodeType::Identifier(
                                        PotentialGenericTypeIdentifier::Identifier(
                                            ParserText::from(String::from("len")).into(),
                                        ),
                                    ),
                                )),
                                args: vec![CallArg::Value(Node::new(
                                    span,
                                    NodeType::Identifier(
                                        PotentialGenericTypeIdentifier::Identifier(
                                            tmp_ident.clone().into(),
                                        ),
                                    ),
                                ))],
                                reverse_args: Vec::new(),
                            },
                        );
                        let offset = (total_tail - i as i64) as i64;
                        let index_expr = Node::new(
                            span,
                            NodeType::BinaryExpression {
                                left: Box::new(len_call),
                                right: Box::new(Node::new(span, NodeType::IntLiteral(offset))),
                                operator: BinaryOperator::Sub,
                            },
                        );
                        let member = Node::new(
                            span,
                            NodeType::MemberExpression {
                                path: vec![
                                    (
                                        Node::new(
                                            span,
                                            NodeType::Identifier(
                                                PotentialGenericTypeIdentifier::Identifier(
                                                    tmp_ident.clone().into(),
                                                ),
                                            ),
                                        ),
                                        false,
                                    ),
                                    (index_expr, true),
                                ],
                            },
                        );

                        if is_declaration {
                            out.push(Node::new(
                                span,
                                NodeType::VariableDeclaration {
                                    var_type: var_type.clone(),
                                    identifier: name.clone().into(),
                                    data_type: PotentialNewType::DataType(ParserDataType::from(
                                        ParserInnerType::Auto(None),
                                    )),
                                    value: Box::new(member),
                                },
                            ));
                        } else {
                            out.push(Node::new(
                                span,
                                NodeType::AssignmentExpression {
                                    identifier: Box::new(Node::new(
                                        span,
                                        NodeType::Identifier(
                                            PotentialGenericTypeIdentifier::Identifier(
                                                name.clone().into(),
                                            ),
                                        ),
                                    )),
                                    value: Box::new(member),
                                },
                            ));
                        }
                    }
                }
            }
            DestructurePattern::Struct(fields) => {
                for (field, var_type, name) in fields {
                    let member = Node::new(
                        span,
                        NodeType::MemberExpression {
                            path: vec![
                                (
                                    Node::new(
                                        span,
                                        NodeType::Identifier(
                                            PotentialGenericTypeIdentifier::Identifier(
                                                tmp_ident.clone().into(),
                                            ),
                                        ),
                                    ),
                                    false,
                                ),
                                (
                                    Node::new(
                                        span,
                                        NodeType::Identifier(
                                            PotentialGenericTypeIdentifier::Identifier(
                                                PotentialDollarIdentifier::Identifier(
                                                    ParserText::from(field.clone()),
                                                )
                                                .into(),
                                            ),
                                        ),
                                    ),
                                    false,
                                ),
                            ],
                        },
                    );

                    if is_declaration {
                        out.push(Node::new(
                            span,
                            NodeType::VariableDeclaration {
                                var_type: var_type.clone(),
                                identifier: name.clone().into(),
                                data_type: PotentialNewType::DataType(ParserDataType::from(
                                    ParserInnerType::Auto(None),
                                )),
                                value: Box::new(member),
                            },
                        ));
                    } else {
                        out.push(Node::new(
                            span,
                            NodeType::AssignmentExpression {
                                identifier: Box::new(Node::new(
                                    span,
                                    NodeType::Identifier(
                                        PotentialGenericTypeIdentifier::Identifier(
                                            name.clone().into(),
                                        ),
                                    ),
                                )),
                                value: Box::new(member),
                            },
                        ));
                    }
                }
            }
        }

        out
    }

    fn collect_defers_chain(&self, scope: &u64) -> Vec<Node> {
        let mut out = Vec::new();
        let mut current = Some(*scope);
        while let Some(id) = current {
            if let Some(s) = self.scopes.get(&id) {
                out.extend(s.defers.clone());
                current = s.parent;
            } else {
                break;
            }
        }
        out
    }

    fn insert_auto_drops(
        &self,
        stmts: &mut Vec<MiddleNode>,
        defined: &[String],
        protected_extra: &[String],
    ) {
        use rustc_hash::FxHashMap;
        use rustc_hash::FxHashSet;
        let mut last_use: FxHashMap<String, usize> = FxHashMap::default();
        let mut protected: FxHashSet<String> = FxHashSet::default();
        let mut returned_idents: FxHashSet<String> = FxHashSet::default();
        for ident in protected_extra {
            protected.insert(ident.clone());
        }

        for (idx, stmt) in stmts.iter().enumerate() {
            if let MiddleNodeType::Return { value: Some(val) } = &stmt.node_type {
                for ident in val.identifiers_used() {
                    let ident = ident.to_string();
                    protected.insert(ident.clone());
                    returned_idents.insert(ident);
                }
            }
            if let MiddleNodeType::FunctionDeclaration { .. } = &stmt.node_type {
                for ident in stmt.captured() {
                    protected.insert(ident.to_string());
                }
            }
            for ident in stmt.identifiers_used() {
                last_use.insert(ident.to_string(), idx);
            }
            if let MiddleNodeType::Drop(name) = &stmt.node_type {
                last_use.insert(name.to_string(), idx);
            }
        }

        let mut inserts: Vec<(usize, String)> = Vec::new();
        for name in defined {
            if protected.contains(name) {
                continue;
            }
            let must_keep = returned_idents.contains(name);
            if let Some(&idx) = last_use.get(name) {
                if idx >= stmts.len() {
                    continue;
                } else if let Some(stmt) = stmts.get(idx) {
                    match &stmt.node_type {
                        MiddleNodeType::Return { .. }
                        | MiddleNodeType::Break
                        | MiddleNodeType::Continue => continue,
                        MiddleNodeType::Drop(drop_name) if drop_name.text == *name => continue,
                        _ => {}
                    }
                }
                let insert_idx = (idx + 1).min(stmts.len());
                inserts.push((insert_idx, name.clone()));
            } else if !must_keep {
                inserts.push((stmts.len(), name.clone()));
            }
        }

        inserts.sort_by(|a, b| b.0.cmp(&a.0));
        for (idx, name) in inserts {
            if idx <= stmts.len() {
                stmts.insert(
                    idx,
                    MiddleNode::new(MiddleNodeType::Drop(name.into()), self.current_span()),
                );
            }
        }
    }
}

impl MiddleEnvironment {
    pub fn get_scope_member_scope_path(
        &mut self,
        scope: &u64,
        mut path: Vec<Node>,
    ) -> Result<(u64, Node), MiddleErr> {
        if let NodeType::Identifier(x) = &path[0].node_type {
            let x = self
                .resolve_potential_generic_ident(scope, x)
                .unwrap_or(x.to_string().into());
            if let Ok(s) = self.get_next_scope(*scope, &x.text) {
                if path.len() <= 2 {
                    return Ok((s, path.remove(1)));
                }

                match self.get_scope_member_scope_path(&s, path[1..].to_vec()) {
                    Ok(x) => return Ok(x),
                    _ => {}
                }
            }
        }

        Err(MiddleErr::At(
            path[0].span,
            Box::new(MiddleErr::Scope(format!("{:?}", path[0].node_type))),
        )
        .into())
    }

    pub fn handle_operator_overloads(
        &mut self,
        scope: &u64,
        span: Span,
        left: Node,
        right: Node,
        operator: Operator,
    ) -> Result<Option<MiddleNode>, MiddleErr> {
        if let (Some(left_ty), Some(right_ty)) = (
            self.resolve_type_from_node(scope, &left),
            self.resolve_type_from_node(scope, &right),
        ) {
            if let Some(overload) = self
                .overloads
                .iter()
                .filter(|x| x.parameters.len() == 2 && x.operator == operator)
                .find(|x| {
                    x.parameters[0].data_type == left_ty.data_type
                        && x.parameters[1].data_type == right_ty.data_type
                })
                .map(|x| x.clone())
            {
                return Ok(Some(MiddleNode {
                    node_type: MiddleNodeType::CallExpression {
                        caller: Box::new(self.evaluate_inner(scope, overload.func.clone())?),
                        args: vec![
                            self.evaluate_inner(scope, left)?,
                            self.evaluate_inner(scope, right)?,
                        ],
                    },
                    span,
                }));
            }
        }

        Ok(None)
    }

    pub fn get_operator_overload(
        &mut self,
        scope: &u64,
        left: &Node,
        right: &Node,
        operator: &Operator,
    ) -> Option<&MiddleOverload> {
        if let (Some(left_ty), Some(right_ty)) = (
            self.resolve_type_from_node(scope, left),
            self.resolve_type_from_node(scope, right),
        ) {
            if let Some(overload) = self
                .overloads
                .iter()
                .filter(|x| x.parameters.len() == 2 && &x.operator == operator)
                .find(|x| {
                    x.parameters[0].data_type == left_ty.data_type
                        && x.parameters[1].data_type == right_ty.data_type
                })
            {
                return Some(overload);
            }
        }

        None
    }

    pub fn evaluate_scope_member_expression(
        &mut self,
        scope: &u64,
        path: Vec<Node>,
    ) -> Result<MiddleNode, MiddleErr> {
        let (s, node) = self.get_scope_member_scope_path(scope, path)?;
        Ok(self.evaluate(&s, node))
    }
}
