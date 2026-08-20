use ast::{MiddleNode, MiddleNodeType};
use calibre_parser::{
    Span,
    ast::{
        binary::BinaryOperator,
        idents::{PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::{DestructurePattern, Node, NodeType, VarType},
        types::ParserDataType,
    },
};
use environment::*;

pub mod ast;
pub mod context;
pub mod environment;
pub mod errors;
pub mod inline;
pub mod multipass;
pub mod native;
pub mod scoping;
pub mod symbols;
pub mod tags;
pub mod testing;
pub mod translate;
pub mod traversal;
pub mod typing;

impl MiddleEnvironment {
    fn emit_destructure_statements(
        &self,
        tmp_ident: &PotentialDollarIdentifier,
        pattern: &DestructurePattern,
        span: Span,
        is_declaration: bool,
    ) -> Vec<Node> {
        let estimated = match pattern {
            DestructurePattern::Tuple(bindings) => bindings.iter().flatten().count(),
            DestructurePattern::Struct(fields) => fields.len(),
        };
        let mut out = Vec::with_capacity(estimated);

        let tmp_member_base = || {
            Node::new(
                span,
                NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                    tmp_ident.clone(),
                )),
            )
        };

        let push_binding = |out: &mut Vec<Node>,
                            var_type: &VarType,
                            name: &PotentialDollarIdentifier,
                            member: Node| {
            if is_declaration {
                out.push(Node::new(
                    span,
                    NodeType::VariableDeclaration {
                        var_type: *var_type,
                        identifier: name.clone(),
                        data_type: ParserDataType::auto(span),
                        value: Box::new(member),
                    },
                ));
            } else {
                out.push(Node::new(
                    span,
                    NodeType::AssignmentExpression {
                        identifier: Box::new(Node::new(
                            span,
                            NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                                name.clone(),
                            )),
                        )),
                        value: Box::new(member),
                    },
                ));
            }
        };
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
                            let index_node = Node::int(span, idx);
                            Node::new(
                                span,
                                NodeType::MemberExpression {
                                    path: vec![(tmp_member_base(), false), (index_node, true)],
                                },
                            )
                        } else {
                            Node::new(
                                span,
                                NodeType::MemberExpression {
                                    path: vec![
                                        (tmp_member_base(), false),
                                        (Node::int(span, idx), true),
                                    ],
                                },
                            )
                        };
                        push_binding(&mut out, var_type, name, member);
                    }
                }

                for (i, entry) in tail.into_iter().enumerate() {
                    if let Some((var_type, name)) = entry {
                        let offset = (total_tail - i as i64) as i64;
                        let index_expr = Node::new(
                            span,
                            NodeType::BinaryExpression {
                                left: Box::new(Node::len(
                                    span,
                                    Node::new(
                                        span,
                                        NodeType::Identifier(
                                            PotentialGenericTypeIdentifier::Identifier(
                                                tmp_ident.clone(),
                                            ),
                                        ),
                                    ),
                                )),
                                right: Box::new(Node::int(span, offset)),
                                operator: BinaryOperator::Sub,
                            },
                        );
                        let member = Node::new(
                            span,
                            NodeType::MemberExpression {
                                path: vec![(tmp_member_base(), false), (index_expr, true)],
                            },
                        );
                        push_binding(&mut out, var_type, name, member);
                    }
                }
            }
            DestructurePattern::Struct(fields) => {
                for (field, var_type, name) in fields {
                    let member = Node::new(
                        span,
                        NodeType::MemberExpression {
                            path: vec![
                                (tmp_member_base(), false),
                                (Node::identifier(span, field), false),
                            ],
                        },
                    );
                    push_binding(&mut out, var_type, name, member);
                }
            }
        }

        out
    }
}
