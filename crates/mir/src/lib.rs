use ast::{MiddleNode, MiddleNodeType};
use calibre_parser::{
    Span,
    ast::{
        binary::BinaryOperator,
        idents::{PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::{AstNode, AstNodeType, DestructurePattern, VarType},
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
    ) -> Vec<AstNode> {
        let estimated = match pattern {
            DestructurePattern::Tuple(bindings) => bindings.iter().flatten().count(),
            DestructurePattern::Struct(fields) => fields.len(),
        };
        let mut out = Vec::with_capacity(estimated);

        let tmp_member_base = || {
            AstNode::new(
                span,
                AstNodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                    tmp_ident.clone(),
                )),
            )
        };

        let push_binding = |out: &mut Vec<AstNode>,
                            var_type: &VarType,
                            name: &PotentialDollarIdentifier,
                            member: AstNode| {
            if is_declaration {
                out.push(AstNode::new(
                    span,
                    AstNodeType::VariableDeclaration {
                        var_type: *var_type,
                        identifier: name.clone(),
                        data_type: ParserDataType::auto(span),
                        value: Box::new(member),
                    },
                ));
            } else {
                out.push(AstNode::new(
                    span,
                    AstNodeType::AssignmentExpression {
                        identifier: Box::new(AstNode::new(
                            span,
                            AstNodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
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
                        push_binding(&mut out, var_type, name, {
                            let index_node = AstNode::int(span, idx);
                            AstNode::new(
                                span,
                                AstNodeType::IndexAccess {
                                    base: Box::new(tmp_member_base()),
                                    index: Box::new(index_node),
                                },
                            )
                        });
                    }
                }

                for (i, entry) in tail.into_iter().enumerate() {
                    if let Some((var_type, name)) = entry {
                        let index_expr = AstNode::new(
                            span,
                            AstNodeType::BinaryExpression {
                                left: Box::new(AstNode::len(
                                    span,
                                    AstNode::new(
                                        span,
                                        AstNodeType::Identifier(
                                            PotentialGenericTypeIdentifier::Identifier(
                                                tmp_ident.clone(),
                                            ),
                                        ),
                                    ),
                                )),
                                right: Box::new(AstNode::int(span, total_tail - i as i64)),
                                operator: BinaryOperator::Sub,
                            },
                        );

                        push_binding(
                            &mut out,
                            var_type,
                            name,
                            AstNode::new(
                                span,
                                AstNodeType::IndexAccess {
                                    base: Box::new(tmp_member_base()),
                                    index: Box::new(index_expr),
                                },
                            ),
                        );
                    }
                }
            }
            DestructurePattern::Struct(fields) => {
                for (field, var_type, name) in fields {
                    push_binding(
                        &mut out,
                        var_type,
                        name,
                        AstNode::new(
                            span,
                            AstNodeType::FieldAccess {
                                base: Box::new(tmp_member_base()),
                                field: PotentialDollarIdentifier::new(span, field),
                            },
                        ),
                    );
                }
            }
        }

        out
    }
}
