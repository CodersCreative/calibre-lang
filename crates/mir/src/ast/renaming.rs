use crate::{
    MiddleNode, MiddleNodeType,
    ast::{MirBreak, MirDeref, MirDrop, MirMove, MirRef, MirSpawn},
};
use calibre_parser::ast::{ObjectMap, idents::ParserText};
use rustc_hash::FxHashMap;

#[derive(Default)]
pub struct AlphaRenameState {
    pub data: FxHashMap<String, String>,
}

impl MiddleNode {
    pub fn rename(self, state: &mut AlphaRenameState) -> Self {
        Self {
            node_type: self.node_type.rename(state),
            span: self.span,
        }
    }
}

#[inline]
fn mapped_name_or_original(state: &AlphaRenameState, original: String) -> String {
    state.data.get(&original).cloned().unwrap_or(original)
}

impl MiddleNodeType {
    pub fn rename(self, state: &mut AlphaRenameState) -> Self {
        match self {
            MiddleNodeType::Break(MirBreak {
                label: _,
                value: None,
            })
            | MiddleNodeType::Continue(_)
            | MiddleNodeType::EmptyLine
            | MiddleNodeType::Null
            | MiddleNodeType::EnumExpression { data: None, .. }
            | MiddleNodeType::CharLiteral(_)
            | MiddleNodeType::FloatLiteral(_)
            | MiddleNodeType::BigLiteral(_)
            | MiddleNodeType::IntLiteral { .. }
            | MiddleNodeType::StringLiteral(_)
            | MiddleNodeType::ExternFunction { .. } => self,
            MiddleNodeType::Break(MirBreak {
                label,
                value: Some(value),
            }) => MiddleNodeType::Break(MirBreak {
                label,
                value: Some(Box::new(value.rename(state))),
            }),
            MiddleNodeType::Emit { value } => MiddleNodeType::Emit {
                value: Box::new(value.rename(state)),
            },
            MiddleNodeType::Spawn(MirSpawn { value }) => MiddleNodeType::Spawn(MirSpawn {
                value: Box::new(value.rename(state)),
            }),
            MiddleNodeType::RefStatement(MirRef { mutability, value }) => {
                MiddleNodeType::RefStatement(MirRef {
                    mutability,
                    value: Box::new(value.rename(state)),
                })
            }
            MiddleNodeType::DerefStatement(MirDeref { value }) => {
                MiddleNodeType::DerefStatement(MirDeref {
                    value: Box::new(value.rename(state)),
                })
            }
            MiddleNodeType::Drop(MirDrop { identifier }) => MiddleNodeType::Drop(MirDrop {
                identifier: mapped_name_or_original(state, identifier.text).into(),
            }),
            MiddleNodeType::Move(MirMove { identifier }) => MiddleNodeType::Move(MirMove {
                identifier: mapped_name_or_original(state, identifier.text).into(),
            }),
            MiddleNodeType::VariableDeclaration {
                var_type,
                identifier,
                value,
                data_type,
            } => {
                let new_name = format!("{}->{}", identifier.text, fastrand::u32(0..u32::MAX));
                state.data.insert(identifier.text, new_name.clone());

                MiddleNodeType::VariableDeclaration {
                    var_type,
                    identifier: ParserText {
                        text: new_name,
                        span: identifier.span,
                    },
                    value: Box::new(value.rename(state)),
                    data_type,
                }
            }
            MiddleNodeType::EnumExpression {
                identifier,
                value,
                data: Some(data),
            } => MiddleNodeType::EnumExpression {
                identifier,
                value,
                data: Some(Box::new(data.rename(state))),
            },
            MiddleNodeType::ScopeDeclaration {
                body,
                create_new_scope,
                is_temp,
                scope_id,
            } => MiddleNodeType::ScopeDeclaration {
                body: body.into_iter().map(|x| x.rename(state)).collect(),
                create_new_scope,
                is_temp,
                scope_id,
            },
            MiddleNodeType::FunctionDeclaration {
                parameters,
                body,
                return_type,
                scope_id,
            } => MiddleNodeType::FunctionDeclaration {
                parameters: parameters
                    .into_iter()
                    .map(|x| {
                        let new_name = format!("{}->{}", x.0.text, fastrand::u32(0..u32::MAX));
                        state.data.insert(x.0.text, new_name.clone());

                        (
                            ParserText {
                                text: new_name,
                                span: x.0.span,
                            },
                            x.1,
                            x.2.map(|x| Box::new(x.rename(state))),
                        )
                    })
                    .collect(),
                body: Box::new(body.rename(state)),
                return_type,
                scope_id,
            },
            MiddleNodeType::AssignmentExpression { identifier, value } => {
                MiddleNodeType::AssignmentExpression {
                    identifier: Box::new(identifier.rename(state)),
                    value: Box::new(value.rename(state)),
                }
            }
            MiddleNodeType::NegExpression { value } => MiddleNodeType::NegExpression {
                value: Box::new(value.rename(state)),
            },
            MiddleNodeType::DebugExpression {
                pretty_printed_str,
                value,
            } => MiddleNodeType::DebugExpression {
                pretty_printed_str,
                value: Box::new(value.rename(state)),
            },
            MiddleNodeType::AsExpression {
                value,
                data_type,
                failure_mode,
            } => MiddleNodeType::AsExpression {
                value: Box::new(value.rename(state)),
                data_type,
                failure_mode,
            },
            MiddleNodeType::IsExpression { value, data_type } => MiddleNodeType::IsExpression {
                value: Box::new(value.rename(state)),
                data_type,
            },
            MiddleNodeType::RangeDeclaration {
                from,
                to,
                inclusive,
            } => MiddleNodeType::RangeDeclaration {
                from: Box::new(from.rename(state)),
                to: Box::new(to.rename(state)),
                inclusive,
            },
            MiddleNodeType::LoopDeclaration {
                state: s,
                body,
                scope_id,
                label,
            } => MiddleNodeType::LoopDeclaration {
                state: s.map(|value| Box::new(value.rename(state))),
                body: Box::new(body.rename(state)),
                scope_id,
                label,
            },
            MiddleNodeType::Return { value } => MiddleNodeType::Return {
                value: value.map(|value| Box::new(value.rename(state))),
            },
            MiddleNodeType::Identifier(x) => MiddleNodeType::Identifier(ParserText {
                text: mapped_name_or_original(state, x.text),
                span: x.span,
            }),
            MiddleNodeType::ListLiteral(data_type, x) => MiddleNodeType::ListLiteral(
                data_type,
                x.into_iter().map(|x| x.rename(state)).collect(),
            ),
            MiddleNodeType::FieldAccess { base, field } => MiddleNodeType::FieldAccess {
                base: Box::new(base.rename(state)),
                field,
            },
            MiddleNodeType::ScopeAccess { base, field } => MiddleNodeType::ScopeAccess {
                base: Box::new(base.rename(state)),
                field,
            },
            MiddleNodeType::IndexAccess { base, index } => MiddleNodeType::IndexAccess {
                base: Box::new(base.rename(state)),
                index: Box::new(index.rename(state)),
            },
            MiddleNodeType::CallExpression { caller, args } => MiddleNodeType::CallExpression {
                caller: Box::new(caller.rename(state)),
                args: args.into_iter().map(|x| x.rename(state)).collect(),
            },
            MiddleNodeType::BinaryExpression {
                left,
                right,
                operator,
            } => MiddleNodeType::BinaryExpression {
                left: Box::new(left.rename(state)),
                right: Box::new(right.rename(state)),
                operator,
            },
            MiddleNodeType::ComparisonExpression {
                left,
                right,
                operator,
            } => MiddleNodeType::ComparisonExpression {
                left: Box::new(left.rename(state)),
                right: Box::new(right.rename(state)),
                operator,
            },
            MiddleNodeType::BooleanExpression {
                left,
                right,
                operator,
            } => MiddleNodeType::BooleanExpression {
                left: Box::new(left.rename(state)),
                right: Box::new(right.rename(state)),
                operator,
            },
            MiddleNodeType::AggregateExpression { identifier, value } => {
                MiddleNodeType::AggregateExpression {
                    identifier,
                    value: ObjectMap(
                        value
                            .0
                            .into_iter()
                            .map(|x| (x.0, x.1.rename(state)))
                            .collect(),
                    ),
                }
            }
            MiddleNodeType::Conditional {
                comparison,
                then,
                otherwise,
            } => MiddleNodeType::Conditional {
                comparison: Box::new(comparison.rename(state)),
                then: Box::new(then.rename(state)),
                otherwise: otherwise.map(|value| Box::new(value.rename(state))),
            },
        }
    }
}
