use crate::{
    MiddleNode, MiddleNodeType,
    ast::{
        MirAggregate, MirAs, MirAssignment, MirBinary, MirBoolean, MirBreak, MirCall,
        MirComparison, MirConditional, MirDebug, MirDeref, MirDrop, MirEmit, MirEnum, MirField,
        MirIdentifier, MirIndex, MirIs, MirList, MirLoop, MirMove, MirNeg, MirRange, MirRef,
        MirReturn, MirScope, MirSpawn,
    },
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
            | MiddleNodeType::EnumExpression(MirEnum { data: None, .. })
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
            MiddleNodeType::Emit(MirEmit { value }) => MiddleNodeType::Emit(MirEmit {
                value: Box::new(value.rename(state)),
            }),
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
            MiddleNodeType::EnumExpression(MirEnum {
                identifier,
                value,
                data: Some(data),
            }) => MiddleNodeType::EnumExpression(MirEnum {
                identifier,
                value,
                data: Some(Box::new(data.rename(state))),
            }),
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
            MiddleNodeType::AssignmentExpression(MirAssignment { identifier, value }) => {
                MiddleNodeType::AssignmentExpression(MirAssignment {
                    identifier: Box::new(identifier.rename(state)),
                    value: Box::new(value.rename(state)),
                })
            }
            MiddleNodeType::NegExpression(MirNeg { value }) => {
                MiddleNodeType::NegExpression(MirNeg {
                    value: Box::new(value.rename(state)),
                })
            }
            MiddleNodeType::DebugExpression(MirDebug {
                pretty_printed_str,
                value,
            }) => MiddleNodeType::DebugExpression(MirDebug {
                pretty_printed_str,
                value: Box::new(value.rename(state)),
            }),
            MiddleNodeType::AsExpression(MirAs {
                value,
                data_type,
                failure_mode,
            }) => MiddleNodeType::AsExpression(MirAs {
                value: Box::new(value.rename(state)),
                data_type,
                failure_mode,
            }),
            MiddleNodeType::IsExpression(MirIs { value, data_type }) => {
                MiddleNodeType::IsExpression(MirIs {
                    value: Box::new(value.rename(state)),
                    data_type,
                })
            }
            MiddleNodeType::RangeDeclaration(MirRange {
                from,
                to,
                inclusive,
            }) => MiddleNodeType::RangeDeclaration(MirRange {
                from: Box::new(from.rename(state)),
                to: Box::new(to.rename(state)),
                inclusive,
            }),
            MiddleNodeType::LoopDeclaration(MirLoop {
                state: s,
                body,
                scope_id,
                label,
            }) => MiddleNodeType::LoopDeclaration(MirLoop {
                state: s.map(|value| Box::new(value.rename(state))),
                body: Box::new(body.rename(state)),
                scope_id,
                label,
            }),
            MiddleNodeType::Return(MirReturn { value }) => MiddleNodeType::Return(MirReturn {
                value: value.map(|value| Box::new(value.rename(state))),
            }),
            MiddleNodeType::Identifier(MirIdentifier { identifier }) => {
                MiddleNodeType::Identifier(MirIdentifier {
                    identifier: ParserText {
                        text: mapped_name_or_original(state, identifier.text),
                        span: identifier.span,
                    },
                })
            }
            MiddleNodeType::ListLiteral(MirList { data_type, values }) => {
                MiddleNodeType::ListLiteral(MirList {
                    data_type,
                    values: values.into_iter().map(|x| x.rename(state)).collect(),
                })
            }
            MiddleNodeType::FieldAccess(MirField { base, field }) => {
                MiddleNodeType::FieldAccess(MirField {
                    base: Box::new(base.rename(state)),
                    field,
                })
            }
            MiddleNodeType::ScopeAccess(MirScope { base, field }) => {
                MiddleNodeType::ScopeAccess(MirScope {
                    base: Box::new(base.rename(state)),
                    field,
                })
            }
            MiddleNodeType::IndexAccess(MirIndex { base, index }) => {
                MiddleNodeType::IndexAccess(MirIndex {
                    base: Box::new(base.rename(state)),
                    index: Box::new(index.rename(state)),
                })
            }
            MiddleNodeType::CallExpression(MirCall { caller, args }) => {
                MiddleNodeType::CallExpression(MirCall {
                    caller: Box::new(caller.rename(state)),
                    args: args.into_iter().map(|x| x.rename(state)).collect(),
                })
            }
            MiddleNodeType::BinaryExpression(MirBinary {
                left,
                right,
                operator,
            }) => MiddleNodeType::BinaryExpression(MirBinary {
                left: Box::new(left.rename(state)),
                right: Box::new(right.rename(state)),
                operator,
            }),
            MiddleNodeType::ComparisonExpression(MirComparison {
                left,
                right,
                operator,
            }) => MiddleNodeType::ComparisonExpression(MirComparison {
                left: Box::new(left.rename(state)),
                right: Box::new(right.rename(state)),
                operator,
            }),
            MiddleNodeType::BooleanExpression(MirBoolean {
                left,
                right,
                operator,
            }) => MiddleNodeType::BooleanExpression(MirBoolean {
                left: Box::new(left.rename(state)),
                right: Box::new(right.rename(state)),
                operator,
            }),
            MiddleNodeType::AggregateExpression(MirAggregate { identifier, value }) => {
                MiddleNodeType::AggregateExpression(MirAggregate {
                    identifier,
                    value: ObjectMap(
                        value
                            .0
                            .into_iter()
                            .map(|x| (x.0, x.1.rename(state)))
                            .collect(),
                    ),
                })
            }
            MiddleNodeType::Conditional(MirConditional {
                comparison,
                then,
                otherwise,
            }) => MiddleNodeType::Conditional(MirConditional {
                comparison: Box::new(comparison.rename(state)),
                then: Box::new(then.rename(state)),
                otherwise: otherwise.map(|value| Box::new(value.rename(state))),
            }),
        }
    }
}
