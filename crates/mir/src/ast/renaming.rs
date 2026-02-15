use calibre_parser::ast::{ObjectMap, ParserText};
use rand::random_range;
use rustc_hash::FxHashMap;

use crate::{MiddleNode, MiddleNodeType};

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

impl MiddleNodeType {
    pub fn rename(self, state: &mut AlphaRenameState) -> Self {
        match self {
            MiddleNodeType::Break {
                label: _,
                value: None,
            }
            | MiddleNodeType::Continue { label: _ }
            | MiddleNodeType::EmptyLine
            | MiddleNodeType::Null
            | MiddleNodeType::EnumExpression { data: None, .. }
            | MiddleNodeType::CharLiteral(_)
            | MiddleNodeType::FloatLiteral(_)
            | MiddleNodeType::IntLiteral(_)
            | MiddleNodeType::StringLiteral(_)
            | MiddleNodeType::ExternFunction { .. } => self,
            MiddleNodeType::Break {
                label,
                value: Some(value),
            } => MiddleNodeType::Break {
                label,
                value: Some(Box::new(value.rename(state))),
            },
            MiddleNodeType::Spawn { value } => MiddleNodeType::Spawn {
                value: Box::new(value.rename(state)),
            },
            MiddleNodeType::RefStatement { mutability, value } => MiddleNodeType::RefStatement {
                mutability,
                value: Box::new(value.rename(state)),
            },
            MiddleNodeType::DerefStatement { value } => MiddleNodeType::DerefStatement {
                value: Box::new(value.rename(state)),
            },
            MiddleNodeType::Drop(x) => {
                let name = if let Some(x) = state.data.get(&x.text) {
                    x.clone()
                } else {
                    x.text
                };

                MiddleNodeType::Drop(name.into())
            }
            MiddleNodeType::Move(x) => {
                let name = if let Some(x) = state.data.get(&x.text) {
                    x.clone()
                } else {
                    x.text
                };

                MiddleNodeType::Move(name.into())
            }
            MiddleNodeType::VariableDeclaration {
                var_type,
                identifier,
                value,
                data_type,
            } => {
                let new_name = format!("{}->{}", identifier.text, random_range(0..10000000));
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
                        let new_name = format!("{}->{}", x.0.text, random_range(0..10000000));
                        state.data.insert(x.0.text, new_name.clone());

                        (
                            ParserText {
                                text: new_name,
                                span: x.0.span,
                            },
                            x.1,
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
            MiddleNodeType::AsExpression { value, data_type } => MiddleNodeType::AsExpression {
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
            MiddleNodeType::Identifier(x) => {
                let name = if let Some(x) = state.data.get(&x.text) {
                    x.clone()
                } else {
                    x.text
                };

                MiddleNodeType::Identifier(ParserText {
                    text: name,
                    span: x.span,
                })
            }
            MiddleNodeType::ListLiteral(data_type, x) => MiddleNodeType::ListLiteral(
                data_type,
                x.into_iter().map(|x| x.rename(state)).collect(),
            ),
            MiddleNodeType::MemberExpression { mut path } => {
                path[0].0 = path[0].0.clone().rename(state);

                path = path
                    .into_iter()
                    .map(|x| if x.1 { (x.0.rename(state), x.1) } else { x })
                    .collect();

                MiddleNodeType::MemberExpression { path }
            }
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
            MiddleNodeType::IfStatement {
                comparison,
                then,
                otherwise,
            } => MiddleNodeType::IfStatement {
                comparison: Box::new(comparison.rename(state)),
                then: Box::new(then.rename(state)),
                otherwise: otherwise.map(|value| Box::new(value.rename(state))),
            },
        }
    }
}
