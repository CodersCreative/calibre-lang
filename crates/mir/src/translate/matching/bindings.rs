use super::patterns::BindingDeclaration;
use crate::{
    environment::MiddleEnvironment, errors::MiddleErr, scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
};
use calibre_parser::ast::{
    nodes::{
        AstNode,
        matching::{MatchArmType, MatchStringPatternPart, MatchStructFieldPattern, MatchTupleItem},
    },
    types::{ParserDataType, ParserInnerType},
};
use ustr::Ustr;

pub struct BindingExtractor;

impl BindingExtractor {
    pub fn extract_bindings(
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        pattern: &MatchArmType,
        value: &AstNode,
    ) -> Result<Vec<BindingDeclaration>, MiddleErr> {
        let (inner_pattern, aliases) = pattern.clone().alias_bindings();
        let mut bindings = aliases
            .into_iter()
            .map(|(var_type, name)| {
                Ok(BindingDeclaration {
                    name: env.resolve(scope, &name, ResolutionOptions::default().with_dollar())?,
                    value: value.clone(),
                    var_type,
                    data_type: None,
                })
            })
            .collect::<Result<Vec<_>, MiddleErr>>()?;

        match &inner_pattern {
            MatchArmType::Let { var_type, name } => {
                bindings.push(BindingDeclaration {
                    name: env.resolve(scope, name, ResolutionOptions::default().with_dollar())?,
                    value: value.clone(),
                    var_type: *var_type,
                    data_type: None,
                });
            }
            MatchArmType::TuplePattern(items) => {
                for (idx, item) in items.iter().enumerate() {
                    bindings.extend(Self::extract_tuple_item_bindings(
                        env,
                        scope,
                        item,
                        AstNode::member(env.context.current_span(), value.clone(), idx.to_string()),
                    )?);
                }
            }
            MatchArmType::ListPattern(items) => {
                for (idx, item) in items.iter().enumerate() {
                    if matches!(item, MatchTupleItem::Rest(_)) {
                        break;
                    }

                    bindings.extend(Self::extract_tuple_item_bindings(
                        env,
                        scope,
                        item,
                        env.match_index_access(value.clone(), idx),
                    )?);
                }
            }
            MatchArmType::StructPattern(fields) => {
                for field in fields {
                    match field {
                        MatchStructFieldPattern::Binding {
                            field: field_name,
                            var_type,
                            name,
                        } => {
                            bindings.push(BindingDeclaration {
                                name: env.resolve(scope, name, ResolutionOptions::default())?,
                                value: AstNode::member(
                                    env.context.current_span(),
                                    value.clone(),
                                    field_name.clone(),
                                ),
                                var_type: *var_type,
                                data_type: None,
                            });
                        }
                        MatchStructFieldPattern::Value { .. } => {}
                        MatchStructFieldPattern::AlternativeValues { .. } => {}
                    }
                }
            }
            MatchArmType::StringPattern(parts) => {
                for part in parts {
                    match part {
                        MatchStringPatternPart::Binding { var_type, name } => {
                            bindings.push(BindingDeclaration {
                                name: env.resolve(
                                    scope,
                                    name,
                                    ResolutionOptions::default().with_dollar(),
                                )?,
                                value: value.clone(),
                                var_type: *var_type,
                                data_type: Some(ParserDataType::new(
                                    env.context.current_span(),
                                    ParserInnerType::Str,
                                )),
                            });
                        }
                        MatchStringPatternPart::Literal(_)
                        | MatchStringPatternPart::Wildcard(_) => {}
                    }
                }
            }
            MatchArmType::Enum {
                var_type,
                name,
                destructure,
                pattern: payload_pattern,
                ..
            } => {
                if name.is_some() || destructure.is_some() || payload_pattern.is_some() {
                    let name = match name {
                        Some(x) => {
                            env.resolve(scope, x, ResolutionOptions::default().with_dollar())?
                        }
                        _ => Ustr::from("match_destructure"),
                    };

                    let payload_value =
                        AstNode::member(env.context.current_span(), value.clone(), "next");

                    bindings.push(BindingDeclaration {
                        name,
                        value: payload_value,
                        var_type: *var_type,
                        data_type: None,
                    });

                    if let Some(payload_pattern) = payload_pattern {
                        bindings.extend(Self::extract_bindings(
                            env,
                            scope,
                            payload_pattern,
                            &AstNode::member(env.context.current_span(), value.clone(), "next"),
                        )?);
                    }
                }
            }
            MatchArmType::Value(_)
            | MatchArmType::Wildcard(_)
            | MatchArmType::IsType(_)
            | MatchArmType::In(_) => {}
            MatchArmType::At { .. } => {
                unreachable!()
            }
        }

        Ok(bindings)
    }

    fn extract_tuple_item_bindings(
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        item: &MatchTupleItem,
        value: AstNode,
    ) -> Result<Vec<BindingDeclaration>, MiddleErr> {
        let (inner_item, aliases) = item.clone().alias_bindings();

        let mut bindings = aliases
            .into_iter()
            .map(|(var_type, name)| {
                Ok(BindingDeclaration {
                    name: env.resolve(scope, &name, ResolutionOptions::default().with_dollar())?,
                    value: value.clone(),
                    var_type,
                    data_type: None,
                })
            })
            .collect::<Result<Vec<_>, MiddleErr>>()?;

        match inner_item {
            MatchTupleItem::Binding { var_type, name } => {
                bindings.push(BindingDeclaration {
                    name: env.resolve(scope, &name, ResolutionOptions::default().with_dollar())?,
                    value,
                    var_type,
                    data_type: None,
                });
            }
            MatchTupleItem::Enum {
                var_type,
                name,
                destructure,
                pattern: payload_pattern,
                ..
            } => {
                if name.is_some() || destructure.is_some() || payload_pattern.is_some() {
                    let name = match name {
                        Some(x) => {
                            env.resolve(scope, &x, ResolutionOptions::default().with_dollar())?
                        }
                        _ => Ustr::from("match_destructure"),
                    };

                    let payload_value =
                        AstNode::member(env.context.current_span(), value.clone(), "next");

                    bindings.push(BindingDeclaration {
                        name,
                        value: payload_value,
                        var_type,
                        data_type: None,
                    });

                    if let Some(payload_pattern) = payload_pattern {
                        bindings.extend(Self::extract_bindings(
                            env,
                            scope,
                            &payload_pattern,
                            &AstNode::member(env.context.current_span(), value.clone(), "next"),
                        )?);
                    }
                }
            }
            MatchTupleItem::StructPattern(_) => {
                // TODO
            }
            MatchTupleItem::Rest(_)
            | MatchTupleItem::Wildcard(_)
            | MatchTupleItem::Value(_)
            | MatchTupleItem::IsType(_)
            | MatchTupleItem::In(_)
            | MatchTupleItem::StringPattern(_) => {}
            MatchTupleItem::At { .. } => {
                unreachable!()
            }
        }

        Ok(bindings)
    }
}
