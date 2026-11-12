use super::patterns::BindingDeclaration;
use crate::{
    environment::MiddleEnvironment, errors::MiddleErr, scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
};
use calibre_parser::ast::{
    nodes::{
        AstNode, AstNodeType,
        access::AstIndex,
        functions::CallArg,
        literals::AstRange,
        matching::{MatchArmType, MatchStringPatternPart, MatchStructFieldPattern, RestPattern},
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
        let mut span = *inner_pattern.span();

        let mut bindings = aliases
            .into_iter()
            .map(|(var_type, name)| {
                Ok(BindingDeclaration {
                    name: env
                        .resolve(scope, &name, ResolutionOptions::default().with_dollar())?
                        .unwrap_dollar(),
                    value: value.clone(),
                    var_type,
                    data_type: None,
                })
            })
            .collect::<Result<Vec<_>, MiddleErr>>()?;

        match &inner_pattern {
            MatchArmType::Let { var_type, name } => {
                bindings.push(BindingDeclaration {
                    name: env
                        .resolve(scope, name, ResolutionOptions::default().with_dollar())?
                        .unwrap_dollar(),
                    value: value.clone(),
                    var_type: *var_type,
                    data_type: None,
                });
            }
            MatchArmType::ActivePattern { function, pattern } => {
                let value =
                    AstNode::call(span, function.clone(), vec![CallArg::Value(value.clone())]);

                let value = AstNode::member(span, value.clone(), "Some");

                bindings.extend(Self::extract_bindings(env, scope, pattern, &value)?);
            }
            MatchArmType::TuplePattern(items) => {
                for (idx, item) in items.iter().enumerate() {
                    span = *item.span();

                    let value = AstNode::member(span, value.clone(), idx.to_string());

                    bindings.extend(Self::extract_bindings(env, scope, item, &value)?);
                }
            }
            MatchArmType::ListPattern(items) => {
                let rest_index = items
                    .iter()
                    .position(|item| matches!(item, MatchArmType::Rest(_)));

                for (idx, item) in items.iter().enumerate() {
                    if let Some(rest_index) = rest_index
                        && idx >= rest_index
                    {
                        span = *item.span();

                        if let MatchArmType::Rest(RestPattern::Binding { var_type, name }) = item {
                            let value = AstNode::new(
                                span,
                                AstNodeType::IndexAccess(AstIndex {
                                    base: Box::new(value.clone()),
                                    index: Box::new(AstNode::new(
                                        span,
                                        AstNodeType::RangeDeclaration(AstRange {
                                            from: Box::new(AstNode::int(span, rest_index)),
                                            to: Box::new(AstNode::call(
                                                span,
                                                AstNode::identifier(span, "len"),
                                                vec![CallArg::Value(value.clone())],
                                            )),
                                            inclusive: false,
                                        }),
                                    )),
                                    panic: true,
                                }),
                            );

                            bindings.push(BindingDeclaration {
                                name: env
                                    .resolve(
                                        scope,
                                        name,
                                        ResolutionOptions::default().with_dollar(),
                                    )?
                                    .unwrap_dollar(),
                                value,
                                var_type: *var_type,
                                data_type: None,
                            });
                        }

                        break;
                    }

                    let value = env.match_index_access(value.clone(), idx);
                    bindings.extend(Self::extract_bindings(env, scope, item, &value)?);
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
                            span = *name.span();

                            bindings.push(BindingDeclaration {
                                name: env
                                    .resolve(scope, name, ResolutionOptions::default())?
                                    .unwrap_dollar(),
                                value: AstNode::member(span, value.clone(), field_name.clone()),
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
                            span = *name.span();

                            bindings.push(BindingDeclaration {
                                name: env
                                    .resolve(
                                        scope,
                                        name,
                                        ResolutionOptions::default().with_dollar(),
                                    )?
                                    .unwrap_dollar(),
                                value: value.clone(),
                                var_type: *var_type,
                                data_type: Some(ParserDataType::new(span, ParserInnerType::Str)),
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
                value: variant,
            } => {
                if name.is_some() || destructure.is_some() || payload_pattern.is_some() {
                    let name = match name {
                        Some(x) => env
                            .resolve(scope, x, ResolutionOptions::default().with_dollar())?
                            .unwrap_dollar(),
                        _ => Ustr::from("match_destructure"),
                    };

                    let variant = env
                        .resolve(scope, variant, ResolutionOptions::default().with_dollar())?
                        .unwrap_dollar();

                    let payload_value = AstNode::member(span, value.clone(), variant);

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
                            &AstNode::member(span, value.clone(), variant),
                        )?);
                    }
                }
            }
            MatchArmType::Value(_)
            | MatchArmType::Rest(_)
            | MatchArmType::Wildcard(_)
            | MatchArmType::IsType(_)
            | MatchArmType::In(_) => {}
            MatchArmType::At { .. } => {
                unreachable!()
            }
        }

        Ok(bindings)
    }
}
