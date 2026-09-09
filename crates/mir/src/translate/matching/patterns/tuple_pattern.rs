use super::{BindingDeclaration, PatternTranslation, PatternTranslator};
use crate::{
    environment::MiddleEnvironment, errors::MiddleErr, scoping::ScopeId,
    symbols::resolve::ResolutionOptions, translate::matching::PatternTranslatorDispatcher,
};
use calibre_parser::ast::{
    comparison::ComparisonOperator,
    matching::{MatchArmType, MatchTupleItem},
    nodes::{AstNode, AstNodeType},
};

pub struct TuplePatternTranslator;

impl PatternTranslator for TuplePatternTranslator {
    fn translate(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        pattern: &MatchArmType,
        value: &AstNode,
    ) -> Result<PatternTranslation, MiddleErr> {
        let (inner_pattern, aliases) = pattern.clone().alias_bindings();

        let MatchArmType::TuplePattern(items) = inner_pattern else {
            return Err(env.context.err_at_current(MiddleErr::Internal(
                "TuplePatternCompiler called with non-tuple pattern".to_string(),
            )));
        };

        let mut condition = AstNode::bool(env.context.current_span(), true);

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
            .collect::<Result<Vec<_>, _>>()?;

        let mut idx = 0;
        for item in items {
            let (inner_item, item_aliases) = item.alias_bindings();

            match inner_item {
                MatchTupleItem::Rest(_) => break,
                MatchTupleItem::Wildcard(_) => {
                    idx += 1;
                }
                MatchTupleItem::Binding { var_type, name } => {
                    let current =
                        AstNode::member(env.context.current_span(), value.clone(), idx.to_string());

                    bindings.push(BindingDeclaration {
                        name: env.resolve(
                            scope,
                            &name,
                            ResolutionOptions::default().with_dollar(),
                        )?,
                        value: current,
                        var_type,
                        data_type: None,
                    });

                    bindings.append(
                        &mut item_aliases
                            .into_iter()
                            .map(|(var_type, name)| {
                                Ok(BindingDeclaration {
                                    name: env.resolve(
                                        scope,
                                        &name,
                                        ResolutionOptions::default().with_dollar(),
                                    )?,
                                    value: AstNode::member(
                                        env.context.current_span(),
                                        value.clone(),
                                        idx.to_string(),
                                    ),
                                    var_type,
                                    data_type: None,
                                })
                            })
                            .collect::<Result<_, MiddleErr>>()?,
                    );

                    idx += 1;
                }
                MatchTupleItem::Value(expected) => {
                    let current =
                        AstNode::member(env.context.current_span(), value.clone(), idx.to_string());

                    condition = env.bool_and_nodes(
                        condition,
                        AstNode::new(
                            env.context.current_span(),
                            AstNodeType::ComparisonExpression {
                                left: Box::new(current),
                                right: Box::new(expected),
                                operator: ComparisonOperator::Equal,
                            },
                        ),
                    );

                    bindings.append(
                        &mut item_aliases
                            .into_iter()
                            .map(|(var_type, name)| {
                                Ok(BindingDeclaration {
                                    name: env.resolve(
                                        scope,
                                        &name,
                                        ResolutionOptions::default().with_dollar(),
                                    )?,
                                    value: AstNode::member(
                                        env.context.current_span(),
                                        value.clone(),
                                        idx.to_string(),
                                    ),
                                    var_type,
                                    data_type: None,
                                })
                            })
                            .collect::<Result<_, MiddleErr>>()?,
                    );

                    idx += 1;
                }
                MatchTupleItem::StructPattern(struct_fields) => {
                    let current =
                        AstNode::member(env.context.current_span(), value.clone(), idx.to_string());

                    let struct_pattern = MatchArmType::StructPattern(struct_fields);
                    let struct_compilation = PatternTranslatorDispatcher::translate(
                        env,
                        scope,
                        &struct_pattern,
                        &current,
                    )?;

                    condition = env.bool_and_nodes(condition, struct_compilation.condition);

                    bindings.extend(struct_compilation.bindings);

                    bindings.append(
                        &mut item_aliases
                            .into_iter()
                            .map(|(var_type, name)| {
                                Ok(BindingDeclaration {
                                    name: env.resolve(
                                        scope,
                                        &name,
                                        ResolutionOptions::default().with_dollar(),
                                    )?,
                                    value: current.clone(),
                                    var_type,
                                    data_type: None,
                                })
                            })
                            .collect::<Result<_, MiddleErr>>()?,
                    );

                    idx += 1;
                }
                MatchTupleItem::Enum {
                    value: variant_name,
                    var_type,
                    name,
                    destructure,
                    pattern: payload_pattern,
                } => {
                    let current =
                        AstNode::member(env.context.current_span(), value.clone(), idx.to_string());

                    let enum_pattern = MatchArmType::Enum {
                        value: variant_name,
                        var_type,
                        name,
                        destructure,
                        pattern: payload_pattern,
                    };

                    let enum_compilation = PatternTranslatorDispatcher::translate(
                        env,
                        scope,
                        &enum_pattern,
                        &current,
                    )?;

                    condition = env.bool_and_nodes(condition, enum_compilation.condition);

                    bindings.extend(enum_compilation.bindings);

                    bindings.append(
                        &mut item_aliases
                            .into_iter()
                            .map(|(var_type, name)| {
                                Ok(BindingDeclaration {
                                    name: env.resolve(
                                        scope,
                                        &name,
                                        ResolutionOptions::default().with_dollar(),
                                    )?,
                                    value: current.clone(),
                                    var_type,
                                    data_type: None,
                                })
                            })
                            .collect::<Result<_, MiddleErr>>()?,
                    );

                    idx += 1;
                }
                MatchTupleItem::StringPattern(parts) => {
                    let current =
                        AstNode::member(env.context.current_span(), value.clone(), idx.to_string());

                    let string_pattern = MatchArmType::StringPattern(parts);
                    let string_compilation = PatternTranslatorDispatcher::translate(
                        env,
                        scope,
                        &string_pattern,
                        &current,
                    )?;

                    condition = env.bool_and_nodes(condition, string_compilation.condition);

                    bindings.extend(string_compilation.bindings);

                    bindings.append(
                        &mut item_aliases
                            .into_iter()
                            .map(|(var_type, name)| {
                                Ok(BindingDeclaration {
                                    name: env.resolve(
                                        scope,
                                        &name,
                                        ResolutionOptions::default().with_dollar(),
                                    )?,
                                    value: current.clone(),
                                    var_type,
                                    data_type: None,
                                })
                            })
                            .collect::<Result<_, MiddleErr>>()?,
                    );

                    idx += 1;
                }
                MatchTupleItem::IsType(_) | MatchTupleItem::In(_) => {
                    // TODO
                    idx += 1;
                }
                MatchTupleItem::At { .. } => {
                    return Err(env.context.err_at_current(MiddleErr::Internal(
                        "Unwrapped @ pattern still present in tuple".to_string(),
                    )));
                }
            }
        }

        Ok(PatternTranslation {
            condition,
            bindings,
        })
    }
}
