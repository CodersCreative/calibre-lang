use super::{BindingDeclaration, PatternTranslation, PatternTranslator};
use crate::{
    ast::types::MirDataType,
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
    translate::{MirLowering, matching::PatternTranslatorDispatcher},
};
use calibre_parser::ast::{
    comparison::ComparisonOperator,
    nodes::{
        AstNode, AstNodeType,
        binary::AstComparison,
        functions::CallArg,
        literals::AstTuple,
        matching::{MatchArmType, RestPattern},
    },
    types::ParserDataType,
};

pub struct TuplePatternTranslator;

impl PatternTranslator for TuplePatternTranslator {
    fn translate(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        pattern: &MatchArmType,
        value: &AstNode,
        value_type: Option<&MirDataType>,
    ) -> Result<PatternTranslation, MiddleErr> {
        let (inner_pattern, aliases) = pattern.clone().alias_bindings();
        let mut span = *inner_pattern.span();

        let MatchArmType::TuplePattern(items) = inner_pattern else {
            return Err(env.context.err_at_span(
                span,
                MiddleErr::Internal(
                    "TuplePatternCompiler called with non-tuple pattern".to_string(),
                ),
            ));
        };

        let mut condition = AstNode::bool(span, true);

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
            .collect::<Result<Vec<_>, _>>()?;

        let mut idx = 0;
        for item in &items {
            let (inner_item, item_aliases) = item.clone().alias_bindings();
            span = *inner_item.span();

            match inner_item {
                MatchArmType::Rest(RestPattern::Discard(_)) => break,
                MatchArmType::Rest(RestPattern::Binding { var_type, name }) => {
                    let MirDataType::Tuple(value_types) = value_type.ok_or_else(|| {
                        env.context.err_at_span(
                            span,
                            MiddleErr::CantMatch(Box::new(ParserDataType::auto(span))),
                        )
                    })?
                    else {
                        break;
                    };

                    let values = (idx..value_types.len())
                        .into_iter()
                        .map(|i| AstNode::member(span, value.clone(), i.to_string()))
                        .collect();

                    let value = AstNode::new(span, AstNodeType::TupleLiteral(AstTuple { values }));

                    bindings.push(BindingDeclaration {
                        name: env
                            .resolve(scope, &name, ResolutionOptions::default().with_dollar())?
                            .unwrap_dollar(),
                        value: value.clone(),
                        var_type,
                        data_type: None,
                    });

                    for (var_type, name) in item_aliases {
                        let name = env
                            .resolve(scope, &name, ResolutionOptions::default().with_dollar())?
                            .unwrap_dollar();

                        bindings.push(BindingDeclaration {
                            name,
                            value: value.clone(),
                            var_type,
                            data_type: None,
                        });
                    }

                    break;
                }
                MatchArmType::Wildcard(_) => {
                    idx += 1;
                }
                MatchArmType::Let { var_type, name } => {
                    let current = AstNode::member(span, value.clone(), idx.to_string());

                    bindings.push(BindingDeclaration {
                        name: env
                            .resolve(scope, &name, ResolutionOptions::default().with_dollar())?
                            .unwrap_dollar(),
                        value: current,
                        var_type,
                        data_type: None,
                    });

                    let value = AstNode::member(span, value.clone(), idx.to_string());

                    bindings.append(
                        &mut item_aliases
                            .into_iter()
                            .map(|(var_type, name)| {
                                Ok(BindingDeclaration {
                                    name: env
                                        .resolve(
                                            scope,
                                            &name,
                                            ResolutionOptions::default().with_dollar(),
                                        )?
                                        .unwrap_dollar(),
                                    value: value.clone(),
                                    var_type,
                                    data_type: None,
                                })
                            })
                            .collect::<Result<_, MiddleErr>>()?,
                    );

                    idx += 1;
                }
                MatchArmType::Value(expected) => {
                    let current = AstNode::member(span, value.clone(), idx.to_string());

                    condition = env.bool_and_nodes(
                        condition,
                        AstNode::new(
                            span,
                            AstNodeType::ComparisonExpression(AstComparison {
                                left: Box::new(current.clone()),
                                right: Box::new(expected),
                                operator: ComparisonOperator::Equal,
                            }),
                        ),
                    );

                    bindings.append(
                        &mut item_aliases
                            .into_iter()
                            .map(|(var_type, name)| {
                                Ok(BindingDeclaration {
                                    name: env
                                        .resolve(
                                            scope,
                                            &name,
                                            ResolutionOptions::default().with_dollar(),
                                        )?
                                        .unwrap_dollar(),
                                    value: current.clone(),
                                    var_type,
                                    data_type: None,
                                })
                            })
                            .collect::<Result<_, MiddleErr>>()?,
                    );

                    idx += 1;
                }
                MatchArmType::StructPattern(struct_fields) => {
                    let current = AstNode::member(span, value.clone(), idx.to_string());

                    let struct_pattern = MatchArmType::StructPattern(struct_fields);

                    // TODO Extract the value type
                    let struct_compilation = PatternTranslatorDispatcher::translate(
                        env,
                        scope,
                        &struct_pattern,
                        &current,
                        None,
                    )?;

                    condition = env.bool_and_nodes(condition, struct_compilation.condition);

                    bindings.extend(struct_compilation.bindings);

                    bindings.append(
                        &mut item_aliases
                            .into_iter()
                            .map(|(var_type, name)| {
                                Ok(BindingDeclaration {
                                    name: env
                                        .resolve(
                                            scope,
                                            &name,
                                            ResolutionOptions::default().with_dollar(),
                                        )?
                                        .unwrap_dollar(),
                                    value: current.clone(),
                                    var_type,
                                    data_type: None,
                                })
                            })
                            .collect::<Result<_, MiddleErr>>()?,
                    );

                    idx += 1;
                }
                MatchArmType::Enum {
                    value: variant_name,
                    var_type,
                    name,
                    destructure,
                    pattern: payload_pattern,
                } => {
                    let current = AstNode::member(span, value.clone(), idx.to_string());

                    let enum_pattern = MatchArmType::Enum {
                        value: variant_name,
                        var_type,
                        name,
                        destructure,
                        pattern: payload_pattern,
                    };

                    // TODO Extract the value type
                    let enum_compilation = PatternTranslatorDispatcher::translate(
                        env,
                        scope,
                        &enum_pattern,
                        &current,
                        None,
                    )?;

                    condition = env.bool_and_nodes(condition, enum_compilation.condition);

                    bindings.extend(enum_compilation.bindings);

                    bindings.append(
                        &mut item_aliases
                            .into_iter()
                            .map(|(var_type, name)| {
                                Ok(BindingDeclaration {
                                    name: env
                                        .resolve(
                                            scope,
                                            &name,
                                            ResolutionOptions::default().with_dollar(),
                                        )?
                                        .unwrap_dollar(),
                                    value: current.clone(),
                                    var_type,
                                    data_type: None,
                                })
                            })
                            .collect::<Result<_, MiddleErr>>()?,
                    );

                    idx += 1;
                }
                MatchArmType::StringPattern(parts) => {
                    let current = AstNode::member(span, value.clone(), idx.to_string());

                    let string_pattern = MatchArmType::StringPattern(parts);

                    // TODO Extract the value type
                    let string_compilation = PatternTranslatorDispatcher::translate(
                        env,
                        scope,
                        &string_pattern,
                        &current,
                        None,
                    )?;

                    condition = env.bool_and_nodes(condition, string_compilation.condition);

                    bindings.extend(string_compilation.bindings);

                    bindings.append(
                        &mut item_aliases
                            .into_iter()
                            .map(|(var_type, name)| {
                                Ok(BindingDeclaration {
                                    name: env
                                        .resolve(
                                            scope,
                                            &name,
                                            ResolutionOptions::default().with_dollar(),
                                        )?
                                        .unwrap_dollar(),
                                    value: current.clone(),
                                    var_type,
                                    data_type: None,
                                })
                            })
                            .collect::<Result<_, MiddleErr>>()?,
                    );

                    idx += 1;
                }
                MatchArmType::IsType(_) | MatchArmType::In(_) => {
                    // TODO
                    idx += 1;
                }
                MatchArmType::ActivePattern { function, pattern } => {
                    span = function.span;

                    let current = AstNode::member(span, value.clone(), idx.to_string());

                    let Some(MirDataType::Function { return_type, .. }) =
                        function.type_of(env, scope, function.span)
                    else {
                        return Err(env.context.err_at_span(
                            span,
                            MiddleErr::Internal(
                                "ActivePatternTranslator called with non function".to_string(),
                            ),
                        ));
                    };

                    let result = AstNode::call(
                        span,
                        function.clone(),
                        vec![CallArg::Value(current.clone())],
                    );

                    let is_some = if return_type.is_option() {
                        AstNode::call(
                            span,
                            AstNode::member(span, result.clone(), "is_some"),
                            Vec::new(),
                        )
                    } else {
                        AstNode::call(
                            span,
                            AstNode::member(span, result.clone(), "is_ok"),
                            Vec::new(),
                        )
                    };

                    let unwrapped_value = if return_type.is_option() {
                        AstNode::member(span, result.clone(), "Some")
                    } else {
                        AstNode::member(span, result.clone(), "Ok")
                    };

                    let inner_translation = PatternTranslatorDispatcher::translate(
                        env,
                        scope,
                        pattern.as_ref(),
                        &unwrapped_value,
                        Some(
                            &return_type
                                .unwrap_one_result()
                                .cloned()
                                .unwrap_or_else(|| return_type.unwrap_one_option()),
                        ),
                    )?;

                    condition = env.bool_and_nodes(condition, is_some);

                    condition = env.bool_and_nodes(condition, inner_translation.condition);
                    bindings.extend(inner_translation.bindings);

                    idx += 1;
                }
                MatchArmType::At { .. } => {
                    return Err(env.context.err_at_span(
                        span,
                        MiddleErr::Internal(
                            "Unwrapped @ pattern still present in tuple".to_string(),
                        ),
                    ));
                }
                MatchArmType::ListPattern(_) | MatchArmType::TuplePattern(_) => {
                    return Err(env.context.err_at_span(
                        span,
                        MiddleErr::Internal(
                            "list or tuple pattern still present in tuple".to_string(),
                        ),
                    ));
                }
            }
        }

        Ok(PatternTranslation {
            condition,
            bindings,
        })
    }
}
