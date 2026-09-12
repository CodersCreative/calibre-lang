use super::patterns::{
    BindingDeclaration, PatternTranslation, PatternTranslator, enum_pattern::EnumPatternTranslator,
    list_pattern::ListPatternTranslator, string_pattern::StringPatternTranslator,
    struct_pattern::StructPatternTranslator, tuple_pattern::TuplePatternTranslator,
    value_pattern::ValuePatternTranslator,
};
use crate::{
    environment::MiddleEnvironment, errors::MiddleErr, scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
};
use calibre_parser::ast::{
    idents::ParserText,
    nodes::{
        AstNode, AstNodeType,
        binary::{AstIn, AstIs},
        matching::MatchArmType,
    },
    types::{ParserDataType, ParserInnerType},
};

pub struct PatternTranslatorDispatcher;

impl PatternTranslatorDispatcher {
    pub fn translate(
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        pattern: &MatchArmType,
        value: &AstNode,
    ) -> Result<PatternTranslation, MiddleErr> {
        let (inner_pattern, aliases) = pattern.clone().alias_bindings();

        let compilation = match &inner_pattern {
            MatchArmType::Value(_) | MatchArmType::Wildcard(_) => {
                ValuePatternTranslator.translate(env, scope, &inner_pattern, value)?
            }
            MatchArmType::Enum { .. } => {
                EnumPatternTranslator.translate(env, scope, &inner_pattern, value)?
            }
            MatchArmType::TuplePattern(_) => {
                TuplePatternTranslator.translate(env, scope, &inner_pattern, value)?
            }
            MatchArmType::ListPattern(_) => {
                ListPatternTranslator.translate(env, scope, &inner_pattern, value)?
            }
            MatchArmType::StructPattern(_) => {
                StructPatternTranslator.translate(env, scope, &inner_pattern, value)?
            }
            MatchArmType::StringPattern(_) => {
                StringPatternTranslator.translate(env, scope, &inner_pattern, value)?
            }
            MatchArmType::IsType(data_type) => {
                let condition = AstNode::new(
                    env.context.current_span(),
                    AstNodeType::IsExpression(AstIs {
                        value: Box::new(value.clone()),
                        data_type: data_type.clone(),
                    }),
                );

                let bindings = aliases
                    .into_iter()
                    .map(|(var_type, name)| {
                        Ok(BindingDeclaration {
                            name: env.resolve(
                                scope,
                                &name,
                                ResolutionOptions::default().with_dollar(),
                            )?,
                            value: value.clone(),
                            var_type,
                            data_type: None,
                        })
                    })
                    .collect::<Result<_, MiddleErr>>()?;

                PatternTranslation {
                    condition,
                    bindings,
                }
            }
            MatchArmType::In(in_value) => {
                let condition = AstNode::new(
                    env.context.current_span(),
                    AstNodeType::InDeclaration(AstIn {
                        identifier: Box::new(value.clone()),
                        value: Box::new(in_value.clone()),
                    }),
                );

                let bindings = aliases
                    .into_iter()
                    .map(|(var_type, name)| {
                        Ok(BindingDeclaration {
                            name: env.resolve(
                                scope,
                                &name,
                                ResolutionOptions::default().with_dollar(),
                            )?,
                            value: value.clone(),
                            var_type,
                            data_type: None,
                        })
                    })
                    .collect::<Result<_, MiddleErr>>()?;

                PatternTranslation {
                    condition,
                    bindings,
                }
            }
            MatchArmType::Let { var_type, name } => {
                let condition = AstNode::bool(env.context.current_span(), true);

                let mut bindings: Vec<BindingDeclaration> = aliases
                    .into_iter()
                    .map(|(var_type, name)| {
                        Ok(BindingDeclaration {
                            name: env.resolve(
                                scope,
                                &name,
                                ResolutionOptions::default().with_dollar(),
                            )?,
                            value: value.clone(),
                            var_type,
                            data_type: None,
                        })
                    })
                    .collect::<Result<_, MiddleErr>>()?;

                bindings.push(BindingDeclaration {
                    name: env.resolve(scope, name, ResolutionOptions::default().with_dollar())?,
                    value: value.clone(),
                    var_type: *var_type,
                    data_type: None,
                });

                PatternTranslation {
                    condition,
                    bindings,
                }
            }
            MatchArmType::At { .. } => {
                return Err(env.context.err_at_current(MiddleErr::Internal(
                    "Unwrapped @ pattern still present".to_string(),
                )));
            }
        };

        Ok(compilation)
    }

    pub fn bindings_to_decls(
        bindings: &[BindingDeclaration],
        env: &MiddleEnvironment,
    ) -> Vec<AstNode> {
        bindings
            .iter()
            .map(|binding| {
                AstNode::new(
                    env.context.current_span(),
                    AstNodeType::VariableDeclaration {
                        var_type: binding.var_type,
                        identifier: ParserText::new(env.context.current_span(), binding.name)
                            .into(),
                        value: Box::new(binding.value.clone()),
                        data_type: binding.data_type.clone().unwrap_or_else(|| {
                            ParserDataType::new(
                                env.context.current_span(),
                                ParserInnerType::Auto(None),
                            )
                        }),
                    },
                )
            })
            .collect()
    }
}
