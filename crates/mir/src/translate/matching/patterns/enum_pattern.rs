use super::{BindingDeclaration, PatternTranslation, PatternTranslator};
use crate::{
    ast::types::MirDataType, environment::MiddleEnvironment, errors::MiddleErr, scoping::ScopeId,
    symbols::resolve::ResolutionOptions, translate::matching::PatternTranslatorDispatcher,
};
use calibre_parser::ast::{
    nodes::{AstNode, matching::MatchArmType},
    types::ParserDataType,
};

pub struct EnumPatternTranslator;

impl PatternTranslator for EnumPatternTranslator {
    fn translate(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        pattern: &MatchArmType,
        value: &AstNode,
        value_type: Option<&MirDataType>,
    ) -> Result<PatternTranslation, MiddleErr> {
        let (inner_pattern, aliases) = pattern.clone().alias_bindings();
        let span = *inner_pattern.span();

        let MatchArmType::Enum {
            value: variant_name,
            pattern,
        } = inner_pattern
        else {
            return Err(env.context.err_at_span(
                span,
                MiddleErr::Internal("EnumPatternCompiler called with non-enum pattern".to_string()),
            ));
        };

        let resolved_variant = env
            .resolve(
                scope,
                &variant_name,
                ResolutionOptions::default().with_dollar(),
            )?
            .unwrap_dollar();

        let variant_index = if let Some(value_type) = value_type {
            env.enum_variant_index_from_data_type(value_type, &resolved_variant)
                .ok_or_else(|| {
                    env.context.err_at_span(
                        span,
                        MiddleErr::CantMatch(Box::new(ParserDataType::auto(span))),
                    )
                })?
        } else {
            env.enum_variant_index_from_value(scope, value, &resolved_variant)
                .ok_or_else(|| {
                    env.context.err_at_span(
                        span,
                        MiddleErr::CantMatch(Box::new(ParserDataType::auto(span))),
                    )
                })?
        };

        let condition = env.discriminant_eq(value.clone(), variant_index);

        let mut bindings = aliases
            .into_iter()
            .map(|(var_type, name)| {
                Ok(BindingDeclaration {
                    name: env
                        .resolve(scope, &name, ResolutionOptions::default().with_dollar())?
                        .unwrap_dollar(),
                    value: value.clone(),
                    var_type,
                    data_type: value_type.cloned().map(|x| x.into()),
                })
            })
            .collect::<Result<Vec<_>, MiddleErr>>()?;

        let payload_type =
            value_type.and_then(|x| env.resolve_member_field_type(x, &resolved_variant));

        let payload_value = AstNode::member(span, value.clone(), resolved_variant);

        if let Some(pattern) = pattern {
            let inner_translation = PatternTranslatorDispatcher::translate(
                env,
                scope,
                &pattern,
                &payload_value,
                payload_type.as_ref(),
            )?;

            let condition = env.bool_and_nodes(condition, inner_translation.condition);
            bindings.extend(inner_translation.bindings);

            Ok(PatternTranslation {
                condition,
                bindings,
            })
        } else {
            Ok(PatternTranslation {
                condition,
                bindings,
            })
        }
    }
}
