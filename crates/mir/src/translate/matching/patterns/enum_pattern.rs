use super::{BindingDeclaration, PatternTranslation, PatternTranslator};
use crate::{
    environment::MiddleEnvironment, errors::MiddleErr, scoping::ScopeId,
    symbols::resolve::ResolutionOptions, translate::matching::PatternTranslatorDispatcher,
};
use calibre_parser::ast::{
    matching::MatchArmType,
    nodes::AstNode,
    types::{ParserDataType, ParserInnerType},
};
use ustr::Ustr;

pub struct EnumPatternTranslator;

impl PatternTranslator for EnumPatternTranslator {
    fn translate(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        pattern: &MatchArmType,
        value: &AstNode,
    ) -> Result<PatternTranslation, MiddleErr> {
        let (inner_pattern, aliases) = pattern.clone().alias_bindings();

        let MatchArmType::Enum {
            value: variant_name,
            var_type,
            name,
            destructure,
            pattern: payload_pattern,
        } = inner_pattern
        else {
            return Err(env.context.err_at_current(MiddleErr::Internal(
                "EnumPatternCompiler called with non-enum pattern".to_string(),
            )));
        };

        let resolved_variant = env.resolve(
            scope,
            &variant_name,
            ResolutionOptions::default().with_dollar(),
        )?;

        let variant_index = env
            .enum_variant_index_from_value(scope, value, &resolved_variant)
            .ok_or_else(|| {
                env.context
                    .err_at_current(MiddleErr::CantMatch(Box::new(ParserDataType::new(
                        env.context.current_span(),
                        ParserInnerType::Auto(None),
                    ))))
            })?;

        let condition = env.discriminant_eq(value.clone(), variant_index);

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

        if let Some(payload_pattern) = payload_pattern {
            let payload_value = AstNode::member(env.context.current_span(), value.clone(), "next");

            if name.is_some() || destructure.is_some() {
                let name = match name {
                    Some(x) => {
                        env.resolve(scope, &x, ResolutionOptions::default().with_dollar())?
                    }
                    _ => Ustr::from("match_destructure"),
                };

                bindings.push(BindingDeclaration {
                    name,
                    value: payload_value.clone(),
                    var_type,
                    data_type: None,
                });
            }

            let payload_compilation = PatternTranslatorDispatcher::translate(
                env,
                scope,
                &payload_pattern,
                &payload_value,
            )?;

            bindings.extend(payload_compilation.bindings);

            let condition = env.bool_and_nodes(condition, payload_compilation.condition);

            Ok(PatternTranslation {
                condition,
                bindings,
            })
        } else if name.is_some() || destructure.is_some() {
            let name = match name {
                Some(x) => env.resolve(scope, &x, ResolutionOptions::default().with_dollar())?,
                _ => Ustr::from("match_destructure"),
            };

            let payload_value = AstNode::member(env.context.current_span(), value.clone(), "next");

            bindings.push(BindingDeclaration {
                name,
                value: payload_value,
                var_type,
                data_type: None,
            });

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
