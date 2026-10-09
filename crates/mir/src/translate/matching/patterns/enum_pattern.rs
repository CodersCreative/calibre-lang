use super::{BindingDeclaration, PatternTranslation, PatternTranslator};
use crate::{
    ast::types::MirDataType, environment::MiddleEnvironment, errors::MiddleErr, scoping::ScopeId,
    symbols::resolve::ResolutionOptions, translate::matching::PatternTranslatorDispatcher,
};
use calibre_parser::ast::{
    nodes::{AstNode, matching::MatchArmType},
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
        value_type: Option<&MirDataType>,
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
                    env.context
                        .err_at_current(MiddleErr::CantMatch(Box::new(ParserDataType::new(
                            env.context.current_span(),
                            ParserInnerType::Auto(None),
                        ))))
                })?
        } else {
            env.enum_variant_index_from_value(scope, value, &resolved_variant)
                .ok_or_else(|| {
                    env.context
                        .err_at_current(MiddleErr::CantMatch(Box::new(ParserDataType::new(
                            env.context.current_span(),
                            ParserInnerType::Auto(None),
                        ))))
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

        let member = match resolved_variant.as_str() {
            "Ok" => "ok",
            "Err" => "err",
            _ => "next",
        };

        let payload_type =
            value_type.and_then(|x| env.resolve_member_field_type(x, &Ustr::from(member)));

        let payload_value = AstNode::member(env.context.current_span(), value.clone(), member);

        if let Some(payload_pattern) = payload_pattern {
            if name.is_some() || destructure.is_some() {
                let name = match name {
                    Some(x) => env
                        .resolve(scope, &x, ResolutionOptions::default().with_dollar())?
                        .unwrap_dollar(),
                    _ => Ustr::from("match_destructure"),
                };

                bindings.push(BindingDeclaration {
                    name,
                    value: payload_value.clone(),
                    var_type,
                    data_type: payload_type.clone().map(ParserDataType::from),
                });
            }

            // TODO Extract the value type
            let payload_compilation = PatternTranslatorDispatcher::translate(
                env,
                scope,
                &payload_pattern,
                &payload_value,
                payload_type.as_ref(),
            )?;

            bindings.extend(payload_compilation.bindings);

            let condition = env.bool_and_nodes(condition, payload_compilation.condition);

            Ok(PatternTranslation {
                condition,
                bindings,
            })
        } else if name.is_some() || destructure.is_some() {
            let name = match name {
                Some(x) => env
                    .resolve(scope, &x, ResolutionOptions::default().with_dollar())?
                    .unwrap_dollar(),
                _ => Ustr::from("match_destructure"),
            };

            bindings.push(BindingDeclaration {
                name,
                value: payload_value,
                var_type,
                data_type: payload_type.map(ParserDataType::from),
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
