use crate::{
    ast::types::MirDataType,
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
    translate::matching::{
        ExhaustivenessReport,
        exhaustiveness::{ExhaustivenessChecker, WildcardChecker},
    },
    typing::MiddleTypeDefType,
};
use calibre_parser::ast::nodes::matching::{MatchArmType, MatchStructFieldPattern};
use ustr::{Ustr, UstrSet};

pub struct EnumExhaustivenessChecker;

impl ExhaustivenessChecker for EnumExhaustivenessChecker {
    fn check(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        patterns: &[MatchArmType],
        data_type: &MirDataType,
    ) -> Result<ExhaustivenessReport, MiddleErr> {
        let unwrapped_type = data_type.unwrap_all_refs();

        let enum_key = match &unwrapped_type {
            MirDataType::Struct { identifier, .. } => identifier,
            _ => {
                return Ok(ExhaustivenessReport::exhaustive());
            }
        };

        let enum_def = match env.typing.objects.get(enum_key) {
            Some(obj) => obj,
            None => {
                return Ok(ExhaustivenessReport::exhaustive());
            }
        };

        let variants = match &enum_def.object_type {
            MiddleTypeDefType::Enum { variants, .. } => variants.clone(),
            _ => {
                return Ok(ExhaustivenessReport::exhaustive());
            }
        };

        if WildcardChecker::has_wildcard_in_patterns(patterns) {
            return Ok(ExhaustivenessReport::exhaustive());
        }

        let mut covered_variants = UstrSet::default();
        let mut has_guards = false;

        for pattern in patterns {
            let (inner_pattern, _aliases) = pattern.clone().alias_bindings();

            match inner_pattern {
                MatchArmType::Enum { value, .. } => {
                    covered_variants.insert(
                        env.resolve(scope, &value, ResolutionOptions::default().with_dollar())?
                            .unwrap_dollar(),
                    );
                }
                _ => {
                    has_guards = true;
                }
            }
        }

        let missing_variants: Vec<String> = variants
            .iter()
            .filter(|(name, _)| !covered_variants.contains(name))
            .map(|(name, _)| format!(".{}", name))
            .collect();

        if has_guards && !missing_variants.is_empty() {
            return Ok(ExhaustivenessReport::requires_wildcard(
                "add a `_` pattern".to_string(),
            ));
        }

        if missing_variants.is_empty() {
            Ok(ExhaustivenessReport::exhaustive())
        } else {
            Ok(ExhaustivenessReport::non_exhaustive(missing_variants))
        }
    }
}

pub struct StructExhaustivenessChecker;

impl ExhaustivenessChecker for StructExhaustivenessChecker {
    fn check(
        &self,
        env: &mut MiddleEnvironment,
        _scope: ScopeId,
        patterns: &[MatchArmType],
        data_type: &MirDataType,
    ) -> Result<ExhaustivenessReport, MiddleErr> {
        let unwrapped_type = data_type.unwrap_all_refs();

        let struct_key = match &unwrapped_type {
            MirDataType::Struct { identifier, .. } => identifier,
            _ => {
                return Ok(ExhaustivenessReport::exhaustive());
            }
        };

        if WildcardChecker::has_wildcard_in_patterns(patterns) {
            return Ok(ExhaustivenessReport::exhaustive());
        }

        let struct_def = match env.typing.objects.get(struct_key) {
            Some(obj) => obj,
            None => {
                return Ok(ExhaustivenessReport::requires_wildcard(
                    "struct type not known, add a `_` pattern".to_string(),
                ));
            }
        };

        let fields = match &struct_def.object_type {
            MiddleTypeDefType::Struct(fields) => fields.clone(),
            _ => {
                return Ok(ExhaustivenessReport::requires_wildcard(
                    "not a struct type, add a `_` pattern".to_string(),
                ));
            }
        };

        let mut has_bindings = false;
        let mut has_infinite_field_patterns = false;
        let mut has_guards = false;

        for pattern in patterns {
            let (inner_pattern, _aliases) = pattern.clone().alias_bindings();

            match inner_pattern {
                MatchArmType::StructPattern(field_patterns) => {
                    for field_pattern in &field_patterns {
                        match field_pattern {
                            MatchStructFieldPattern::Binding { .. } => {
                                has_bindings = true;
                            }
                            MatchStructFieldPattern::Value { .. }
                            | MatchStructFieldPattern::AlternativeValues { .. } => {
                                if let Some(field_name) = field_pattern.field_name()
                                    && let Some((_, (field_type, _))) =
                                        fields.0.iter().find(|(name, _)| *name == field_name)
                                    && field_type.is_infinite()
                                {
                                    has_infinite_field_patterns = true;
                                }
                            }
                        }
                    }
                }
                MatchArmType::Wildcard(_) => {
                    return Ok(ExhaustivenessReport::exhaustive());
                }
                _ => {
                    has_guards = true;
                }
            }
        }

        // TODO Do guard exhaustiveness checking
        if has_guards {
            return Ok(ExhaustivenessReport::requires_wildcard(
                "add a `_` pattern".to_string(),
            ));
        }

        if has_bindings {
            for pattern in patterns {
                let (inner_pattern, _aliases) = pattern.clone().alias_bindings();
                if let MatchArmType::StructPattern(field_patterns) = inner_pattern {
                    let bound_fields: Vec<Ustr> = field_patterns
                        .iter()
                        .filter_map(|fp| fp.field_name().map(|x| Ustr::from(x.as_str())))
                        .collect();

                    if bound_fields.len() == fields.0.len() {
                        return Ok(ExhaustivenessReport::exhaustive());
                    }
                }
            }
        }

        if has_infinite_field_patterns {
            return Ok(ExhaustivenessReport::requires_wildcard(
                "struct has fields with infinite possibilities, add a `_` pattern".to_string(),
            ));
        }

        if has_bindings {
            return Ok(ExhaustivenessReport::requires_wildcard(
                "add a `_` pattern".to_string(),
            ));
        }

        Ok(ExhaustivenessReport::requires_wildcard(
            "cannot prove that all fielda are covered, add a `_` pattern".to_string(),
        ))
    }
}
