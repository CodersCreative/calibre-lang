use crate::{
    ast::types::MirDataType,
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    translate::matching::{
        ExhaustivenessReport,
        exhaustiveness::{ExhaustivenessChecker, WildcardChecker},
    },
};
use calibre_parser::ast::nodes::matching::MatchArmType;
use rustc_hash::FxHashSet;
use ustr::Ustr;

pub struct ResultExhaustivenessChecker;

impl ExhaustivenessChecker for ResultExhaustivenessChecker {
    fn check(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        patterns: &[MatchArmType],
        data_type: &MirDataType,
    ) -> Result<ExhaustivenessReport, MiddleErr> {
        if !data_type.unwrap_all_refs().is_result() {
            return Ok(ExhaustivenessReport::exhaustive());
        }

        if WildcardChecker::has_wildcard_in_patterns(patterns) {
            return Ok(ExhaustivenessReport::exhaustive());
        }

        let mut covered_variants = FxHashSet::default();
        let mut has_guards = false;

        for pattern in patterns {
            let (inner_pattern, _aliases) = pattern.clone().alias_bindings();

            match inner_pattern {
                MatchArmType::Enum { value, .. } => {
                    let variant_str = value.text();
                    covered_variants.insert(Ustr::from(variant_str));
                }
                _ => {
                    has_guards = true;
                }
            }
        }

        let missing_variants: Vec<String> = ["Ok", "Err"]
            .iter()
            .filter(|name| !covered_variants.contains(&Ustr::from(name)))
            .map(|name| format!(".{}", name))
            .collect();

        // TODO Do guard exhaustiveness checking
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

pub struct OptionExhaustivenessChecker;

impl ExhaustivenessChecker for OptionExhaustivenessChecker {
    fn check(
        &self,
        _env: &mut crate::environment::MiddleEnvironment,
        _scope: crate::scoping::ScopeId,
        patterns: &[MatchArmType],
        data_type: &MirDataType,
    ) -> Result<ExhaustivenessReport, MiddleErr> {
        if !data_type.unwrap_all_refs().is_option() {
            return Ok(ExhaustivenessReport::exhaustive());
        }

        if WildcardChecker::has_wildcard_in_patterns(patterns) {
            return Ok(ExhaustivenessReport::exhaustive());
        }

        let mut covered_variants = FxHashSet::default();
        let mut has_guards = false;

        for pattern in patterns {
            let (inner_pattern, _aliases) = pattern.clone().alias_bindings();

            match inner_pattern {
                MatchArmType::Enum { value, .. } => {
                    let variant_str = value.text();
                    covered_variants.insert(Ustr::from(variant_str));
                }
                _ => {
                    has_guards = true;
                }
            }
        }

        let missing_variants: Vec<String> = ["Some", "None"]
            .iter()
            .filter(|name| !covered_variants.contains(&Ustr::from(name)))
            .map(|name| format!(".{}", name))
            .collect();

        // TODO Do guard exhaustiveness checking
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
