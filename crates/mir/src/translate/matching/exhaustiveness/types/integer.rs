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

pub struct IntegerExhaustivenessChecker;

impl ExhaustivenessChecker for IntegerExhaustivenessChecker {
    fn check(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        patterns: &[MatchArmType],
        data_type: &MirDataType,
    ) -> Result<ExhaustivenessReport, MiddleErr> {
        if !data_type.unwrap_all_refs().is_int() {
            return Ok(ExhaustivenessReport::exhaustive());
        }

        if WildcardChecker::has_wildcard_in_patterns(patterns) {
            return Ok(ExhaustivenessReport::exhaustive());
        }

        Ok(ExhaustivenessReport::requires_wildcard(
            "integers have infinite possibilities, add a `_` pattern".to_string(),
        ))
    }
}
