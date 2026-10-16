use crate::{
    ast::types::MirDataType,
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    translate::matching::{
        ExhaustivenessReport,
        exhaustiveness::{ExhaustivenessChecker, wildcard::WildcardChecker},
    },
};
use calibre_parser::ast::nodes::matching::MatchArmType;

pub mod aggregate;
pub mod bool;
pub mod integer;
pub mod list;
pub mod result;
pub mod string;
pub mod tuple;

pub struct NormalExhaustivenessChecker;

impl ExhaustivenessChecker for NormalExhaustivenessChecker {
    fn check(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        patterns: &[MatchArmType],
        _data_type: &MirDataType,
    ) -> Result<ExhaustivenessReport, MiddleErr> {
        if WildcardChecker::has_wildcard_in_patterns(patterns) {
            return Ok(ExhaustivenessReport::exhaustive());
        }

        Ok(ExhaustivenessReport::requires_wildcard(
            "add a `_` pattern".to_string(),
        ))
    }
}
