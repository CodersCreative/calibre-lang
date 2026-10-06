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

pub struct TupleExhaustivenessChecker;

impl ExhaustivenessChecker for TupleExhaustivenessChecker {
    fn check(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        patterns: &[MatchArmType],
        data_type: &MirDataType,
    ) -> Result<ExhaustivenessReport, MiddleErr> {
        let tuple_types = match data_type.unwrap_all_refs() {
            MirDataType::Tuple(types) => types,
            _ => {
                return Ok(ExhaustivenessReport::exhaustive());
            }
        };

        if WildcardChecker::has_wildcard_in_patterns(patterns) {
            return Ok(ExhaustivenessReport::exhaustive());
        }

        let has_infinite = tuple_types.iter().any(|t| {
            let t = t.unwrap_all_refs();
            t.is_infinite()
        });

        if has_infinite {
            return Ok(ExhaustivenessReport::requires_wildcard(
                "tuple contains infinite possibilities, add a `_` pattern".to_string(),
            ));
        }

        // TODO Implement Cartesian product checking and fully finish tuple exhaustiveness
        Ok(ExhaustivenessReport::requires_wildcard(
            "tuple exhaustiveness not fully implemented, add a `_` pattern".to_string(),
        ))
    }
}
