use crate::{
    ast::types::MirDataType, environment::MiddleEnvironment, errors::MiddleErr, scoping::ScopeId,
};
use calibre_parser::ast::nodes::matching::MatchArmType;

pub struct UsefulnessChecker;

impl UsefulnessChecker {
    // TODO Implement full usefulness algorithm
    pub fn is_useful(
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _patterns_seen: &[MatchArmType],
        _pattern: &MatchArmType,
        _data_type: &MirDataType,
    ) -> Result<bool, MiddleErr> {
        Ok(true)
    }

    // TODO Compute usefulness for all patterns and check exhaustiveness
    pub fn compute_match_usefulness(
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        patterns: &[MatchArmType],
        data_type: &MirDataType,
    ) -> Result<super::ExhaustivenessReport, MiddleErr> {
        super::ExhaustivenessCheckerDispatcher::check(env, scope, patterns, data_type)
    }
}
