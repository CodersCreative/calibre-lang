pub mod guards;
pub mod types;
pub mod usefulness;
pub mod wildcard;

use crate::{
    ast::types::MirDataType,
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    translate::matching::exhaustiveness::{
        types::{
            aggregate::{EnumExhaustivenessChecker, StructExhaustivenessChecker},
            bool::BoolExhaustivenessChecker,
            integer::IntegerExhaustivenessChecker,
            list::ListExhaustivenessChecker,
            result::{OptionExhaustivenessChecker, ResultExhaustivenessChecker},
            string::StringExhaustivenessChecker,
            tuple::TupleExhaustivenessChecker,
        },
        wildcard::WildcardChecker,
    },
    typing::MiddleTypeDefType,
};
use calibre_parser::{Span, ast::nodes::matching::MatchArmType};

#[derive(Debug, Clone)]
pub struct ExhaustivenessReport {
    pub is_exhaustive: bool,
    pub missing_patterns: Vec<String>,
    pub unreachable_patterns: Vec<usize>,
    pub requires_wildcard: bool,
}

impl ExhaustivenessReport {
    pub fn exhaustive() -> Self {
        Self {
            is_exhaustive: true,
            missing_patterns: Vec::new(),
            unreachable_patterns: Vec::new(),
            requires_wildcard: false,
        }
    }

    pub fn non_exhaustive(missing: Vec<String>) -> Self {
        Self {
            is_exhaustive: false,
            missing_patterns: missing,
            unreachable_patterns: Vec::new(),
            requires_wildcard: false,
        }
    }

    pub fn requires_wildcard(reason: String) -> Self {
        Self {
            is_exhaustive: false,
            missing_patterns: vec![reason],
            unreachable_patterns: Vec::new(),
            requires_wildcard: true,
        }
    }
}

pub trait ExhaustivenessChecker {
    fn check(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        patterns: &[MatchArmType],
        data_type: &MirDataType,
    ) -> Result<ExhaustivenessReport, MiddleErr>;
}

pub struct ExhaustivenessCheckerDispatcher;

impl ExhaustivenessCheckerDispatcher {
    pub fn check(
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        patterns: &[MatchArmType],
        data_type: &MirDataType,
    ) -> Result<ExhaustivenessReport, MiddleErr> {
        let checker: Box<dyn ExhaustivenessChecker> = match data_type.unwrap_all_refs() {
            MirDataType::Bool => Box::new(BoolExhaustivenessChecker),
            MirDataType::Int | MirDataType::UInt | MirDataType::Byte => {
                Box::new(IntegerExhaustivenessChecker)
            }
            MirDataType::Str => Box::new(StringExhaustivenessChecker),
            MirDataType::Option(_) => Box::new(OptionExhaustivenessChecker),
            MirDataType::Result { .. } => Box::new(ResultExhaustivenessChecker),
            MirDataType::Struct { identifier, .. } => {
                if let Some(obj) = env.typing.objects.get(identifier) {
                    if matches!(obj.object_type, MiddleTypeDefType::Enum { .. }) {
                        Box::new(EnumExhaustivenessChecker)
                    } else {
                        Box::new(StructExhaustivenessChecker)
                    }
                } else {
                    Box::new(StructExhaustivenessChecker)
                }
            }
            MirDataType::Tuple(_) => Box::new(TupleExhaustivenessChecker),
            MirDataType::List(_) => Box::new(ListExhaustivenessChecker),
            _ => {
                return Ok(ExhaustivenessReport::requires_wildcard(String::from(
                    "add a `_` pattern",
                )));
            }
        };

        let mut report = checker.check(env, scope, patterns, data_type)?;

        // TODO Implement better unreachability analysis
        if let Some(wildcard_idx) = WildcardChecker::find_wildcard_index(patterns) {
            for idx in (wildcard_idx + 1)..patterns.len() {
                report.unreachable_patterns.push(idx);
            }
        }

        Ok(report)
    }

    pub fn add_errors(env: &mut MiddleEnvironment, report: &ExhaustivenessReport, span: Span) {
        if !report.is_exhaustive {
            let error = MiddleErr::NonExhaustiveMatch {
                missing_patterns: report.missing_patterns.clone(),
            };
            let error = env.context.err_at_span(span, error);
            env.context.push_error(error);
        }

        for &idx in &report.unreachable_patterns {
            let error = MiddleErr::UnreachableMatchArm { arm_index: idx };
            let error = env.context.err_at_span(span, error);
            env.context.push_error(error);
        }
    }
}
