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
use calibre_parser::ast::nodes::{AstNodeType, matching::MatchArmType};

// TODO Do proper boolean expression checking
pub struct BoolExhaustivenessChecker;

impl ExhaustivenessChecker for BoolExhaustivenessChecker {
    fn check(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        patterns: &[MatchArmType],
        data_type: &MirDataType,
    ) -> Result<ExhaustivenessReport, MiddleErr> {
        let unwrapped_type = data_type.unwrap_all_refs();

        if !matches!(unwrapped_type, MirDataType::Bool) {
            return Ok(ExhaustivenessReport::exhaustive());
        }

        if WildcardChecker::has_wildcard_in_patterns(patterns) {
            return Ok(ExhaustivenessReport::exhaustive());
        }

        let mut covered_true = false;
        let mut covered_false = false;

        for pattern in patterns {
            match pattern {
                MatchArmType::Value(node) => {
                    if let AstNodeType::Identifier(id) = &node.node_type {
                        let ident_str = id.value.get_ident().text();
                        if ident_str == "true" {
                            covered_true = true;
                        } else if ident_str == "false" {
                            covered_false = true;
                        }
                    }
                }
                _ => {
                    return Ok(ExhaustivenessReport::requires_wildcard(
                        "pattern to complext, add a `_` pattern".to_string(),
                    ));
                }
            }
        }

        if covered_true && covered_false {
            Ok(ExhaustivenessReport::exhaustive())
        } else {
            let mut missing = Vec::new();

            if !covered_true {
                missing.push("true".to_string());
            }

            if !covered_false {
                missing.push("false".to_string());
            }

            Ok(ExhaustivenessReport::non_exhaustive(missing))
        }
    }
}
