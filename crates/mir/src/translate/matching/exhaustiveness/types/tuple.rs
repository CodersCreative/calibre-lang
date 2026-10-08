use crate::{
    ast::types::MirDataType,
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    translate::matching::{
        ExhaustivenessReport,
        exhaustiveness::{ExhaustivenessChecker, WildcardChecker},
    },
    typing::MiddleTypeDefType,
};
use calibre_parser::ast::nodes::{AstNode, AstNodeType, matching::MatchArmType};

pub struct TupleExhaustivenessChecker;

impl ExhaustivenessChecker for TupleExhaustivenessChecker {
    fn check(
        &self,
        env: &mut MiddleEnvironment,
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

        if WildcardChecker::has_full_wildcard_in_patterns(patterns) {
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

        let mut covered_patterns = Vec::new();

        for pattern in patterns {
            let (inner_pattern, _aliases) = pattern.clone().alias_bindings();
            if let MatchArmType::TuplePattern(items) = inner_pattern {
                covered_patterns.push(items);
            } else {
                return Ok(ExhaustivenessReport::requires_wildcard(
                    "non-tuple pattern in tuple pattern, add a `_` pattern".to_string(),
                ));
            }
        }

        let field_domains: Vec<Vec<String>> =
            tuple_types.iter().map(|t| compute_domain(env, t)).collect();

        if field_domains.iter().any(|d| d.is_empty()) {
            return Ok(ExhaustivenessReport::requires_wildcard(
                "tuple has unknown or complex field types, add a `_` pattern".to_string(),
            ));
        }

        let all_combinations = cartesian_product(&field_domains);

        let mut missing_combinations = Vec::new();
        for combination in all_combinations {
            if !is_combination_covered(&combination, &covered_patterns) {
                missing_combinations.push(format!("({})", combination.join(", ")));
            }
        }

        if missing_combinations.is_empty() {
            Ok(ExhaustivenessReport::exhaustive())
        } else {
            Ok(ExhaustivenessReport::non_exhaustive(missing_combinations))
        }
    }
}

// TODO Include more types
fn compute_domain(env: &MiddleEnvironment, data_type: &MirDataType) -> Vec<String> {
    match data_type.unwrap_all_refs() {
        MirDataType::Bool => vec!["true".to_string(), "false".to_string()],
        MirDataType::Struct { identifier, .. } => {
            if let Some(obj) = env.typing.objects.get(identifier)
                && let MiddleTypeDefType::Enum { variants, .. } = &obj.object_type
            {
                return variants
                    .iter()
                    .map(|(name, _)| format!(".{}", name))
                    .collect();
            }
            vec![]
        }
        _ => vec![],
    }
}

fn cartesian_product(domains: &[Vec<String>]) -> Vec<Vec<String>> {
    if domains.is_empty() {
        return vec![vec![]];
    }

    let first = &domains[0];
    let rest = cartesian_product(&domains[1..]);

    let mut result = Vec::new();
    for value in first {
        for combo in &rest {
            let mut new_combo = vec![value.clone()];
            new_combo.extend(combo.clone());
            result.push(new_combo);
        }
    }
    result
}

fn is_combination_covered(combination: &[String], patterns: &[Vec<MatchArmType>]) -> bool {
    for pattern in patterns {
        if pattern_matches_combination(pattern, combination) {
            return true;
        }
    }
    false
}

fn pattern_matches_combination(pattern: &[MatchArmType], combination: &[String]) -> bool {
    for (field_pattern, expected_value) in pattern.iter().zip(combination.iter()) {
        match field_pattern {
            MatchArmType::Value(node) => {
                if !pattern_value_matches(node, expected_value) {
                    return false;
                }
            }

            MatchArmType::Enum { value, .. } => {
                let variant_name = format!(".{}", value.text());
                if variant_name != *expected_value {
                    return false;
                }
            }
            MatchArmType::Wildcard(_) | MatchArmType::Let { .. } => {}
            _ => {
                return false;
            }
        }
    }
    true
}

fn pattern_value_matches(node: &AstNode, expected: &str) -> bool {
    match &node.node_type {
        AstNodeType::Identifier(id) => {
            let ident_str = id.value.get_ident().text();
            ident_str == expected
        }
        _ => false,
    }
}
