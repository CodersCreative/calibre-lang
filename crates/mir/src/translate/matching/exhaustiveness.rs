use crate::{
    environment::MiddleEnvironment, errors::MiddleErr, scoping::ScopeId,
    symbols::resolve::ResolutionOptions, typing::MiddleTypeDefType,
};
use calibre_parser::{
    Span,
    ast::{
        nodes::matching::MatchArmType,
        types::{ParserDataType, ParserInnerType},
    },
};
use ustr::{Ustr, UstrSet};

#[derive(Debug, Clone)]
pub struct ExhaustivenessReport {
    pub is_exhaustive: bool,
    pub missing_variants: Vec<Ustr>,
    pub unreachable_patterns: Vec<usize>,
}

pub trait ExhaustivenessChecker {
    fn check(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        patterns: &[MatchArmType],
        data_type: &ParserDataType,
    ) -> Result<ExhaustivenessReport, MiddleErr>;
}

pub struct EnumExhaustivenessChecker;

impl ExhaustivenessChecker for EnumExhaustivenessChecker {
    fn check(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        patterns: &[MatchArmType],
        data_type: &ParserDataType,
    ) -> Result<ExhaustivenessReport, MiddleErr> {
        let unwrapped_type = data_type.clone().unwrap_all_refs();

        let enum_key = match &unwrapped_type.data_type {
            ParserInnerType::Struct(name) => Ustr::from(name),
            ParserInnerType::StructWithGenerics { identifier, .. } => Ustr::from(identifier),
            _ => {
                return Ok(ExhaustivenessReport {
                    is_exhaustive: true,
                    missing_variants: Vec::new(),
                    unreachable_patterns: Vec::new(),
                });
            }
        };

        let enum_def = match env.typing.objects.get(&enum_key) {
            Some(obj) => obj,
            None => {
                return Ok(ExhaustivenessReport {
                    is_exhaustive: true,
                    missing_variants: Vec::new(),
                    unreachable_patterns: Vec::new(),
                });
            }
        };

        let variants = match &enum_def.object_type {
            MiddleTypeDefType::Enum { variants, .. } => variants.clone(),
            _ => {
                // I mean this is meant to be for just enums so realistically this is unreachable... but i'll just return an exhaustive report for now
                return Ok(ExhaustivenessReport {
                    is_exhaustive: true,
                    missing_variants: Vec::new(),
                    unreachable_patterns: Vec::new(),
                });
            }
        };

        let mut covered_variants = UstrSet::default();

        for pattern in patterns {
            match pattern {
                MatchArmType::Enum { value, .. } => {
                    covered_variants.insert(env.resolve(
                        scope,
                        value,
                        ResolutionOptions::default().with_dollar(),
                    )?);
                }
                _ => {
                    // TODO deal with variants being matched within
                }
            }
        }

        let missing_variants: Vec<Ustr> = variants
            .iter()
            .filter(|(name, _)| !covered_variants.contains(name))
            .map(|(name, _)| *name)
            .collect();

        let mut is_exhaustive = missing_variants.is_empty();
        let mut unreachable_patterns = Vec::new();
        let mut found_wildcard = false;

        for (idx, pattern) in patterns.iter().enumerate() {
            if matches!(pattern, MatchArmType::Wildcard(_)) {
                found_wildcard = true;
                is_exhaustive = true;
            } else if found_wildcard {
                unreachable_patterns.push(idx);
            }
        }

        Ok(ExhaustivenessReport {
            is_exhaustive,
            missing_variants: if found_wildcard {
                Vec::new()
            } else {
                missing_variants
            },
            unreachable_patterns,
        })
    }
}

pub struct ExhaustivenessCheckerDispatcher;

impl ExhaustivenessCheckerDispatcher {
    pub fn check(
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        patterns: &[MatchArmType],
        data_type: &ParserDataType,
    ) -> Result<ExhaustivenessReport, MiddleErr> {
        let checker = EnumExhaustivenessChecker;
        checker.check(env, scope, patterns, data_type)
    }

    // TODO
    pub fn add_errors(_env: &mut MiddleEnvironment, report: &ExhaustivenessReport, _span: Span) {
        if !report.is_exhaustive && !report.missing_variants.is_empty() {
            eprintln!(
                "Match is not exhaustive, will be an error in the future... when I finally bite the bullet and add more error types"
            );
            // TODO deal with incomplete patterns
        }

        for _ in &report.unreachable_patterns {
            eprintln!(
                "Unreachable patterns, will be an error in the future... when I finally bite the bullet and add more error types"
            );
            // TODO Deal with unreachable patterns
        }
    }
}
