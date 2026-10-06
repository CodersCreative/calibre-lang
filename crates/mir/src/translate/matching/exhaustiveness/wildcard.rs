use calibre_parser::ast::nodes::matching::{MatchArmType, MatchStringPatternPart, MatchTupleItem};

pub struct WildcardChecker;

impl WildcardChecker {
    pub fn has_wildcard(pattern: &MatchArmType) -> bool {
        match pattern {
            MatchArmType::Wildcard(_) => true,
            MatchArmType::At { pattern: inner, .. } => Self::has_wildcard(inner),
            MatchArmType::TuplePattern(items) => items.iter().any(|item| match item {
                MatchTupleItem::Wildcard(_) => true,
                MatchTupleItem::At { pattern: inner, .. } => {
                    Self::has_wildcard_in_tuple_item(inner)
                }
                _ => false,
            }),
            MatchArmType::ListPattern(items) => items.iter().any(|item| match item {
                MatchTupleItem::Wildcard(_) => true,
                MatchTupleItem::Rest(_) => true,
                MatchTupleItem::At { pattern: inner, .. } => {
                    Self::has_wildcard_in_tuple_item(inner)
                }
                _ => false,
            }),
            MatchArmType::Enum { pattern: inner, .. } => inner
                .as_ref()
                .map(|p| Self::has_wildcard(p))
                .unwrap_or(false),
            _ => false,
        }
    }

    fn has_wildcard_in_tuple_item(item: &MatchTupleItem) -> bool {
        match item {
            MatchTupleItem::Wildcard(_) => true,
            MatchTupleItem::At { pattern: inner, .. } => Self::has_wildcard_in_tuple_item(inner),
            MatchTupleItem::Enum { pattern: inner, .. } => inner
                .as_ref()
                .map(|p| Self::has_wildcard(p))
                .unwrap_or(false),
            MatchTupleItem::StringPattern(parts) => parts
                .iter()
                .any(|part| matches!(part, MatchStringPatternPart::Wildcard(_))),
            _ => false,
        }
    }

    pub fn has_wildcard_in_patterns(patterns: &[MatchArmType]) -> bool {
        patterns.iter().any(Self::has_wildcard)
    }

    pub fn has_full_wildcard_in_patterns(patterns: &[MatchArmType]) -> bool {
        patterns.iter().any(MatchArmType::is_wildcard)
    }

    pub fn find_wildcard_index(patterns: &[MatchArmType]) -> Option<usize> {
        patterns.iter().position(Self::has_wildcard)
    }
}
