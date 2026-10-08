use calibre_parser::ast::nodes::matching::MatchArmType;

pub struct WildcardChecker;

impl WildcardChecker {
    pub fn has_wildcard_in_patterns(patterns: &[MatchArmType]) -> bool {
        patterns.iter().any(MatchArmType::has_wildcard)
    }

    pub fn has_full_wildcard_in_patterns(patterns: &[MatchArmType]) -> bool {
        patterns.iter().any(MatchArmType::is_wildcard)
    }

    pub fn find_wildcard_index(patterns: &[MatchArmType]) -> Option<usize> {
        patterns.iter().position(MatchArmType::has_wildcard)
    }
}
