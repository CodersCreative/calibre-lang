use std::fmt::Display;

use serde::{Deserialize, Serialize};

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum ComparisonOperator {
    Greater,
    Lesser,
    LesserEqual,
    GreaterEqual,
    Equal,
    NotEqual,
}

impl Display for ComparisonOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_operator())
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum BooleanOperator {
    And,
    Or,
}

impl Display for BooleanOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_operator())
    }
}

impl BooleanOperator {
    pub fn from_operator(txt: &str) -> Option<Self> {
        match txt.trim() {
            "&&" => Some(Self::And),
            "||" => Some(Self::Or),
            _ => None,
        }
    }

    pub fn to_operator(&self) -> &str {
        match self {
            Self::And => "&&",
            Self::Or => "||",
        }
    }
}

impl ComparisonOperator {
    pub fn from_operator(txt: &str) -> Option<Self> {
        match txt.trim() {
            ">=" => Some(Self::GreaterEqual),
            ">" => Some(Self::Greater),
            "<=" => Some(Self::LesserEqual),
            "<" => Some(Self::Lesser),
            "==" => Some(Self::Equal),
            "!=" => Some(Self::NotEqual),
            _ => None,
        }
    }

    pub fn to_operator(&self) -> &str {
        match self {
            Self::GreaterEqual => ">=",
            Self::Greater => ">",
            Self::LesserEqual => "<=",
            Self::Lesser => "<",
            Self::Equal => "==",
            Self::NotEqual => "!=",
        }
    }
}
