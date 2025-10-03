#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Comparison {
    Greater,
    Lesser,
    LesserEqual,
    GreaterEqual,
    Equal,
    NotEqual,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum BooleanOperation {
    And,
    Or,
}

impl BooleanOperation {
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

impl Comparison {
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
