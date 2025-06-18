use crate::runtime::values::RuntimeValue;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Comparison {
    Greater,
    Lesser,
    LesserEqual,
    GreaterEqual,
    Equal,
    NotEqual,
}

impl Comparison {
    pub fn from_operator(txt : &str) -> Option<Self> {
        match txt.trim() {
            "<=" => Some(Self::GreaterEqual),
            "<" => Some(Self::Greater),
            ">=" => Some(Self::LesserEqual),
            ">" => Some(Self::Lesser),
            "==" => Some(Self::Equal),
            "!=" => Some(Self::NotEqual),
            _ => None,
        }
    }

    pub fn to_operator(&self) -> &str{
        match self {
            Self::GreaterEqual => "<=",
            Self::Greater => "<",
            Self::LesserEqual => ">=",
            Self::Lesser => ">",
            Self::Equal => "==",
            Self::NotEqual => "!=",
        }
    }
    
    pub fn handle(&self, left : RuntimeValue, right : RuntimeValue) -> RuntimeValue  {
        RuntimeValue::Bool(match self {
            Self::NotEqual => left != right,
            Self::Equal => left == right,
            Self::Lesser => left > right,
            Self::LesserEqual => left >= right,
            Self::Greater => left < right,
            Self::GreaterEqual => left <= right,
        })
    }
}

