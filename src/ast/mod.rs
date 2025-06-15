#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinaryOperator {
    Subtract,
    Add,
    Multiply,
    Divide,
    Power,
}
impl BinaryOperator {
    pub fn from_symbol(symbol: char) -> Option<Self> {
        match symbol {
            '+' => Some(Self::Add),
            '-' => Some(Self::Subtract),
            '/' => Some(Self::Divide),
            '*' => Some(Self::Multiply),
            '^' => Some(Self::Power),
            _ => None,
        }
    }

    pub fn to_symbol(&self) -> char {
        match self {
            Self::Add => '+',
            Self::Subtract => '-',
            Self::Divide => '/',
            Self::Multiply => '*',
            Self::Power => '^',
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum NodeType {
    Program(Box<Vec<NodeType>>),
    Identifier(String),
    NumericLiteral(f64),
    BinaryExpression {
        left: Box<NodeType>,
        right: Box<NodeType>,
        operator: BinaryOperator,
    },
}
