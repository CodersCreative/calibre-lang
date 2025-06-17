use std::collections::HashMap;

use crate::runtime::values::RuntimeType;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinaryOperator {
    Subtract,
    Add,
    Multiply,
    Divide,
    Power,
    Modulus,
}
impl BinaryOperator {
    pub fn from_symbol(symbol: char) -> Option<Self> {
        match symbol {
            '+' => Some(Self::Add),
            '-' => Some(Self::Subtract),
            '/' => Some(Self::Divide),
            '*' => Some(Self::Multiply),
            '^' => Some(Self::Power),
            '%' => Some(Self::Modulus),
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
            Self::Modulus => '%',
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum NodeType {
    Program(Box<Vec<NodeType>>),
    VariableDeclaration {
        is_mutable: bool,
        identifier: String,
        value: Option<Box<NodeType>>,
        data_type : Option<String>,
    },
    StructDeclaration {
        identifier: String,
        properties: HashMap<String, RuntimeType>,
    },
    FunctionDeclaration {
        identifier: String,
        parameters: HashMap<String, RuntimeType>,
        body: Box<NodeType>,
        return_type : RuntimeType,
        is_async : bool,
    },
    AssignmentExpression {
        identifier: Box<NodeType>,
        value: Box<NodeType>,
    },
    Identifier(String),
    FloatLiteral(f64),
    IntegerLiteral(i64),
    MemberExpression {
        object: Box<NodeType>,
        property: Box<NodeType>,
        is_computed: bool,
    },
    CallExpression(Box<NodeType>, Box<Vec<NodeType>>),
    EOL,
    BinaryExpression {
        left: Box<NodeType>,
        right: Box<NodeType>,
        operator: BinaryOperator,
    },
    MapLiteral(HashMap<String, Option<NodeType>>),
}
