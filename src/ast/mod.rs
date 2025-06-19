pub mod binary;
pub mod comparison;

use std::collections::HashMap;

use binary::BinaryOperator;
use comparison::Comparison;

use crate::{lexer::TokenType, runtime::values::RuntimeType};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum RefMutability {
    Value,
    Ref,
    MutRef,
    MutValue,
}
impl From<TokenType> for RefMutability {
    fn from(value: TokenType) -> Self {
        match value {
            TokenType::Mut => RefMutability::MutValue,
            TokenType::RefMut => RefMutability::MutRef,
            TokenType::Ref => RefMutability::Ref,
            _ => RefMutability::Value,
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
        data_type: Option<RuntimeType>,
    },
    ImplDeclaration {
        identifier: String,
        functions: Vec<(NodeType, bool)>,
    },
    StructDeclaration {
        identifier: String,
        properties: HashMap<String, RuntimeType>,
    },
    FunctionDeclaration {
        identifier: String,
        parameters: Vec<(String, RuntimeType, RefMutability)>,
        body: Box<Vec<NodeType>>,
        return_type: Option<RuntimeType>,
        is_async: bool,
    },
    AssignmentExpression {
        identifier: Box<NodeType>,
        value: Box<NodeType>,
    },
    Identifier(String),
    StringLiteral(String),
    ListLiteral(Box<Vec<NodeType>>),
    CharLiteral(char),
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
    ComparisonExpression {
        left: Box<NodeType>,
        right: Box<NodeType>,
        operator: Comparison,
    },
    IfStatement {
        comparisons: Box<Vec<NodeType>>,
        bodies: Vec<Box<Vec<NodeType>>>,
    },
    MapLiteral(HashMap<String, Option<NodeType>>),
}
