use crate::ast::{
    binary::BinaryOperator,
    comparison::{BooleanOperator, ComparisonOperator},
    nodes::AstNode,
    types::ParserDataType,
};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstBinary {
    pub left: Box<AstNode>,
    pub right: Box<AstNode>,
    pub operator: BinaryOperator,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstComparison {
    pub left: Box<AstNode>,
    pub right: Box<AstNode>,
    pub operator: ComparisonOperator,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstBoolean {
    pub left: Box<AstNode>,
    pub right: Box<AstNode>,
    pub operator: BooleanOperator,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum AsFailureMode {
    Result,
    Panic,
    Option,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstAs {
    pub value: Box<AstNode>,
    pub data_type: ParserDataType,
    pub failure_mode: AsFailureMode,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstIs {
    pub value: Box<AstNode>,
    pub data_type: ParserDataType,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstIn {
    pub identifier: Box<AstNode>,
    pub value: Box<AstNode>,
}
