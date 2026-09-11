use crate::ast::{nodes::AstNode, types::ParserDataType};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstList {
    pub data_type: ParserDataType,
    pub values: Vec<AstNode>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstListRepeat {
    pub data_type: ParserDataType,
    pub value: Box<AstNode>,
    pub count: Box<AstNode>,
}
