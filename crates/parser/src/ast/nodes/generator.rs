use crate::ast::{
    nodes::{AstNode, loops::LoopType},
    types::ParserDataType,
};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstGenerator {
    pub map: Box<AstNode>,
    pub data_type: Option<ParserDataType>,
    pub loop_type: Box<LoopType>,
    pub conditionals: Vec<AstNode>,
    pub until: Option<Box<AstNode>>,
}
