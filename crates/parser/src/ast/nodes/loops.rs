use crate::ast::{
    idents::PotentialDollarIdentifier,
    nodes::{AstNode, matching::MatchArmType},
    types::ParserDataType,
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum LoopType {
    Let {
        value: AstNode,
        pattern: (Vec<MatchArmType>, Vec<AstNode>),
    },
    While(AstNode),
    For(PotentialDollarIdentifier, AstNode),
    Loop,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstLoop {
    pub loop_type: Box<LoopType>,
    pub body: Box<AstNode>,
    pub until: Option<Box<AstNode>>,
    pub label: Option<PotentialDollarIdentifier>,
    pub else_body: Option<Box<AstNode>>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstIter {
    pub data_type: ParserDataType,
    pub map: Box<AstNode>,
    pub spawned: bool,
    pub loop_type: Box<LoopType>,
    pub conditionals: Vec<AstNode>,
    pub until: Option<Box<AstNode>>,
}
