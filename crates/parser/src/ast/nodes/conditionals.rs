use crate::ast::nodes::{AstNode, matching::MatchArmType};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum IfComparisonType {
    IfLet {
        value: AstNode,
        pattern: (Vec<MatchArmType>, Vec<AstNode>),
    },
    If(AstNode),
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstIf {
    pub comparison: Box<IfComparisonType>,
    pub then: Box<AstNode>,
    pub otherwise: Option<Box<AstNode>>,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum TernaryType {
    Option,
    Result,
    Normal,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstTernary {
    pub comparison: Box<AstNode>,
    pub then: Box<AstNode>,
    pub otherwise: Option<Box<AstNode>>,
    pub ternary_type: TernaryType,
}
