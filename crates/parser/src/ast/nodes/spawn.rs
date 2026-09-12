use crate::ast::nodes::AstNode;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SelectArmKind {
    Recv,
    Send,
    Default,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SelectArm {
    pub patterns: Vec<(SelectArmKind, Option<AstNode>, Option<AstNode>)>,
    pub conditionals: Vec<AstNode>,
    pub body: AstNode,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstSpawn {
    pub items: Vec<AstNode>,
    pub auto_wait: bool,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstSelect {
    pub arms: Vec<SelectArm>,
}
