use crate::ast::nodes::{AstNode, DestructurePattern};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstAssignment {
    pub identifier: Box<AstNode>,
    pub value: Box<AstNode>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstAssignDestructure {
    pub pattern: DestructurePattern,
    pub value: Box<AstNode>,
}
