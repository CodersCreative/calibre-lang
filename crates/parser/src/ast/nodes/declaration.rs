use crate::ast::{
    idents::PotentialDollarIdentifier,
    nodes::{AstNode, DestructurePattern, VarType},
    types::ParserDataType,
};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstDeclaration {
    pub var_type: VarType,
    pub identifier: PotentialDollarIdentifier,
    pub value: Box<AstNode>,
    pub data_type: ParserDataType,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstDeclareDestructure {
    pub var_type: VarType,
    pub pattern: DestructurePattern,
    pub value: Box<AstNode>,
}
