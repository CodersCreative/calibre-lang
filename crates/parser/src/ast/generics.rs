use crate::ast::{idents::PotentialDollarIdentifier, nodes::AstNode, types::ParserDataType};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TraitMemberKind {
    Const,
    Type,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TraitMember {
    pub kind: TraitMemberKind,
    pub identifier: PotentialDollarIdentifier,
    pub data_type: ParserDataType,
    pub value: Option<Box<AstNode>>,
}
