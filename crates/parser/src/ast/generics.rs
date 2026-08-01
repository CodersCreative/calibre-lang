use crate::ast::{idents::PotentialDollarIdentifier, nodes::Node, types::PotentialNewType};

#[derive(Debug, Clone, PartialEq)]
pub enum TraitMemberKind {
    Const,
    Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitMember {
    pub kind: TraitMemberKind,
    pub identifier: PotentialDollarIdentifier,
    pub data_type: PotentialNewType,
    pub value: Option<Box<Node>>,
}
