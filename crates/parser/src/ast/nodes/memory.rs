use crate::ast::{RefMutability, idents::PotentialDollarIdentifier, nodes::AstNode};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstRef {
    pub mutability: RefMutability,
    pub value: Box<AstNode>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstDeref {
    pub value: Box<AstNode>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstDrop {
    pub value: PotentialDollarIdentifier,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstMove {
    pub value: Box<AstNode>,
}
