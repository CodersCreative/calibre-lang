use crate::ast::{
    idents::{PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
    nodes::AstNode,
};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstIdentifier {
    pub value: PotentialGenericTypeIdentifier,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstField {
    pub base: Box<AstNode>,
    pub field: PotentialDollarIdentifier,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstScope {
    pub base: Box<AstNode>,
    pub field: PotentialDollarIdentifier,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstIndex {
    pub base: Box<AstNode>,
    pub index: Box<AstNode>,
}
