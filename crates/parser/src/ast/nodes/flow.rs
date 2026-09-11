use serde::{Deserialize, Serialize};

use crate::ast::{idents::PotentialDollarIdentifier, nodes::AstNode};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum AstEmit {
    Scope(Box<AstNode>),
    Channel {
        channel: Box<AstNode>,
        value: Box<AstNode>,
    },
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstBreak {
    pub label: Option<PotentialDollarIdentifier>,
    pub value: Option<Box<AstNode>>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstContinue {
    pub label: Option<PotentialDollarIdentifier>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct TryCatch {
    pub name: Option<PotentialDollarIdentifier>,
    pub body: Box<AstNode>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstTry {
    pub value: Box<AstNode>,
    pub catch: Option<TryCatch>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstReturn {
    pub value: Option<Box<AstNode>>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstDefer {
    pub value: Box<AstNode>,
    pub function: bool,
}
