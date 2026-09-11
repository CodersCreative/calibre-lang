use serde::{Deserialize, Serialize};

use crate::{
    Span,
    ast::{idents::PotentialDollarIdentifier, nodes::AstNode},
};

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

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum PipeSegment {
    Unnamed(AstNode),
    Named {
        identifier: PotentialDollarIdentifier,
        node: AstNode,
    },
}

impl PipeSegment {
    pub fn is_named(&self) -> bool {
        !matches!(self, Self::Unnamed(_))
    }

    pub fn span(&self) -> &Span {
        match self {
            Self::Unnamed(x) => &x.span,
            Self::Named {
                identifier: _,
                node,
            } => &node.span,
        }
    }

    pub fn get_node(&self) -> &AstNode {
        match self {
            Self::Unnamed(x) => x,
            Self::Named {
                identifier: _,
                node,
            } => node,
        }
    }
}

impl From<PipeSegment> for AstNode {
    fn from(val: PipeSegment) -> AstNode {
        match val {
            PipeSegment::Unnamed(x) => x,
            PipeSegment::Named {
                identifier: _,
                node,
            } => node,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstPipe {
    pub values: Vec<PipeSegment>,
}
