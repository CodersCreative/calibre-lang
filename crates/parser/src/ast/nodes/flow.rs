use crate::{
    Span,
    ast::{
        Substitutable, idents::PotentialDollarIdentifier, nodes::AstNode, types::ParserDataType,
    },
};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum AstEmit {
    Scope(Box<AstNode>),
    Channel {
        left: Box<AstNode>,
        right: Box<AstNode>,
        left_channel: bool,
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

impl TryCatch {
    pub fn substitute(mut self, subst: &FxHashMap<String, ParserDataType>) -> Self {
        *self.body = self.body.substitute(subst);
        self
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum TryType {
    Option,
    Result,
    Panic,
    Normal,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstTry {
    pub value: Box<AstNode>,
    pub catch: Option<TryCatch>,
    pub try_type: TryType,
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

impl Substitutable for PipeSegment {
    fn substitute(self, subst: &FxHashMap<String, ParserDataType>) -> Self {
        match self {
            Self::Named { identifier, node } => Self::Named {
                identifier,
                node: node.substitute(subst),
            },
            Self::Unnamed(x) => Self::Unnamed(x.substitute(subst)),
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
