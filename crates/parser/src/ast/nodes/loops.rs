use crate::ast::{
    idents::PotentialDollarIdentifier,
    nodes::{AstNode, matching::MatchArmType},
    types::ParserDataType,
};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum LoopType {
    Let {
        value: AstNode,
        pattern: (Vec<MatchArmType>, Vec<AstNode>),
    },
    While(AstNode),
    For(PotentialDollarIdentifier, AstNode),
    Loop,
}

impl LoopType {
    pub fn substitute(self, subst: &FxHashMap<String, ParserDataType>) -> Self {
        match self {
            Self::For(x, y) => Self::For(x, y.substitute(subst)),
            Self::While(x) => Self::While(x.substitute(subst)),
            Self::Let { value, pattern } => Self::Let {
                value: value.substitute(subst),
                pattern: (
                    pattern.0.into_iter().map(|x| x.substitute(subst)).collect(),
                    pattern.1.into_iter().map(|x| x.substitute(subst)).collect(),
                ),
            },
            x => x,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstLoop {
    pub loop_type: Box<LoopType>,
    pub body: Box<AstNode>,
    pub until: Option<Box<AstNode>>,
    pub label: Option<PotentialDollarIdentifier>,
    pub else_body: Option<Box<AstNode>>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstIter {
    pub data_type: ParserDataType,
    pub map: Box<AstNode>,
    pub spawned: bool,
    pub loop_type: Box<LoopType>,
    pub conditionals: Vec<AstNode>,
    pub until: Option<Box<AstNode>>,
}
