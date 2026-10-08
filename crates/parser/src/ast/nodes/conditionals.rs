use crate::ast::{
    Substitutable,
    nodes::{AstNode, matching::MatchArmType},
    types::ParserDataType,
};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum IfComparisonType {
    IfLet {
        value: AstNode,
        pattern: (Vec<MatchArmType>, Vec<AstNode>),
    },
    If(AstNode),
}

impl Substitutable for IfComparisonType {
    fn substitute(self, subst: &FxHashMap<String, ParserDataType>) -> Self {
        match self {
            Self::If(x) => Self::If(x.substitute(subst)),
            Self::IfLet { value, pattern } => Self::IfLet {
                value: value.substitute(subst),
                pattern: (
                    pattern.0.into_iter().map(|x| x.substitute(subst)).collect(),
                    pattern.1.into_iter().map(|x| x.substitute(subst)).collect(),
                ),
            },
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstIf {
    pub comparison: Box<IfComparisonType>,
    pub then: Box<AstNode>,
    pub otherwise: Option<Box<AstNode>>,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum TernaryType {
    Option,
    Result,
    Normal,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstTernary {
    pub comparison: Box<AstNode>,
    pub then: Box<AstNode>,
    pub otherwise: Option<Box<AstNode>>,
    pub ternary_type: TernaryType,
}
