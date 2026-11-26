use crate::{
    Span,
    ast::{
        Substitutable,
        binary::BinaryOperator,
        comparison::{BooleanOperator, ComparisonOperator},
        idents::PotentialDollarIdentifier,
        nodes::AstNode,
        types::ParserDataType,
    },
};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstBinary {
    pub left: Box<AstNode>,
    pub right: Box<AstNode>,
    pub operator: BinaryOperator,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstComparison {
    pub left: Box<AstNode>,
    pub right: Box<AstNode>,
    pub operator: ComparisonOperator,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstBoolean {
    pub left: Box<AstNode>,
    pub right: Box<AstNode>,
    pub operator: BooleanOperator,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum AsFailureMode {
    Result,
    Panic,
    Option,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstAs {
    pub value: Box<AstNode>,
    pub data_type: ParserDataType,
    pub failure_mode: AsFailureMode,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum IsType {
    DataType(ParserDataType),
    Enum {
        base: Option<ParserDataType>,
        variant: PotentialDollarIdentifier,
    },
}

impl IsType {
    pub fn span(&self) -> &Span {
        match self {
            Self::DataType(x) => &x.span,
            Self::Enum {
                base: Some(base), ..
            } => &base.span,
            Self::Enum { variant, .. } => variant.span(),
        }
    }
}

impl Substitutable for IsType {
    fn substitute(self, subst: &FxHashMap<String, ParserDataType>) -> Self {
        match self {
            Self::DataType(x) => Self::DataType(x.substitute(subst)),
            Self::Enum { base, variant } => Self::Enum {
                base: base.map(|x| x.substitute(subst)),
                variant,
            },
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstIs {
    pub value: Box<AstNode>,
    pub right: IsType,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstIn {
    pub identifier: Box<AstNode>,
    pub value: Box<AstNode>,
}
