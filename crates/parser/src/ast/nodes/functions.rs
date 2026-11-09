use crate::ast::{
    Substitutable,
    idents::{ParserText, PotentialDollarIdentifier},
    nodes::{AstNode, DestructurePattern, VarType},
    types::{GenericTypes, ParserDataType},
};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct FunctionParam {
    pub var_type: VarType,
    pub identifier: PotentialDollarIdentifier,
    pub data_type: Option<ParserDataType>,
    pub default: Option<Box<AstNode>>,
}

impl FunctionParam {
    pub fn substitute(self, subst: &FxHashMap<String, ParserDataType>) -> Self {
        Self {
            var_type: self.var_type,
            identifier: self.identifier,
            data_type: self.data_type.map(|x| x.substitute(subst)),
            default: self.default.map(|x| Box::new(x.substitute(subst))),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, Default)]
pub struct FunctionHeader {
    pub generics: GenericTypes,
    pub parameters: Vec<FunctionParam>,
    pub return_type: ParserDataType,
    pub param_destructures: Vec<(usize, DestructurePattern)>,
}

impl FunctionHeader {
    pub fn substitute(mut self, subst: &FxHashMap<String, ParserDataType>) -> Self {
        self.parameters = self
            .parameters
            .into_iter()
            .map(|x| x.substitute(subst))
            .collect();

        self.return_type = self.return_type.substitute(subst);

        self.generics = self.generics.substitute(subst);

        self
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstFunction {
    pub header: FunctionHeader,
    pub body: Box<AstNode>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstExtern {
    pub abi: String,
    pub identifier: PotentialDollarIdentifier,
    pub parameters: Vec<ParserDataType>,
    pub return_type: ParserDataType,
    pub library: String,
    pub symbol: Option<String>,
    pub declared: bool,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum CallArg {
    Value(AstNode),
    Named(PotentialDollarIdentifier, AstNode),
}

impl CallArg {
    pub fn get_node(&self) -> &AstNode {
        self.into()
    }
}

impl Substitutable for CallArg {
    fn substitute(self, subst: &FxHashMap<String, ParserDataType>) -> Self {
        match self {
            Self::Named(x, y) => Self::Named(x, y.substitute(subst)),
            Self::Value(x) => Self::Value(x.substitute(subst)),
        }
    }
}

impl From<CallArg> for AstNode {
    fn from(value: CallArg) -> Self {
        match value {
            CallArg::Value(x) => x,
            CallArg::Named(_, x) => x,
        }
    }
}

impl<'a> From<&'a CallArg> for &'a AstNode {
    fn from(value: &'a CallArg) -> Self {
        match value {
            CallArg::Value(x) => x,
            CallArg::Named(_, x) => x,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstCall {
    pub string_fn: Option<ParserText>,
    pub caller: Box<AstNode>,
    pub generic_types: Vec<ParserDataType>,
    pub args: Vec<CallArg>,
    pub reverse_args: Vec<AstNode>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstCurry {
    pub value: Box<AstNode>,
}
