use crate::ast::{
    Substitutable,
    idents::PotentialDollarIdentifier,
    nodes::{AstNode, AstNodeType},
    types::ParserDataType,
};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct NamedScope {
    pub name: PotentialDollarIdentifier,
    pub args: Vec<(PotentialDollarIdentifier, AstNode)>,
}

impl Substitutable for NamedScope {
    fn substitute(mut self, subst: &FxHashMap<String, ParserDataType>) -> Self {
        self.args = self
            .args
            .into_iter()
            .map(|x| (x.0, x.1.substitute(subst)))
            .collect();
        self
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstScopeAlias {
    pub identifier: PotentialDollarIdentifier,
    pub value: NamedScope,
    pub create_new_scope: Option<bool>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstScopeDef {
    pub body: Option<Vec<AstNode>>,
    pub named: Option<NamedScope>,
    pub is_temp: bool,
    pub create_new_scope: Option<bool>,
    pub define: bool,
}

impl From<AstScopeDef> for AstNodeType {
    fn from(value: AstScopeDef) -> Self {
        Self::ScopeDeclaration(value)
    }
}
