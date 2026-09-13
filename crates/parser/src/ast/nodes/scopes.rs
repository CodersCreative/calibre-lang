use crate::ast::{idents::PotentialDollarIdentifier, nodes::AstNode};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct NamedScope {
    pub name: PotentialDollarIdentifier,
    pub args: Vec<(PotentialDollarIdentifier, AstNode)>,
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
