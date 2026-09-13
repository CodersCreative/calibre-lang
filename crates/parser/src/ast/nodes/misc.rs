use crate::ast::{
    idents::{ParserText, PotentialDollarIdentifier},
    nodes::AstNode,
};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstParen {
    pub value: Box<AstNode>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstTest {
    pub identifier: ParserText,
    pub body: Box<AstNode>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstImport {
    pub module: Vec<PotentialDollarIdentifier>,
    pub alias: Option<PotentialDollarIdentifier>,
    pub values: Vec<PotentialDollarIdentifier>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstTag {
    pub node: Box<AstNode>,
    pub tag: ParserText,
    pub arguments: Vec<AstNode>,
}
