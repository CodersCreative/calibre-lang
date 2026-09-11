use serde::{Deserialize, Serialize};

use crate::ast::{
    ObjectType,
    idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
    nodes::AstNode,
    types::ParserDataType,
};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstStruct {
    pub identifier: PotentialGenericTypeIdentifier,
    pub value: ObjectType<AstNode>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstEnum {
    pub identifier: PotentialGenericTypeIdentifier,
    pub value: PotentialDollarIdentifier,
    pub data: Option<Box<AstNode>>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstTuple {
    pub values: Vec<AstNode>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstRange {
    pub from: Box<AstNode>,
    pub to: Box<AstNode>,
    pub inclusive: bool,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstString {
    pub value: ParserText,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstChar {
    pub value: char,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstFloat {
    pub value: f64,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstInt {
    pub value: ParserText,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstBig {
    pub value: ParserText,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstDataType {
    pub data_type: ParserDataType,
}
