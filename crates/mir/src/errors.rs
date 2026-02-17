use std::path::PathBuf;

use calibre_parser::{
    ParserError, Span,
    ast::{NodeType, ParserDataType},
};
use thiserror::Error;

#[derive(Error, Debug, Clone, PartialEq)]
pub enum MiddleErr {
    #[error("{0}")]
    At(Span, Box<MiddleErr>),
    #[error("Expected {0} operation.")]
    ExpectedOperation(String),
    #[error("Expected only functions.")]
    ExpectedFunctions,
    #[error("Index out of bounds for list, {0}.")]
    InvalidIndex(i64),
    #[error("Default value name not identifier.")]
    InvalidDefaultFuncArg,
    #[error("No associated enum item : {1:?} in enum {0:?}")]
    UnexpectedEnumItem(String, String),
    #[error("Setters can only have one argument, {0:?}")]
    SetterArgs(Vec<(NodeType, Option<NodeType>)>),
    #[error("Property not found, {0:?}")]
    PropertyNotFound(String),
    #[error("Unable to import {0:?}")]
    CantImport(String),
    #[error("Unable to find scope : {0:?}")]
    Scope(String),
    #[error("Unable to find variable : {0:?}")]
    Variable(String),
    #[error("Unable to find object : {0:?}")]
    Object(String),
    #[error("Enum Variant does not exist : {0:?}")]
    EnumVariant(String),
    #[error("Internal error: {0}")]
    Internal(String),
    #[error("Cannot perform enum style pattern matching on type : {0}")]
    CantMatch(ParserDataType),
    #[error("Parser error in {path:?}")]
    ParserErrors {
        path: PathBuf,
        contents: String,
        errors: Vec<ParserError>,
    },
    #[error("Multiple middle errors")]
    Multiple(Vec<MiddleErr>),
}
