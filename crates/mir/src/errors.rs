use calibre_parser::{
    ParserError, Span,
    ast::{nodes::NodeType, types::ParserDataType},
};
use std::path::PathBuf;
use thiserror::Error;

#[derive(Error, Debug, Clone, PartialEq)]
pub enum MiddleErr {
    #[error("{0}")]
    At(Span, Box<MiddleErr>),
    #[error("Expected {0} operation.")]
    ExpectedOperation(String),
    #[error("Invalid tag : {0}.")]
    InvalidTag(String),
    #[error("Expected only functions.")]
    ExpectedFunctions,
    #[error("Unable to infer type.")]
    InferImpossible,
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
    #[error("Unable to find macro arg : ${0}")]
    MacroArg(String),
    #[error("Overload Invalid : {0:?}")]
    Overload(String),
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
    #[error("Error in {path:?}")]
    InFile {
        path: PathBuf,
        contents: String,
        error: Box<MiddleErr>,
    },
    #[error("Multiple middle errors")]
    Multiple(Vec<MiddleErr>),
}

impl calibre_parser::CalibreError for MiddleErr {
    fn code(&self) -> usize {
        match self {
            Self::At(_, inner) => inner.code(),
            Self::ExpectedOperation(_) => 201,
            Self::InvalidTag(_) => 202,
            Self::ExpectedFunctions => 203,
            Self::InferImpossible => 204,
            Self::InvalidIndex(_) => 205,
            Self::InvalidDefaultFuncArg => 206,
            Self::UnexpectedEnumItem(_, _) => 207,
            Self::SetterArgs(_) => 208,
            Self::PropertyNotFound(_) => 209,
            Self::CantImport(_) => 210,
            Self::Scope(_) => 211,
            Self::Variable(_) => 212,
            Self::Overload(_) => 213,
            Self::Object(_) => 214,
            Self::EnumVariant(_) => 215,
            Self::Internal(_) => 216,
            Self::CantMatch(_) => 217,
            Self::ParserErrors { .. } => 218,
            Self::InFile { .. } => 2019,
            Self::Multiple(_) => 220,
            Self::MacroArg(_) => 221,
        }
    }

    fn hint(&self) -> Option<String> {
        match self {
            Self::At(_, inner) => inner.hint(),
            Self::ExpectedOperation(op) => Some(format!(
                "ensure the operation `{op}` is valid in this context"
            )),
            Self::InvalidTag(tag) => Some(format!("use a valid tag instead of `{tag}`")),
            Self::ExpectedFunctions => {
                Some("only function declarations are valid in this section".to_string())
            }
            Self::InferImpossible => {
                Some("add explicit type annotations to help type inference".to_string())
            }
            Self::InvalidIndex(idx) => {
                Some(format!("index {idx} is out of bounds - check list length"))
            }
            Self::UnexpectedEnumItem(item, enum_name) => Some(format!(
                "`{item}` is not a valid variant of enum `{enum_name}`"
            )),
            Self::SetterArgs(args) => Some(format!(
                "setters must have exactly one argument, found {}",
                args.len()
            )),
            Self::PropertyNotFound(prop) => {
                Some(format!("property `{prop}` does not exist on this type"))
            }
            Self::CantImport(path) => Some(format!("cannot import from `{path}` - check the path")),
            Self::Scope(scope) => Some(format!("scope `{scope}` not found - check module path")),
            Self::Variable(var) => Some(format!(
                "variable `{var}` not found - check spelling or scope"
            )),
            Self::Overload(msg) => Some(format!("overload error: {msg}")),
            Self::Object(obj) => Some(format!(
                "object `{obj}` not found - check spelling or imports"
            )),
            Self::MacroArg(x) => Some(format!(
                "macro arg `{x}` not found - check spelling or imports"
            )),
            Self::EnumVariant(variant) => Some(format!("enum variant `{variant}` does not exist")),
            Self::Internal(msg) => Some(format!("internal error: {msg} - please report this bug")),
            Self::CantMatch(ty) => Some(format!(
                "cannot perform enum pattern matching on type `{ty}`"
            )),
            Self::ParserErrors { .. } => None,
            Self::InFile { .. } => None,
            Self::Multiple(_) => None,
            _ => None,
        }
    }

    fn step(&self) -> &'static str {
        "MIR"
    }

    fn span(&self) -> Span {
        match self {
            Self::At(span, _) => *span,
            _ => Span::default(),
        }
    }
}
