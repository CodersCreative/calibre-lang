use std::fmt::Display;

use calibre_parser::{
    ParserError,
    ast::{NodeType, ParserDataType},
    lexer::LexerError,
};
use miette::{Diagnostic, LabeledSpan};
use thiserror::Error;

#[derive(Error, Debug, Clone, Diagnostic)]
pub enum MiddleErr {
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
    #[error("Cannot perform enum style pattern matching on type : {0}")]
    CantMatch(String),
    #[error(transparent)]
    #[diagnostic(transparent)]
    Miette(#[from] ReportWrapper),
}
#[derive(Debug, Error, Diagnostic, Clone)]
#[error("")]
pub struct ParserVec(#[label(collection, "related to this")] Vec<LabeledSpan>);

#[derive(Debug, Error, Diagnostic)]
#[diagnostic(transparent)]
pub struct ReportWrapper(miette::Report);

impl Display for ReportWrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl Clone for ReportWrapper {
    fn clone(&self) -> Self {
        Self(miette::Report::msg("Cloned"))
    }
}

impl From<miette::Report> for ReportWrapper {
    fn from(value: miette::Report) -> Self {
        Self(value)
    }
}

impl From<Vec<ParserError>> for MiddleErr {
    fn from(value: Vec<ParserError>) -> Self {
        let mut output = String::new();
        let mut errors = Vec::new();

        // return Self::Parser2 { errors: value };

        for val in value {
            match val {
                ParserError::Lexer(x) => match x {
                    LexerError::Unrecognized { .. } => {
                        errors.push(LabeledSpan::new_with_span(Some(x.to_string()), (0, 1)));
                    }
                },
                ParserError::Syntax {
                    input,
                    err,
                    token,
                    span,
                } => {
                    errors.push(LabeledSpan::new_with_span(
                        Some(format!("{} at {}", err, span)),
                        (output.len() + token.unwrap().0, token.unwrap().1),
                    ));
                    output.push_str(&format!("{}\n", input));
                }
            }
        }
        let report: miette::Report = ParserVec(errors).into();
        Self::Miette(ReportWrapper(report.with_source_code(output)))
    }
}
