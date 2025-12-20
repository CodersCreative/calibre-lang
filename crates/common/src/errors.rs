use crate::environment::{RuntimeType, RuntimeValue};
use calibre_parser::{
    ParserError,
    ast::{LoopType, NodeType, binary::BinaryOperator},
    lexer::LexerError,
};
use miette::{Diagnostic, LabeledSpan};
use std::{
    fmt::Display,
    num::{ParseFloatError, ParseIntError},
};
use thiserror::Error;

#[derive(Error, Debug, Clone, Diagnostic)]
pub enum ASTError<T: RuntimeValue> {
    #[error("Cannot {2:?} values of {0:?}, {1:?}.")]
    BinaryOperator(T, T, BinaryOperator),
    #[error("Cannot perform a boolean operation to values of {0:?}, {1:?}.")]
    BooleanOperator(T, T),
    #[error("Cannot perform a comparison operation to values of {0:?}, {1:?}.")]
    ComparisonOperation(T, T),
}

#[derive(Error, Debug, Clone, Diagnostic)]
pub enum RuntimeErr<T: RuntimeValue, U: RuntimeType> {
    #[error("{err}")]
    At {
        err: Box<Self>,
        #[source_code]
        input: String,
        #[label("here")]
        span: Option<(usize, usize)>,
    },
    #[error("{0}")]
    Value(ValueErr<T, U>),
    #[error("{0}")]
    AST(ASTError<T>),
    #[error("Cannot assign a literal value, {0}.")]
    AssignNonVariable(NodeType),
    #[error("Cannot mutably reference a non mutable value, {0:?}.")]
    MutRefNonMut(T),
    #[error("Cannot mutably reference a non variable, {0}.")]
    RefNonVar(NodeType),
    #[error("Cannot de-reference a non reference, {0}.")]
    DerefNonRef(NodeType),
    #[error("Cannot index a value that is not a list, {0}.")]
    IndexNonList(NodeType),
    #[error("This AST Node has not been implemented, {0}.")]
    NotImplemented(NodeType),
    #[error("Cannot call non-callable value, {0:?}.")]
    CantCallNonFunc(T),
    #[error("Cannot perform not opertion on {0:?}.")]
    CantPerformNot(T),
    #[error("Cannot create list comprehension with a while loop, {0}.")]
    IterWhileLoop(LoopType),
    #[error("Cannot loop through value {0:?}.")]
    UnableToLoop(T),
    #[error("Expected {0} operation.")]
    ExpectedOperation(String),
    #[error("Expected only functions.")]
    ExpectedFunctions,
    #[error("Index out of bounds for list, {0}.")]
    InvalidIndex(i64),
    #[error("Default value name not identifier.")]
    InvalidDefaultFuncArg,
    #[error("{0:?} is not of type {1:?}.")]
    ExpectedType(T, U),
    #[error("Variable {0:?} has an unexpected type.")]
    UnexpectedType(T),
    #[error("Node {0:?} has an unexpected type.")]
    UnexpectedNode(NodeType),
    #[error("Node {0:?} needs to be used in the global scope.")]
    UnexpectedNodeInTemp(NodeType),
    #[error("Node {0:?} can't be used in the global scope.")]
    UnexpectedNodeInGlobal(NodeType),
    #[error("No associated enum item : {1:?} in enum {0:?}")]
    UnexpectedEnumItem(String, String),
    #[error("Setters can only have one argument, {0:?}")]
    SetterArgs(Vec<(NodeType, Option<NodeType>)>),
    #[error("Property not found, {0:?}")]
    PropertyNotFound(String),
    #[error("Unable to import {0:?}")]
    CantImport(String),
}

unsafe impl<T: RuntimeValue, U: RuntimeType> Send for RuntimeErr<T, U> {}
unsafe impl<T: RuntimeValue, U: RuntimeType> Sync for RuntimeErr<T, U> {}

#[derive(Error, Debug, Clone, Diagnostic)]
pub enum ScopeErr {
    #[error("Unable to resolve variable : {0}.")]
    Variable(String),
    #[error("Unable to assign immutable variable : {0}.")]
    AssignConstant(String),
    #[error("Unable to shadow immutable variable : {0}.")]
    ShadowConstant(String),
    #[error("Variable types dont match : {0:?}.")]
    TypeMismatch(String),
    #[error("Unable to resolve object : {0:?}.")]
    Object(String),
    #[error("Unable to resolve static function : {0}.")]
    Function(String),
    #[error("Unable to resolve scope : {0}.")]
    Scope(String),
    #[error("{0}")]
    #[diagnostic()]
    Parser(#[from] ParserError),
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

#[derive(Error, Debug, Clone, Diagnostic)]
pub enum ValueErr<T: RuntimeValue, U: RuntimeType> {
    #[error("Unable to convert: {0:?} -> {1:?}.")]
    Conversion(T, U),
    #[error("Unable to progress value.")]
    ProgressErr,
    #[error("{0}")]
    Scope(ScopeErr),
    #[error("{0}")]
    ParseIntError(ParseIntError),
    #[error("{0}")]
    ParseFloatError(ParseFloatError),
}

impl From<Vec<ParserError>> for ScopeErr {
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
impl<T: RuntimeValue, U: RuntimeType> From<ParseIntError> for ValueErr<T, U> {
    fn from(value: ParseIntError) -> Self {
        Self::ParseIntError(value)
    }
}

impl<T: RuntimeValue, U: RuntimeType> From<ParseFloatError> for ValueErr<T, U> {
    fn from(value: ParseFloatError) -> Self {
        Self::ParseFloatError(value)
    }
}

impl<T: RuntimeValue, U: RuntimeType> From<ScopeErr> for ValueErr<T, U> {
    fn from(value: ScopeErr) -> Self {
        Self::Scope(value)
    }
}

impl<T: RuntimeValue, U: RuntimeType> From<ParserError> for RuntimeErr<T, U> {
    fn from(value: ParserError) -> Self {
        Self::Value(ValueErr::Scope(value.into()))
    }
}

impl<T: RuntimeValue, U: RuntimeType> From<Vec<ParserError>> for RuntimeErr<T, U> {
    fn from(value: Vec<ParserError>) -> Self {
        Self::Value(ValueErr::Scope(value.into()))
    }
}

impl<T: RuntimeValue, U: RuntimeType> From<ASTError<T>> for RuntimeErr<T, U> {
    fn from(value: ASTError<T>) -> Self {
        Self::AST(value)
    }
}

impl<T: RuntimeValue, U: RuntimeType> From<ValueErr<T, U>> for RuntimeErr<T, U> {
    fn from(value: ValueErr<T, U>) -> Self {
        Self::Value(value)
    }
}

impl<T: RuntimeValue, U: RuntimeType> From<ScopeErr> for RuntimeErr<T, U> {
    fn from(value: ScopeErr) -> Self {
        Self::Value(ValueErr::Scope(value))
    }
}
