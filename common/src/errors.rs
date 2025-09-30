
use std::num::{ParseFloatError, ParseIntError};
use calibre_parser::ast::{binary::BinaryOperator, NodeType};
use thiserror::Error;
use crate::environment::{RuntimeValue, RuntimeType};

#[derive(Error, Debug, Clone)]
pub enum ASTError<T : RuntimeValue> {
    #[error("Cannot {2:?} values of {0:?}, {1:?}.")]
    BinaryOperator(T, T, BinaryOperator),
    #[error("Cannot perform a boolean operation to values of {0:?}, {1:?}.")]
    BooleanOperator(T, T),
}

#[derive(Error, Debug, Clone)]
pub enum RuntimeErr<T : RuntimeValue, U : RuntimeType> {
    #[error("{0}")]
    Value(ValueErr<T, U>),
    #[error("{0}")]
    AST(ASTError<T>),
    #[error("Cannot assign a literal value, {0:?}.")]
    AssignNonVariable(NodeType),
    #[error("Cannot mutably reference a non mutable value, {0:?}.")]
    MutRefNonMut(T),
    #[error("Cannot mutably reference a non variable, {0:?}.")]
    RefNonVar(NodeType),
    #[error("Cannot index a value that is not a list, {0:?}.")]
    IndexNonList(NodeType),
    #[error("This AST Node has not been implemented, {0:?}.")]
    NotImplemented(NodeType),
    #[error("Expected {0:?} operation.")]
    ExpectedOperation(String),
    #[error("Expected only functions.")]
    ExpectedFunctions,
    #[error("{0:?} is not of type {1:?}.")]
    ExpectedType(T, U),
    #[error("Variable {0:?} has an unexpected type.")]
    UnexpectedType(T),
    #[error("Node {0:?} has an unexpected type.")]
    UnexpectedNode(NodeType),
    #[error("No associated enum item : {1:?} in enum {0:?}")]
    UnexpectedEnumItem(String, String),
    #[error("Setters can only have one argument, {0:?}")]
    SetterArgs(Vec<(NodeType, Option<NodeType>)>),
    #[error("Property not found, {0:?}")]
    PropertyNotFound(String),
    #[error("Out of bounds of a array: {0:?} - Value : {1:?}")]
    OutOfBounds(String, i16),
}

#[derive(Error, Debug, Clone)]
pub enum ScopeErr<T : RuntimeValue> {
    #[error("Unable to resolve variable : {0}.")]
    Variable(String),
    #[error("Unable to assign immutable variable : {0}.")]
    AssignConstant(String),
    #[error("Unable to shadow immutable variable : {0}.")]
    ShadowConstant(String),
    #[error("Variable types dont match : {0:?} and {1:?}.")]
    TypeMismatch(T, T),
    #[error("Unable to resolve object : {0:?}.")]
    Object(String),
    #[error("Unable to resolve static function : {0}.")]
    Function(String),
    #[error("Unable to resolve scope : {0}.")]
    Scope(String),
}

#[derive(Error, Debug, Clone)]
pub enum ValueErr<T : RuntimeValue, U : RuntimeType> {
    #[error("Unable to convert: {0:?} -> {1:?}.")]
    Conversion(T, U),
    #[error("Unable to progress value.")]
    ProgressErr,
    #[error("{0}")]
    Scope(ScopeErr<T>),
    #[error("{0}")]
    ParseIntError(ParseIntError),
    #[error("{0}")]
    ParseFloatError(ParseFloatError),
}

impl<T :RuntimeValue, U: RuntimeType> From<ParseIntError> for ValueErr<T, U> {
    fn from(value: ParseIntError) -> Self {
        Self::ParseIntError(value)
    }
}

impl<T :RuntimeValue, U: RuntimeType> From<ParseFloatError> for ValueErr<T, U> {
    fn from(value: ParseFloatError) -> Self {
        Self::ParseFloatError(value)
    }
}

impl<T :RuntimeValue, U: RuntimeType> From<ScopeErr<T>> for ValueErr<T, U> {
    fn from(value: ScopeErr<T>) -> Self {
        Self::Scope(value)
    }
}

impl<T :RuntimeValue, U: RuntimeType> From<ASTError<T>> for RuntimeErr<T, U> {
    fn from(value: ASTError<T>) -> Self {
        Self::AST(value)
    }
}
impl<T :RuntimeValue, U: RuntimeType> From<ValueErr<T, U>> for RuntimeErr<T, U> {
    fn from(value: ValueErr<T, U>) -> Self {
        Self::Value(value)
    }
}

impl<T :RuntimeValue, U: RuntimeType> From<ScopeErr<T>> for RuntimeErr<T, U> {
    fn from(value: ScopeErr<T>) -> Self {
        Self::Value(ValueErr::Scope(value))
    }
}
