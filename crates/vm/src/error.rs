use calibre_parser::ast::{
    ParserDataType, ParserInnerType,
    binary::BinaryOperator,
    comparison::{BooleanOperator, ComparisonOperator},
};
use calibre_parser::lexer::Span;

use crate::value::RuntimeValue;

#[derive(Debug)]
pub enum RuntimeError {
    At(Span, Box<RuntimeError>),
    Boolean(RuntimeValue, RuntimeValue, BooleanOperator),
    Comparison(RuntimeValue, RuntimeValue, ComparisonOperator),
    Binary(RuntimeValue, RuntimeValue, BinaryOperator),
    UnexpectedType(RuntimeValue),
    CantConvert(RuntimeValue, ParserInnerType),
    StackUnderflow,
    FunctionNotFound(String),
    InvalidFunctionCall,
    DanglingRef(String),
    Panic,
}
