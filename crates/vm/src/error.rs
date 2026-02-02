use calibre_parser::ast::{
    ParserDataType, ParserInnerType,
    binary::BinaryOperator,
    comparison::{BooleanOperator, ComparisonOperator},
};

use crate::value::RuntimeValue;

#[derive(Debug)]
pub enum RuntimeError {
    Boolean(RuntimeValue, RuntimeValue, BooleanOperator),
    Comparison(RuntimeValue, RuntimeValue, ComparisonOperator),
    Binary(RuntimeValue, RuntimeValue, BinaryOperator),
    UnexpectedType(RuntimeValue),
    CantConvert(RuntimeValue, ParserInnerType),
    StackUnderflow,
    FunctionNotFound(String),
    InvalidFunctionCall,
}
