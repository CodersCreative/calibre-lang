use calibre_parser::ast::{
    binary::BinaryOperator,
    comparison::{BooleanOperator, ComparisonOperator},
};

use crate::value::RuntimeValue;

pub enum RuntimeError {
    Boolean(RuntimeValue, RuntimeValue, BooleanOperator),
    Comparison(RuntimeValue, RuntimeValue, ComparisonOperator),
    Binary(RuntimeValue, RuntimeValue, BinaryOperator),
}
