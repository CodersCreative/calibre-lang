use crate::runtime::{interpreter::InterpreterErr, scope::Environment, values::{RuntimeType, RuntimeValue}};
use calibre_parser::ast::binary::BinaryOperator;
use std::ops::{Add, Div, Mul, Sub};
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum ASTError {
    #[error("Cannot {2:?} values of {0:?}, {1:?}.")]
    BinaryOperator(RuntimeValue, RuntimeValue, BinaryOperator),
    #[error("Cannot perform a boolean operation to values of {0:?}, {1:?}.")]
    BooleanOperator(RuntimeValue, RuntimeValue),
}

pub fn handle(
    op: &BinaryOperator,
    env: &Environment,
    scope: &u64,
    left: RuntimeType,
    right: RuntimeType,
) -> Result<RuntimeType, InterpreterErr> {
    // match op {
    //     BinaryOperator::Add => left + right,
    //     BinaryOperator::Sub => left - right,
    //     BinaryOperator::Mul => left * right,
    //     BinaryOperator::Div => left / right,
    //     BinaryOperator::Pow => left.pow(right),
    //     BinaryOperator::Mod => left.modulus(right),
    //     BinaryOperator::BitXor => left ^ right,
    //     BinaryOperator::BitOr => left | right,
    //     BinaryOperator::BitAnd => Ok(match left.clone() & right.clone() {
    //         Ok(x) => x,
    //         _ => left.special_and(right, env, scope)?,
    //     }),
    //     BinaryOperator::Shl => Ok(match left.clone() << right.clone() {
    //         Ok(x) => x,
    //         _ => left.special_shl(right, env, scope)?,
    //     }),
    //     BinaryOperator::Shr => Ok(match left.clone() >> right.clone() {
    //         Ok(x) => x,
    //         _ => left.special_shr(right, env, scope)?,
    //     }),
    // }?;

    Ok(left)
}

impl RuntimeValue {
    pub fn panic_operator(
        &self,
        rhs: &Self,
        operator: &BinaryOperator,
    ) -> Result<RuntimeValue, ASTError> {
        Err(ASTError::BinaryOperator(
            self.clone(),
            rhs.clone(),
            operator.clone(),
        ))
    }
}
