use crate::runtime::{interpreter::InterpreterErr, scope::CheckerEnvironment, values::RuntimeType};
use calibre_common::errors::ASTError;
use calibre_parser::ast::binary::BinaryOperator;

pub fn handle(
    op: &BinaryOperator,
    _env: &CheckerEnvironment,
    _scope: &u64,
    left: RuntimeType,
    right: RuntimeType,
) -> Result<RuntimeType, InterpreterErr> {
    if left == RuntimeType::Dynamic || right == RuntimeType::Dynamic {
        return Ok(RuntimeType::Dynamic);
    }

    let generic_handle = || -> Result<RuntimeType, InterpreterErr> {
        match (&left, &right) {
            (RuntimeType::Int, RuntimeType::Int) => Ok(RuntimeType::Int),
            (RuntimeType::Int, RuntimeType::Float) | (RuntimeType::Float, RuntimeType::Int) => {
                Ok(RuntimeType::Float)
            }
            (RuntimeType::Float, RuntimeType::Float) => Ok(RuntimeType::Float),
            _ => left.panic_operator(&right, op).map_err(|e| e.into()),
        }
    };

    let generic_bitwise_handle = || -> Result<RuntimeType, InterpreterErr> {
        match (&left, &right) {
            (RuntimeType::Int, RuntimeType::Int) => Ok(RuntimeType::Int),
            (RuntimeType::Int, RuntimeType::Bool) | (RuntimeType::Bool, RuntimeType::Int) => {
                Ok(RuntimeType::Int)
            }
            (RuntimeType::Bool, RuntimeType::Bool) => Ok(RuntimeType::Int),
            _ => left.panic_operator(&right, op).map_err(|e| e.into()),
        }
    };
    match op {
        BinaryOperator::Add | BinaryOperator::Sub | BinaryOperator::Mul | BinaryOperator::Pow => {
            generic_handle()
        }
        BinaryOperator::Div => Ok(RuntimeType::Float),
        BinaryOperator::Mod => Ok(RuntimeType::Int),
        BinaryOperator::BitXor | BinaryOperator::BitOr => generic_bitwise_handle(),
        BinaryOperator::BitAnd => match generic_bitwise_handle() {
            Ok(x) => Ok(x),
            Err(e) => match (&left, &right) {
                (RuntimeType::Str, RuntimeType::Str)
                | (RuntimeType::Char, RuntimeType::Str)
                | (RuntimeType::Str, RuntimeType::Char)
                | (RuntimeType::Char, RuntimeType::Char) => Ok(RuntimeType::Str),
                _ => Err(e),
            },
        },
        BinaryOperator::Shl => match (left.clone(), right.clone()) {
            (RuntimeType::Int, RuntimeType::Int)
            | (RuntimeType::Bool, RuntimeType::Int)
            | (RuntimeType::Int, RuntimeType::Bool)
            | (RuntimeType::Bool, RuntimeType::Bool) => Ok(RuntimeType::Int),
            (RuntimeType::List(x), y) => {
                if let Some(z) = *x.clone() {
                    if !z.is_type(&y) {
                        left.panic_operator(&right, op).map_err(|e| e.into())
                    } else {
                        Ok(RuntimeType::List(x.clone()))
                    }
                } else {
                    Ok(RuntimeType::List(x.clone()))
                }
            }
            (RuntimeType::Tuple(mut x), y) => {
                x.push(y);
                Ok(RuntimeType::Tuple(x.clone()))
            }
            _ => left.panic_operator(&right, op).map_err(|e| e.into()),
        },
        BinaryOperator::Shr => match (left.clone(), right.clone()) {
            (RuntimeType::Int, RuntimeType::Int)
            | (RuntimeType::Bool, RuntimeType::Int)
            | (RuntimeType::Int, RuntimeType::Bool)
            | (RuntimeType::Bool, RuntimeType::Bool) => Ok(RuntimeType::Int),
            (y, RuntimeType::List(x)) => {
                if let Some(z) = *x.clone() {
                    if !z.is_type(&y) {
                        left.panic_operator(&right, op).map_err(|e| e.into())
                    } else {
                        Ok(RuntimeType::List(x.clone()))
                    }
                } else {
                    Ok(RuntimeType::List(x.clone()))
                }
            }
            (y, RuntimeType::Tuple(mut x)) => {
                x.push(y);
                Ok(RuntimeType::Tuple(x.clone()))
            }
            _ => left.panic_operator(&right, op).map_err(|e| e.into()),
        },
    }
}

impl RuntimeType {
    pub fn panic_operator(
        &self,
        rhs: &Self,
        operator: &BinaryOperator,
    ) -> Result<RuntimeType, ASTError<RuntimeType>> {
        Err(ASTError::BinaryOperator(
            self.clone(),
            rhs.clone(),
            operator.clone(),
        ))
    }
}
