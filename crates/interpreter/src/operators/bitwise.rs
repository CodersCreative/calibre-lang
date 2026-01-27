use crate::runtime::values::RuntimeValue;
use calibre_common::errors::ASTError;
use calibre_parser::ast::binary::BinaryOperator;
use std::ops::{BitAnd, BitOr, BitXor, Shl, Shr};

macro_rules! handle_bitwise {
    ($op_trait:ident, $op:tt, $rhs:ident, $self:ident) => {
        match $self {
            RuntimeValue::Int(x) => match $rhs{
                RuntimeValue::Int(y) => Ok(RuntimeValue::Int(x $op y)),
                RuntimeValue::Bool(y) => Ok(RuntimeValue::Int(x $op y as i64)),
                _ => $self.panic_operator(&$rhs, &BinaryOperator::$op_trait),
            },
            RuntimeValue::Bool(x) => match $rhs{
                RuntimeValue::Int(y) => Ok(RuntimeValue::Int((x as i64) $op y)),
                RuntimeValue::Bool(y) => Ok(RuntimeValue::Int((x as i64) $op y as i64)),
                _ => $self.panic_operator(&$rhs, &BinaryOperator::$op_trait),
            },
            _ => $self.panic_operator(&$rhs, &BinaryOperator::$op_trait),
        }
    };
}

macro_rules! impl_bitwise {
    ($op_trait:ident, $op_fn:ident, $op:tt) => {
        impl $op_trait for RuntimeValue {
            type Output = Result<RuntimeValue, ASTError<RuntimeValue>>;
            fn $op_fn(self, rhs: Self) -> Self::Output {
                handle_bitwise!($op_trait, $op, rhs, self)
            }
        }
    };
}

impl_bitwise!(BitXor, bitxor, ^);
impl_bitwise!(BitAnd, bitand, &);
impl_bitwise!(BitOr, bitor, |);
impl_bitwise!(Shl, shl, <<);
impl_bitwise!(Shr, shr, >>);
