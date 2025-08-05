use std::ops::{BitAnd, BitOr, BitXor, Shl, Shr};

use super::binary::{ASTError, BinaryOperator};
use crate::runtime::values::RuntimeValue;

macro_rules! handle_bitwise {
    ($op_trait:ident, $op:tt, $rhs:ident, $self:ident) => {
        match $self {
            RuntimeValue::ULong(x) => match $rhs{
                RuntimeValue::Int(y) => Ok(RuntimeValue::Long((x as i128) $op y as i128)),
                RuntimeValue::UInt(y) => Ok(RuntimeValue::ULong(x $op y as u128)),
                RuntimeValue::Long(y) => Ok(RuntimeValue::Long((x as i128) $op y as i128)),
                RuntimeValue::ULong(y) => Ok(RuntimeValue::ULong(x $op y)),
                RuntimeValue::Bool(y) => Ok(RuntimeValue::ULong(x $op y as u128)),
                _ => $self.panic_operator(&$rhs, &BinaryOperator::$op_trait),
            },
            RuntimeValue::Long(x) => match $rhs{
                RuntimeValue::Int(y) => Ok(RuntimeValue::Long(x $op y as i128)),
                RuntimeValue::UInt(y) => Ok(RuntimeValue::Long(x $op y as i128)),
                RuntimeValue::Long(y) => Ok(RuntimeValue::Long(x $op y)),
                RuntimeValue::ULong(y) => Ok(RuntimeValue::Long(x $op y as i128)),
                RuntimeValue::Bool(y) => Ok(RuntimeValue::Long(x $op y as i128)),
                _ => $self.panic_operator(&$rhs, &BinaryOperator::$op_trait),
            },
            RuntimeValue::UInt(x) => match $rhs{
                RuntimeValue::Int(y) => Ok(RuntimeValue::Int((x as i64) $op y)),
                RuntimeValue::UInt(y) => Ok(RuntimeValue::UInt(x $op y)),
                RuntimeValue::Long(y) => Ok(RuntimeValue::Long((x as i128) $op y)),
                RuntimeValue::ULong(y) => Ok(RuntimeValue::ULong((x as u128) $op y)),
                RuntimeValue::Bool(y) => Ok(RuntimeValue::UInt(x $op y as u64)),
                _ => $self.panic_operator(&$rhs, &BinaryOperator::$op_trait),
            },
            RuntimeValue::Int(x) => match $rhs{
                RuntimeValue::Int(y) => Ok(RuntimeValue::Int(x $op y)),
                RuntimeValue::UInt(y) => Ok(RuntimeValue::Int(x $op y as i64)),
                RuntimeValue::Long(y) => Ok(RuntimeValue::Long((x as i128) $op y)),
                RuntimeValue::ULong(y) => Ok(RuntimeValue::Long((x as i128) $op y as i128)),
                RuntimeValue::Bool(y) => Ok(RuntimeValue::Int(x $op y as i64)),
                _ => $self.panic_operator(&$rhs, &BinaryOperator::$op_trait),
            },
            RuntimeValue::Bool(x) => match $rhs{
                RuntimeValue::Int(y) => Ok(RuntimeValue::Int((x as i64) $op y)),
                RuntimeValue::UInt(y) => Ok(RuntimeValue::Int((x as i64) $op y as i64)),
                RuntimeValue::Long(y) => Ok(RuntimeValue::Long((x as i128) $op y)),
                RuntimeValue::ULong(y) => Ok(RuntimeValue::Long((x as i128) $op y as i128)),
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
            type Output = Result<RuntimeValue, ASTError>;
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
