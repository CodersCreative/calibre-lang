use calibre_parser::ast::{
    ObjectMap,
    binary::BinaryOperator,
    comparison::{BooleanOperator, ComparisonOperator},
};
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Shl, Shr, Sub};

use crate::{error::RuntimeError, value::RuntimeValue};
use dumpster::sync::Gc;

fn comparison_value_handle<T: PartialEq + PartialOrd>(
    op: &ComparisonOperator,
    left: T,
    right: T,
) -> bool {
    match op {
        ComparisonOperator::NotEqual => left != right,
        ComparisonOperator::Equal => left == right,
        ComparisonOperator::Lesser => left < right,
        ComparisonOperator::LesserEqual => left <= right,
        ComparisonOperator::Greater => left > right,
        ComparisonOperator::GreaterEqual => left >= right,
    }
}

pub fn comparison(
    op: &ComparisonOperator,
    left: RuntimeValue,
    right: RuntimeValue,
) -> Result<RuntimeValue, RuntimeError> {
    match (left, right, op) {
        (RuntimeValue::Int(x), RuntimeValue::Int(y), op) => {
            Ok(RuntimeValue::Bool(comparison_value_handle(op, x, y)))
        }
        (RuntimeValue::UInt(x), RuntimeValue::UInt(y), op) => {
            Ok(RuntimeValue::Bool(comparison_value_handle(op, x, y)))
        }
        (RuntimeValue::Float(x), RuntimeValue::Float(y), op) => {
            Ok(RuntimeValue::Bool(comparison_value_handle(op, x, y)))
        }
        (RuntimeValue::Bool(x), RuntimeValue::Bool(y), op) => {
            Ok(RuntimeValue::Bool(comparison_value_handle(op, x, y)))
        }
        (RuntimeValue::Char(x), RuntimeValue::Char(y), op) => {
            Ok(RuntimeValue::Bool(comparison_value_handle(op, x, y)))
        }
        (RuntimeValue::Str(x), RuntimeValue::Str(y), op) => {
            Ok(RuntimeValue::Bool(comparison_value_handle(op, x, y)))
        }
        (left, right, op) => Err(RuntimeError::Comparison(left, right, op.clone())),
    }
}

pub fn boolean(
    op: &BooleanOperator,
    left: RuntimeValue,
    right: RuntimeValue,
) -> Result<RuntimeValue, RuntimeError> {
    match (left, right, op) {
        (RuntimeValue::Bool(x), RuntimeValue::Bool(y), BooleanOperator::And) => {
            Ok(RuntimeValue::Bool(x && y))
        }
        (RuntimeValue::Bool(x), RuntimeValue::Bool(y), BooleanOperator::Or) => {
            Ok(RuntimeValue::Bool(x || y))
        }
        (left, right, op) => Err(RuntimeError::Boolean(left, right, op.clone())),
    }
}

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
            type Output = Result<RuntimeValue, RuntimeError>;
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

pub fn binary(
    op: &BinaryOperator,
    left: RuntimeValue,
    right: RuntimeValue,
) -> Result<RuntimeValue, RuntimeError> {
    Ok(match op {
        BinaryOperator::Add => left + right,
        BinaryOperator::Sub => left - right,
        BinaryOperator::Mul => left * right,
        BinaryOperator::Div => left / right,
        BinaryOperator::Pow => left.pow(right),
        BinaryOperator::Mod => left.modulus(right),
        BinaryOperator::BitXor => left ^ right,
        BinaryOperator::BitOr => left | right,
        BinaryOperator::BitAnd => match left.special_and(right) {
            Ok(x) => Ok(x),
            Err((l, r)) => l & r,
        },
        BinaryOperator::Shl => match left.special_shl(right) {
            Ok(x) => Ok(x),
            Err((l, r)) => l << r,
        },
        BinaryOperator::Shr => match left.special_shr(right) {
            Ok(x) => Ok(x),
            Err((l, r)) => l >> r,
        },
    }?)
}

impl RuntimeValue {
    pub fn panic_operator(
        &self,
        rhs: &Self,
        operator: &BinaryOperator,
    ) -> Result<RuntimeValue, RuntimeError> {
        Err(RuntimeError::Binary(
            self.clone(),
            rhs.clone(),
            operator.clone(),
        ))
    }
}

macro_rules! handle_binop_numeric {
    ($op_trait:ident, $op:tt, $rhs:ident, $self:ident) => {
        match ($self, $rhs) {
            (RuntimeValue::Float(x), RuntimeValue::Float(y)) => Ok(RuntimeValue::Float(x $op y)),
            (RuntimeValue::Int(x), RuntimeValue::Int(y)) => Ok(RuntimeValue::Int(x $op y)),
            (RuntimeValue::UInt(x), RuntimeValue::UInt(y)) => Ok(RuntimeValue::UInt(x $op y)),
            (x, y) => x.panic_operator(&y, &BinaryOperator::$op_trait),
        }
    };
}

macro_rules! impl_binop_numeric {
    ($op_trait:ident, $op_fn:ident, $op:tt) => {
        impl $op_trait for RuntimeValue {
            type Output = Result<RuntimeValue, RuntimeError>;
            fn $op_fn(self, rhs: Self) -> Self::Output {
                handle_binop_numeric!($op_trait, $op, rhs, self)
            }
        }
    };
}

impl_binop_numeric!(Add, add, +);
impl_binop_numeric!(Sub, sub, -);
impl_binop_numeric!(Mul, mul, *);
impl_binop_numeric!(Div, div, /);

impl RuntimeValue {
    fn modulus(self, rhs: Self) -> Result<RuntimeValue, RuntimeError> {
        handle_binop_numeric!(Mul, %, rhs, self)
    }

    fn special_and(self, rhs: Self) -> Result<RuntimeValue, (RuntimeValue, RuntimeValue)> {
        match self {
            Self::Char(x) => {
                let mut x = x.to_string();
                x.push_str(&rhs.to_string());
                Ok(Self::Str(x))
            }
            Self::Str(mut x) => {
                x.push_str(&rhs.to_string());
                Ok(Self::Str(x))
            }
            _ => Err((self, rhs)),
        }
    }

    fn special_shr(self, rhs: Self) -> Result<RuntimeValue, (RuntimeValue, RuntimeValue)> {
        match rhs {
            Self::Aggregate(None, data) => {
                let mut data = data.as_ref().0.0.clone();
                let key = (data.len() - 1).to_string();
                data.push((key, self));
                Ok(Self::Aggregate(
                    None,
                    Gc::new(crate::value::GcMap(ObjectMap(data))),
                ))
            }
            Self::List(data) => {
                let mut data = data.as_ref().0.clone();
                data.push(self);
                Ok(Self::List(Gc::new(crate::value::GcVec(data))))
            }
            _ => Err((self, rhs)),
        }
    }

    fn special_shl(self, rhs: Self) -> Result<RuntimeValue, (RuntimeValue, RuntimeValue)> {
        match self {
            Self::Aggregate(None, data) => {
                let mut data = data.as_ref().0.0.clone();
                let key = (data.len() - 1).to_string();
                data.push((key, rhs));
                Ok(Self::Aggregate(
                    None,
                    Gc::new(crate::value::GcMap(ObjectMap(data))),
                ))
            }
            Self::List(data) => {
                let mut data = data.as_ref().0.clone();
                data.push(rhs);
                Ok(Self::List(Gc::new(crate::value::GcVec(data))))
            }
            _ => Err((self, rhs)),
        }
    }

    fn pow(self, rhs: Self) -> Result<RuntimeValue, RuntimeError> {
        match (self, rhs) {
            (RuntimeValue::Int(x), RuntimeValue::Int(y)) => Ok(RuntimeValue::Int(x.pow(y as u32))),
            (RuntimeValue::Int(x), RuntimeValue::UInt(y)) => Ok(RuntimeValue::Int(x.pow(y as u32))),
            (RuntimeValue::Int(x), RuntimeValue::Float(y)) => {
                Ok(RuntimeValue::Int((x as f64).powf(y as f64) as i64))
            }
            (RuntimeValue::UInt(x), RuntimeValue::UInt(y)) => {
                Ok(RuntimeValue::UInt(x.pow(y as u32)))
            }
            (RuntimeValue::UInt(x), RuntimeValue::Int(y)) => {
                Ok(RuntimeValue::UInt(x.pow(y as u32)))
            }
            (RuntimeValue::UInt(x), RuntimeValue::Float(y)) => {
                Ok(RuntimeValue::UInt((x as f64).powf(y as f64) as u64))
            }
            (RuntimeValue::Float(x), RuntimeValue::Float(y)) => Ok(RuntimeValue::Float(x.powf(y))),
            (RuntimeValue::Float(x), RuntimeValue::Int(y)) => {
                Ok(RuntimeValue::Float(x.powf(y as f64)))
            }
            (RuntimeValue::Float(x), RuntimeValue::UInt(y)) => {
                Ok(RuntimeValue::Float(x.powf(y as f64)))
            }
            (lhs, rhs) => lhs.panic_operator(&rhs, &BinaryOperator::Pow),
        }
    }
}
