use calibre_parser::ast::{
    binary::BinaryOperator,
    comparison::{BooleanOperator, ComparisonOperator},
};
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Shl, Shr, Sub};

use crate::{error::RuntimeError, value::RuntimeValue};

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
    match left {
        RuntimeValue::Int(x) => match right {
            RuntimeValue::Int(y) => Ok(RuntimeValue::Bool(comparison_value_handle(op, x, y))),
            right => Err(RuntimeError::Comparison(
                RuntimeValue::Int(x),
                right,
                op.clone(),
            )),
        },
        RuntimeValue::Float(x) => match right {
            RuntimeValue::Float(y) => Ok(RuntimeValue::Bool(comparison_value_handle(op, x, y))),
            right => Err(RuntimeError::Comparison(
                RuntimeValue::Float(x),
                right,
                op.clone(),
            )),
        },
        RuntimeValue::Char(x) => match right {
            RuntimeValue::Char(y) => Ok(RuntimeValue::Bool(comparison_value_handle(op, x, y))),
            right => Err(RuntimeError::Comparison(
                RuntimeValue::Char(x),
                right,
                op.clone(),
            )),
        },
        RuntimeValue::Str(x) => match right {
            RuntimeValue::Str(y) => Ok(RuntimeValue::Bool(comparison_value_handle(op, x, y))),
            right => Err(RuntimeError::Comparison(
                RuntimeValue::Str(x),
                right,
                op.clone(),
            )),
        },
        _ => Err(RuntimeError::Comparison(left, right, op.clone())),
    }
}

pub fn boolean(
    op: &BooleanOperator,
    left: &RuntimeValue,
    right: &RuntimeValue,
) -> Result<RuntimeValue, RuntimeError> {
    if let RuntimeValue::Bool(x) = left {
        if *x && op == &BooleanOperator::Or {
            return Ok(RuntimeValue::Bool(true));
        } else if !x && op == &BooleanOperator::And {
            return Ok(RuntimeValue::Bool(false));
        }
        if let RuntimeValue::Bool(y) = right {
            return Ok(RuntimeValue::Bool(match op {
                BooleanOperator::And => *x && *y,
                BooleanOperator::Or => *x || *y,
            }));
        }
    }

    Err(RuntimeError::Boolean(
        left.clone(),
        right.clone(),
        op.clone(),
    ))
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
        BinaryOperator::BitAnd => Ok(match left.clone() & right.clone() {
            Ok(x) => x,
            _ => left.special_and(right)?,
        }),
        BinaryOperator::Shl => Ok(match left.clone() << right.clone() {
            Ok(x) => x,
            _ => left.special_shl(right)?,
        }),
        BinaryOperator::Shr => Ok(match left.clone() >> right.clone() {
            Ok(x) => x,
            _ => left.special_shr(right)?,
        }),
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
        match $self {
            RuntimeValue::Float(x) => match $rhs{
                RuntimeValue::Int(y) => Ok(RuntimeValue::Float(x $op y as f64)),
                RuntimeValue::Float(y) => Ok(RuntimeValue::Float(x $op y)),
                _ => $self.panic_operator(&$rhs, &BinaryOperator::$op_trait),
            },
            RuntimeValue::Int(x) => match $rhs{
                RuntimeValue::Int(y) => Ok(RuntimeValue::Int(x $op y)),
                RuntimeValue::Float(y) => Ok(RuntimeValue::Float(x as f64 $op y)),
                _ => $self.panic_operator(&$rhs, &BinaryOperator::$op_trait),
            },
            _ => $self.panic_operator(&$rhs, &BinaryOperator::$op_trait),
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

    fn special_and(self, rhs: Self) -> Result<RuntimeValue, RuntimeError> {
        let add_str = || -> Result<RuntimeValue, RuntimeError> {
            let mut x = rhs.to_string();
            x.push_str(&self.to_string());
            Ok(RuntimeValue::Str(x))
        };

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
            _ => match rhs {
                Self::Str(_) => add_str(),
                Self::Char(_) => add_str(),
                _ => self.panic_operator(&rhs, &BinaryOperator::BitAnd),
            },
        }
    }

    fn special_shr(self, rhs: Self) -> Result<RuntimeValue, RuntimeError> {
        match rhs {
            Self::Aggregate(None, mut data) => {
                let key = (data.len() - 1).to_string();
                data.push((key, self));
                Ok(Self::Aggregate(None, data))
            }
            Self::List(mut data) => {
                data.push(self);
                Ok(Self::List(data))
            }
            _ => match rhs {
                _ => self.panic_operator(&rhs, &BinaryOperator::Shl),
            },
        }
    }

    fn special_shl(self, rhs: Self) -> Result<RuntimeValue, RuntimeError> {
        match self {
            Self::Aggregate(None, mut data) => {
                let key = (data.len() - 1).to_string();
                data.push((key, rhs));
                Ok(Self::Aggregate(None, data))
            }
            Self::List(mut data) => {
                data.push(rhs);
                Ok(Self::List(data))
            }
            _ => match rhs {
                _ => self.panic_operator(&rhs, &BinaryOperator::Shl),
            },
        }
    }

    fn pow(self, rhs: Self) -> Result<RuntimeValue, RuntimeError> {
        match self {
            RuntimeValue::Int(x) => match rhs {
                RuntimeValue::Int(y) => Ok(RuntimeValue::Int(x.pow(y as u32))),
                RuntimeValue::Float(y) => Ok(RuntimeValue::Float((x as f64).powf(y))),
                _ => self.panic_operator(&rhs, &BinaryOperator::Pow),
            },
            RuntimeValue::Float(x) => match rhs {
                RuntimeValue::Int(y) => Ok(RuntimeValue::Float(x.powf(y as f64))),
                RuntimeValue::Float(y) => Ok(RuntimeValue::Float(x.powf(y))),
                _ => self.panic_operator(&rhs, &BinaryOperator::Pow),
            },
            _ => self.panic_operator(&rhs, &BinaryOperator::Pow),
        }
    }
}
