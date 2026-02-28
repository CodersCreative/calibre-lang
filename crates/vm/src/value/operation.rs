use calibre_parser::ast::{
    binary::BinaryOperator,
    comparison::{BooleanOperator, ComparisonOperator},
};
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Shl, Shr, Sub};

use crate::{VM, error::RuntimeError, value::RuntimeValue};
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
    fn eq_value(left: &RuntimeValue, right: &RuntimeValue) -> Option<bool> {
        match (left, right) {
            (RuntimeValue::Null, RuntimeValue::Null) => Some(true),
            (RuntimeValue::Bool(a), RuntimeValue::Bool(b)) => Some(a == b),
            (RuntimeValue::Int(a), RuntimeValue::Int(b)) => Some(a == b),
            (RuntimeValue::UInt(a), RuntimeValue::UInt(b)) => Some(a == b),
            (RuntimeValue::Byte(a), RuntimeValue::Byte(b)) => Some(a == b),
            (RuntimeValue::Int(a), RuntimeValue::UInt(b)) => Some(*a >= 0 && (*a as u64) == *b),
            (RuntimeValue::UInt(a), RuntimeValue::Int(b)) => Some(*b >= 0 && *a == (*b as u64)),
            (RuntimeValue::UInt(a), RuntimeValue::Byte(b)) => Some(*a == *b as u64),
            (RuntimeValue::Byte(a), RuntimeValue::UInt(b)) => Some(*a as u64 == *b),
            (RuntimeValue::Int(a), RuntimeValue::Byte(b)) => Some(*a >= 0 && (*a as u8) == *b),
            (RuntimeValue::Byte(a), RuntimeValue::Int(b)) => Some(*b >= 0 && *a == (*b as u8)),
            (RuntimeValue::Float(a), RuntimeValue::Float(b)) => Some(a == b),
            (RuntimeValue::Char(a), RuntimeValue::Char(b)) => Some(a == b),
            (RuntimeValue::Str(a), RuntimeValue::Str(b)) => Some(a == b),
            (RuntimeValue::Enum(a_name, a_idx, _), RuntimeValue::Enum(b_name, b_idx, _)) => {
                Some(a_name == b_name && a_idx == b_idx)
            }
            (RuntimeValue::Option(a), RuntimeValue::Option(b)) => match (a, b) {
                (None, None) => Some(true),
                (Some(_), None) | (None, Some(_)) => Some(false),
                (Some(a), Some(b)) => eq_value(a, b),
            },
            _ => None,
        }
    }

    match (left, right, op) {
        (RuntimeValue::Option(a), RuntimeValue::Option(b), op) => match op {
            ComparisonOperator::Equal | ComparisonOperator::NotEqual => {
                let eq = match (a, b) {
                    (None, None) => true,
                    (Some(_), None) | (None, Some(_)) => false,
                    (Some(a), Some(b)) => eq_value(&a, &b).unwrap_or(false),
                };
                Ok(RuntimeValue::Bool(
                    if matches!(op, ComparisonOperator::Equal) {
                        eq
                    } else {
                        !eq
                    },
                ))
            }
            _ => Err(RuntimeError::Comparison(
                RuntimeValue::Option(a),
                RuntimeValue::Option(b),
                op.clone(),
            )),
        },
        (RuntimeValue::Enum(a_name, a_idx, _), RuntimeValue::Enum(b_name, b_idx, _), op) => {
            match op {
                ComparisonOperator::Equal => {
                    Ok(RuntimeValue::Bool(a_name == b_name && a_idx == b_idx))
                }
                ComparisonOperator::NotEqual => {
                    Ok(RuntimeValue::Bool(a_name != b_name || a_idx != b_idx))
                }
                _ => Err(RuntimeError::Comparison(
                    RuntimeValue::Enum(a_name, a_idx, None),
                    RuntimeValue::Enum(b_name, b_idx, None),
                    op.clone(),
                )),
            }
        }
        (RuntimeValue::Int(x), RuntimeValue::Int(y), op) => {
            Ok(RuntimeValue::Bool(comparison_value_handle(op, x, y)))
        }
        (RuntimeValue::UInt(x), RuntimeValue::UInt(y), op) => {
            Ok(RuntimeValue::Bool(comparison_value_handle(op, x, y)))
        }
        (RuntimeValue::Byte(x), RuntimeValue::Byte(y), op) => {
            Ok(RuntimeValue::Bool(comparison_value_handle(op, x, y)))
        }
        (RuntimeValue::Byte(x), RuntimeValue::UInt(y), op) => {
            Ok(RuntimeValue::Bool(comparison_value_handle(op, x as u64, y)))
        }
        (RuntimeValue::UInt(x), RuntimeValue::Byte(y), op) => {
            Ok(RuntimeValue::Bool(comparison_value_handle(op, x, y as u64)))
        }
        (RuntimeValue::Byte(x), RuntimeValue::Int(y), op) => {
            if y < 0 {
                Ok(RuntimeValue::Bool(matches!(
                    op,
                    ComparisonOperator::Greater
                        | ComparisonOperator::GreaterEqual
                        | ComparisonOperator::NotEqual
                )))
            } else {
                Ok(RuntimeValue::Bool(comparison_value_handle(op, x as i64, y)))
            }
        }
        (RuntimeValue::Int(x), RuntimeValue::Byte(y), op) => {
            if x < 0 {
                Ok(RuntimeValue::Bool(matches!(
                    op,
                    ComparisonOperator::Lesser
                        | ComparisonOperator::LesserEqual
                        | ComparisonOperator::NotEqual
                )))
            } else {
                Ok(RuntimeValue::Bool(comparison_value_handle(
                    op, x as i64, y as i64,
                )))
            }
        }
        (RuntimeValue::Int(x), RuntimeValue::UInt(y), op) => {
            if x < 0 {
                Ok(RuntimeValue::Bool(matches!(
                    op,
                    ComparisonOperator::Lesser
                        | ComparisonOperator::LesserEqual
                        | ComparisonOperator::NotEqual
                )))
            } else {
                Ok(RuntimeValue::Bool(comparison_value_handle(op, x as u64, y)))
            }
        }
        (RuntimeValue::UInt(x), RuntimeValue::Int(y), op) => {
            if y < 0 {
                Ok(RuntimeValue::Bool(matches!(
                    op,
                    ComparisonOperator::Greater
                        | ComparisonOperator::GreaterEqual
                        | ComparisonOperator::NotEqual
                )))
            } else {
                Ok(RuntimeValue::Bool(comparison_value_handle(op, x, y as u64)))
            }
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
                RuntimeValue::UInt(y) => Ok(RuntimeValue::Int(x $op (y as i64))),
                RuntimeValue::Byte(y) => Ok(RuntimeValue::Int(x $op (y as i64))),
                RuntimeValue::Bool(y) => Ok(RuntimeValue::Int(x $op y as i64)),
                _ => $self.panic_operator(&$rhs, &BinaryOperator::$op_trait),
            },
            RuntimeValue::UInt(x) => match $rhs{
                RuntimeValue::UInt(y) => Ok(RuntimeValue::UInt(x $op y)),
                RuntimeValue::Byte(y) => Ok(RuntimeValue::UInt(x $op y as u64)),
                RuntimeValue::Int(y) => Ok(RuntimeValue::Int((x as i64) $op y)),
                RuntimeValue::Bool(y) => Ok(RuntimeValue::UInt(x $op y as u64)),
                _ => $self.panic_operator(&$rhs, &BinaryOperator::$op_trait),
            },
            RuntimeValue::Byte(x) => match $rhs{
                RuntimeValue::Byte(y) => Ok(RuntimeValue::Byte(x $op y)),
                RuntimeValue::UInt(y) => Ok(RuntimeValue::UInt((x as u64) $op y)),
                RuntimeValue::Int(y) => Ok(RuntimeValue::Int((x as i64) $op y)),
                RuntimeValue::Bool(y) => Ok(RuntimeValue::UInt((x as u64) $op y as u64)),
                _ => $self.panic_operator(&$rhs, &BinaryOperator::$op_trait),
            },
            RuntimeValue::Bool(x) => match $rhs{
                RuntimeValue::Int(y) => Ok(RuntimeValue::Int((x as i64) $op y)),
                RuntimeValue::UInt(y) => Ok(RuntimeValue::UInt((x as u64) $op y)),
                RuntimeValue::Byte(y) => Ok(RuntimeValue::UInt((x as u64) $op y as u64)),
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
    vm: &mut VM,
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
        BinaryOperator::BitAnd => match left.special_and(vm, right) {
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
            (RuntimeValue::Float(x), RuntimeValue::Int(y)) => {
                Ok(RuntimeValue::Float(x $op y as f64))
            }
            (RuntimeValue::Int(x), RuntimeValue::Float(y)) => {
                Ok(RuntimeValue::Float((x as f64) $op y))
            }
            (RuntimeValue::Float(x), RuntimeValue::UInt(y)) => {
                Ok(RuntimeValue::Float(x $op y as f64))
            }
            (RuntimeValue::Float(x), RuntimeValue::Byte(y)) => {
                Ok(RuntimeValue::Float(x $op y as f64))
            }
            (RuntimeValue::UInt(x), RuntimeValue::Float(y)) => {
                Ok(RuntimeValue::Float((x as f64) $op y))
            }
            (RuntimeValue::Byte(x), RuntimeValue::Float(y)) => {
                Ok(RuntimeValue::Float((x as f64) $op y))
            }
            (RuntimeValue::Int(x), RuntimeValue::Int(y)) => Ok(RuntimeValue::Int(x $op y)),
            (RuntimeValue::UInt(x), RuntimeValue::UInt(y)) => Ok(RuntimeValue::UInt(x $op y)),
            (RuntimeValue::Byte(x), RuntimeValue::Byte(y)) => {
                Ok(RuntimeValue::Byte(x $op y))
            }
            (RuntimeValue::Int(x), RuntimeValue::UInt(y)) => {
                Ok(RuntimeValue::Int(x $op y as i64))
            }
            (RuntimeValue::Int(x), RuntimeValue::Byte(y)) => {
                Ok(RuntimeValue::Int(x $op y as i64))
            }
            (RuntimeValue::UInt(x), RuntimeValue::Int(y)) => {
                Ok(RuntimeValue::Int(x as i64 $op y))
            }
            (RuntimeValue::UInt(x), RuntimeValue::Byte(y)) => {
                Ok(RuntimeValue::UInt(x $op y as u64))
            }
            (RuntimeValue::Byte(x), RuntimeValue::UInt(y)) => {
                Ok(RuntimeValue::UInt((x as u64) $op y))
            }
            (RuntimeValue::Byte(x), RuntimeValue::Int(y)) => {
                Ok(RuntimeValue::Int(x as i64 $op y))
            }
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

    fn special_and(
        self,
        vm: &mut VM,
        rhs: Self,
    ) -> Result<RuntimeValue, (RuntimeValue, RuntimeValue)> {
        match self {
            Self::Char(x) => {
                let mut s = x.to_string();
                s.push_str(&vm.display_value(&rhs));
                Ok(Self::Str(std::sync::Arc::new(s)))
            }
            Self::Str(x) => {
                let mut s = x.as_str().to_string();
                s.push_str(&vm.display_value(&rhs));
                Ok(Self::Str(std::sync::Arc::new(s)))
            }
            lhs => match rhs {
                Self::Char(x) => {
                    let mut s = x.to_string();
                    s.push_str(&vm.display_value(&lhs));
                    Ok(Self::Str(std::sync::Arc::new(s)))
                }
                Self::Str(x) => {
                    let mut s = x.as_str().to_string();
                    s.push_str(&vm.display_value(&lhs));
                    Ok(Self::Str(std::sync::Arc::new(s)))
                }
                _ => Err((lhs, rhs)),
            },
        }
    }

    fn special_shr(self, rhs: Self) -> Result<RuntimeValue, (RuntimeValue, RuntimeValue)> {
        match rhs {
            Self::Aggregate(None, data) => {
                let mut data = data.clone();
                let entries = &mut Gc::make_mut(&mut data).0.0;
                let key = entries.len().to_string();
                entries.push((key, self));
                Ok(Self::Aggregate(None, data))
            }
            Self::List(data) => {
                let mut data = data.clone();
                Gc::make_mut(&mut data).0.push(self);
                Ok(Self::List(data))
            }
            _ => Err((self, rhs)),
        }
    }

    fn special_shl(self, rhs: Self) -> Result<RuntimeValue, (RuntimeValue, RuntimeValue)> {
        match self {
            Self::Aggregate(None, data) => {
                let mut data = data.clone();
                let entries = &mut Gc::make_mut(&mut data).0.0;
                let key = entries.len().to_string();
                entries.push((key, rhs));
                Ok(Self::Aggregate(None, data))
            }
            Self::List(data) => {
                let mut data = data.clone();
                Gc::make_mut(&mut data).0.push(rhs);
                Ok(Self::List(data))
            }
            _ => Err((self, rhs)),
        }
    }

    fn pow(self, rhs: Self) -> Result<RuntimeValue, RuntimeError> {
        match (self, rhs) {
            (RuntimeValue::Int(x), RuntimeValue::Int(y)) => {
                if y < 0 {
                    Ok(RuntimeValue::Float((x as f64).powf(y as f64)))
                } else {
                    Ok(RuntimeValue::Int(x.pow(y as u32)))
                }
            }
            (RuntimeValue::Int(x), RuntimeValue::UInt(y)) => Ok(RuntimeValue::Int(x.pow(y as u32))),
            (RuntimeValue::Int(x), RuntimeValue::Byte(y)) => Ok(RuntimeValue::Int(x.pow(y as u32))),
            (RuntimeValue::Int(x), RuntimeValue::Float(y)) => {
                Ok(RuntimeValue::Int((x as f64).powf(y as f64) as i64))
            }
            (RuntimeValue::UInt(x), RuntimeValue::UInt(y)) => {
                Ok(RuntimeValue::UInt(x.pow(y as u32)))
            }
            (RuntimeValue::UInt(x), RuntimeValue::Byte(y)) => {
                Ok(RuntimeValue::UInt(x.pow(y as u32)))
            }
            (RuntimeValue::UInt(x), RuntimeValue::Int(y)) => {
                if y < 0 {
                    Ok(RuntimeValue::Float((x as f64).powf(y as f64)))
                } else {
                    Ok(RuntimeValue::UInt(x.pow(y as u32)))
                }
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
            (RuntimeValue::Float(x), RuntimeValue::Byte(y)) => {
                Ok(RuntimeValue::Float(x.powf(y as f64)))
            }
            (RuntimeValue::Byte(x), RuntimeValue::Byte(y)) => {
                Ok(RuntimeValue::Byte(x.pow(y as u32)))
            }
            (RuntimeValue::Byte(x), RuntimeValue::UInt(y)) => {
                Ok(RuntimeValue::UInt((x as u64).pow(y as u32)))
            }
            (RuntimeValue::Byte(x), RuntimeValue::Int(y)) => {
                if y < 0 {
                    Ok(RuntimeValue::Float((x as f64).powf(y as f64)))
                } else {
                    Ok(RuntimeValue::UInt((x as u64).pow(y as u32)))
                }
            }
            (RuntimeValue::Byte(x), RuntimeValue::Float(y)) => {
                Ok(RuntimeValue::Float((x as f64).powf(y as f64)))
            }
            (lhs, rhs) => lhs.panic_operator(&rhs, &BinaryOperator::Pow),
        }
    }
}
