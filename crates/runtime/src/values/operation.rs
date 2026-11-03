use crate::values::aggregate::GcAggregate;
use crate::values::list::GcList;
use crate::values::string::GcString;
use crate::values::{BIG_CONSTS, Value, ValueTag, alloc_big};
use crate::{BIG_PRECISION, BIG_ROUNDING};
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Shl, Shr, Sub};

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinaryOperator {
    Sub,
    Add,
    Mul,
    Div,
    Pow,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ComparisonOperator {
    Greater,
    Lesser,
    LesserEqual,
    GreaterEqual,
    Equal,
    NotEqual,
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_comparison(
    op: ComparisonOperator,
    left_tag: ValueTag,
    left: i64,
    right_tag: ValueTag,
    right: i64,
) -> bool {
    let left = Value::from_tag(left_tag, left);
    let right = Value::from_tag(right_tag, right);

    if let Ok(Value::Bool(x)) = comparison(&op, left, right) {
        x
    } else {
        false
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_binary(
    op: BinaryOperator,
    left_tag: ValueTag,
    left: i64,
    right_tag: ValueTag,
    right: i64,
) -> i64 {
    let left = Value::from_tag(left_tag, left);
    let right = Value::from_tag(right_tag, right);

    match binary(&op, left, right) {
        Ok(x) => i64::from(x),
        Err(e) => panic!("{}", e),
    }
}

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

pub fn comparison(op: &ComparisonOperator, left: Value, right: Value) -> Result<Value, String> {
    fn eq_value(left: &Value, right: &Value) -> Option<bool> {
        match (left, right) {
            (Value::Null, Value::Null) => Some(true),
            (Value::Bool(a), Value::Bool(b)) => Some(a == b),
            (Value::Big(a), Value::Big(b)) => Some(a == b),
            (Value::Int(a), Value::Int(b)) => Some(a == b),
            (Value::UInt(a), Value::UInt(b)) => Some(a == b),
            (Value::Byte(a), Value::Byte(b)) => Some(a == b),
            (Value::Int(a), Value::UInt(b)) => Some(*a >= 0 && a.cast_unsigned() == *b),
            (Value::UInt(a), Value::Int(b)) => Some(*b >= 0 && *a == b.cast_unsigned()),
            (Value::UInt(a), Value::Byte(b)) => Some(*a == *b as u64),
            (Value::Byte(a), Value::UInt(b)) => Some(*a as u64 == *b),
            (Value::Int(a), Value::Byte(b)) => Some(*a >= 0 && (a.cast_unsigned() as u8) == *b),
            (Value::Byte(a), Value::Int(b)) => Some(*b >= 0 && *a == (b.cast_unsigned() as u8)),
            (Value::Float(a), Value::Float(b)) => Some(a == b),
            (Value::Char(a), Value::Char(b)) => Some(a == b),
            (Value::String(a), Value::String(b)) => Some(a == b),
            (Value::List(a), Value::List(b)) => {
                let a = unsafe { &**a };

                let b = unsafe { &**b };

                if a.data.len() != b.data.len() {
                    return Some(false);
                }

                for (left, right) in a.data.iter().zip(b.data.iter()) {
                    if !eq_value(left, right).unwrap_or(false) {
                        return Some(false);
                    }
                }

                Some(true)
            }
            _ => None,
        }
    }

    match (left, right, op) {
        (Value::Null, right, op) | (right, Value::Null, op) => match op {
            ComparisonOperator::Equal => Ok(Value::Bool(matches!(right, Value::Null))),
            ComparisonOperator::NotEqual => Ok(Value::Bool(!matches!(right, Value::Null))),
            ComparisonOperator::Lesser
            | ComparisonOperator::LesserEqual
            | ComparisonOperator::Greater
            | ComparisonOperator::GreaterEqual => Ok(Value::Bool(false)),
        },
        (Value::List(a), Value::List(b), op) => {
            let a_ref = unsafe { &*a };

            let b_ref = unsafe { &*b };

            match op {
                ComparisonOperator::Equal => Ok(Value::Bool(
                    a_ref.data.len() == b_ref.data.len()
                        && a_ref
                            .data
                            .iter()
                            .zip(b_ref.data.iter())
                            .all(|(left, right)| eq_value(left, right).unwrap_or(false)),
                )),
                ComparisonOperator::NotEqual => Ok(Value::Bool(
                    a_ref.data.len() != b_ref.data.len()
                        || a_ref
                            .data
                            .iter()
                            .zip(b_ref.data.iter())
                            .any(|(left, right)| !eq_value(left, right).unwrap_or(false)),
                )),
                _ => Err(format!(
                    "Unable to perform {op:?} on {} and {}",
                    Value::List(a),
                    Value::List(b)
                )),
            }
        }
        (Value::List(a), other, op) | (other, Value::List(a), op) => match op {
            ComparisonOperator::Equal => Ok(Value::Bool(matches!(other, Value::List(_)))),
            ComparisonOperator::NotEqual => Ok(Value::Bool(!matches!(other, Value::List(_)))),
            _ => Err(format!(
                "Unable to perform {op:?} on {} and {}",
                Value::List(a),
                other
            )),
        },
        (Value::Big(x), Value::Big(y), op) => Ok(Value::Bool(comparison_value_handle(op, x, y))),
        (Value::Int(x), Value::Int(y), op) => Ok(Value::Bool(comparison_value_handle(op, x, y))),
        (Value::UInt(x), Value::UInt(y), op) => Ok(Value::Bool(comparison_value_handle(op, x, y))),
        (Value::Byte(x), Value::Byte(y), op) => Ok(Value::Bool(comparison_value_handle(op, x, y))),
        (Value::Byte(x), Value::UInt(y), op) => {
            Ok(Value::Bool(comparison_value_handle(op, x as u64, y)))
        }
        (Value::UInt(x), Value::Byte(y), op) => {
            Ok(Value::Bool(comparison_value_handle(op, x, y as u64)))
        }
        (Value::Byte(x), Value::Int(y), op) => {
            if y < 0 {
                Ok(Value::Bool(matches!(
                    op,
                    ComparisonOperator::Greater
                        | ComparisonOperator::GreaterEqual
                        | ComparisonOperator::NotEqual
                )))
            } else {
                Ok(Value::Bool(comparison_value_handle(op, x as i64, y)))
            }
        }
        (Value::Int(x), Value::Byte(y), op) => {
            if x < 0 {
                Ok(Value::Bool(matches!(
                    op,
                    ComparisonOperator::Lesser
                        | ComparisonOperator::LesserEqual
                        | ComparisonOperator::NotEqual
                )))
            } else {
                Ok(Value::Bool(comparison_value_handle(op, x, y as i64)))
            }
        }
        (Value::Int(x), Value::UInt(y), op) => {
            if x < 0 {
                Ok(Value::Bool(matches!(
                    op,
                    ComparisonOperator::Lesser
                        | ComparisonOperator::LesserEqual
                        | ComparisonOperator::NotEqual
                )))
            } else {
                Ok(Value::Bool(comparison_value_handle(
                    op,
                    x.cast_unsigned(),
                    y,
                )))
            }
        }
        (Value::UInt(x), Value::Int(y), op) => {
            if y < 0 {
                Ok(Value::Bool(matches!(
                    op,
                    ComparisonOperator::Greater
                        | ComparisonOperator::GreaterEqual
                        | ComparisonOperator::NotEqual
                )))
            } else {
                Ok(Value::Bool(comparison_value_handle(
                    op,
                    x,
                    y.cast_unsigned(),
                )))
            }
        }
        (Value::Float(x), Value::Float(y), op) => {
            Ok(Value::Bool(comparison_value_handle(op, x, y)))
        }
        (Value::Bool(x), Value::Bool(y), op) => Ok(Value::Bool(comparison_value_handle(op, x, y))),
        (Value::Char(x), Value::Char(y), op) => Ok(Value::Bool(comparison_value_handle(op, x, y))),
        (Value::String(x), Value::String(y), op) => {
            Ok(Value::Bool(comparison_value_handle(op, x, y)))
        }
        (left, right, ComparisonOperator::Equal) => {
            Ok(Value::Bool(matches!(eq_value(&left, &right), Some(true))))
        }
        (left, right, ComparisonOperator::NotEqual) => {
            Ok(Value::Bool(!matches!(eq_value(&left, &right), Some(true))))
        }
        (left, right, op) => Err(format!("Unable to perform {op:?} on {left} and {right}")),
    }
}

macro_rules! handle_bitwise {
    ($op_trait:ident, $op:tt, $rhs:ident, $self:ident) => {
        match $self {
            Value::Int(x) => match $rhs{
                Value::Int(y) => Ok(Value::Int(x $op y)),
                Value::UInt(y) => Ok(Value::Int(x $op (y as i64))),
                Value::Byte(y) => Ok(Value::Int(x $op (y as i64))),
                Value::Bool(y) => Ok(Value::Int(x $op y as i64)),
                _ => $self.panic_operator(&$rhs, &BinaryOperator::$op_trait),
            },
            Value::UInt(x) => match $rhs{
                Value::UInt(y) => Ok(Value::UInt(x $op y)),
                Value::Byte(y) => Ok(Value::UInt(x $op y as u64)),
                Value::Int(y) => Ok(Value::Int((x as i64) $op y)),
                Value::Bool(y) => Ok(Value::UInt(x $op y as u64)),
                _ => $self.panic_operator(&$rhs, &BinaryOperator::$op_trait),
            },
            Value::Byte(x) => match $rhs{
                Value::Byte(y) => Ok(Value::Byte(x $op y)),
                Value::UInt(y) => Ok(Value::UInt((x as u64) $op y)),
                Value::Int(y) => Ok(Value::Int((x as i64) $op y)),
                Value::Bool(y) => Ok(Value::UInt((x as u64) $op y as u64)),
                _ => $self.panic_operator(&$rhs, &BinaryOperator::$op_trait),
            },
            Value::Bool(x) => match $rhs{
                Value::Int(y) => Ok(Value::Int((x as i64) $op y)),
                Value::UInt(y) => Ok(Value::UInt((x as u64) $op y)),
                Value::Byte(y) => Ok(Value::UInt((x as u64) $op y as u64)),
                Value::Bool(y) => Ok(Value::Int((x as i64) $op y as i64)),
                _ => $self.panic_operator(&$rhs, &BinaryOperator::$op_trait),
            },
            _ => $self.panic_operator(&$rhs, &BinaryOperator::$op_trait),
        }
    };
}

macro_rules! impl_bitwise {
    ($op_trait:ident, $op_fn:ident, $op:tt) => {
        impl $op_trait for Value {
            type Output = Result<Value, String>;
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

pub fn binary(op: &BinaryOperator, left: Value, right: Value) -> Result<Value, String> {
    match (left, right) {
        (Value::Big(x), Value::Big(y)) => {
            let x_ref = unsafe { &*x };

            let y_ref = unsafe { &*y };

            Ok(match op {
                BinaryOperator::Add => {
                    Value::Big(alloc_big(x_ref.add(y_ref, BIG_PRECISION, BIG_ROUNDING)))
                }
                BinaryOperator::Sub => {
                    Value::Big(alloc_big(x_ref.sub(y_ref, BIG_PRECISION, BIG_ROUNDING)))
                }
                BinaryOperator::Mul => {
                    Value::Big(alloc_big(x_ref.mul(y_ref, BIG_PRECISION, BIG_ROUNDING)))
                }
                BinaryOperator::Div => {
                    Value::Big(alloc_big(x_ref.div(y_ref, BIG_PRECISION, BIG_ROUNDING)))
                }
                BinaryOperator::Pow => {
                    if let Ok(mut consts) = BIG_CONSTS.lock() {
                        Value::Big(alloc_big(x_ref.pow(
                            y_ref,
                            BIG_PRECISION,
                            BIG_ROUNDING,
                            &mut consts,
                        )))
                    } else {
                        Value::Big(x)
                    }
                }
                _ => return Value::Big(x).panic_operator(&Value::Big(y), op),
            })
        }
        (left, right) => Ok(match op {
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
        }?),
    }
}

impl Value {
    pub fn panic_operator(&self, rhs: &Self, operator: &BinaryOperator) -> Result<Value, String> {
        Err(format!(
            "Unable to perform {operator:?} on {self} and {rhs}"
        ))
    }
}

macro_rules! handle_binop_numeric {
    ($op_trait:ident, $op:tt, $rhs:ident, $self:ident) => {
        match ($self, $rhs) {
            (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x $op y)),
            (Value::Float(x), Value::Int(y)) => {
                Ok(Value::Float(x $op y as f64))
            }
            (Value::Int(x), Value::Float(y)) => {
                Ok(Value::Float((x as f64) $op y))
            }
            (Value::Float(x), Value::UInt(y)) => {
                Ok(Value::Float(x $op y as f64))
            }
            (Value::Float(x), Value::Byte(y)) => {
                Ok(Value::Float(x $op y as f64))
            }
            (Value::UInt(x), Value::Float(y)) => {
                Ok(Value::Float((x as f64) $op y))
            }
            (Value::Byte(x), Value::Float(y)) => {
                Ok(Value::Float((x as f64) $op y))
            }
            (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x $op y)),
            (Value::UInt(x), Value::UInt(y)) => Ok(Value::UInt(x $op y)),
            (Value::Byte(x), Value::Byte(y)) => {
                Ok(Value::Byte(x $op y))
            }
            (Value::Int(x), Value::UInt(y)) => {
                Ok(Value::Int(x $op y as i64))
            }
            (Value::Int(x), Value::Byte(y)) => {
                Ok(Value::Int(x $op y as i64))
            }
            (Value::UInt(x), Value::Int(y)) => {
                Ok(Value::Int(x as i64 $op y))
            }
            (Value::UInt(x), Value::Byte(y)) => {
                Ok(Value::UInt(x $op y as u64))
            }
            (Value::Byte(x), Value::UInt(y)) => {
                Ok(Value::UInt((x as u64) $op y))
            }
            (Value::Byte(x), Value::Int(y)) => {
                Ok(Value::Int(x as i64 $op y))
            }
            (x, y) => x.panic_operator(&y, &BinaryOperator::$op_trait),
        }
    };
}

macro_rules! impl_binop_numeric {
    ($op_trait:ident, $op_fn:ident, $op:tt) => {
        impl $op_trait for Value {
            type Output = Result<Value, String>;
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

impl Value {
    fn modulus(self, rhs: Self) -> Result<Value, String> {
        handle_binop_numeric!(Mul, %, rhs, self)
    }

    fn special_and(self, rhs: Self) -> Result<Value, (Value, Value)> {
        match self {
            Self::Char(x) => {
                let rhs_str = rhs.to_string();
                let x = unsafe { char::from_u32_unchecked(x) };

                let mut data = String::with_capacity(x.len_utf8() + rhs_str.len());
                data.push(x);
                data.push_str(&rhs_str);
                Ok(Self::String(GcString { data }.alloc()))
            }
            Self::String(x) => {
                let x = unsafe { &*x };
                let rhs_str = rhs.to_string();

                let mut data = String::with_capacity(x.data.len() + rhs_str.len());
                data.push_str(x.data.as_str());
                data.push_str(&rhs_str);
                Ok(Self::String(GcString { data }.alloc()))
            }
            lhs => match rhs {
                Self::Char(x) => {
                    let lhs_str = lhs.to_string();
                    let x = unsafe { char::from_u32_unchecked(x) };

                    let mut data = String::with_capacity(lhs_str.len() + x.len_utf8());
                    data.push_str(&lhs_str);
                    data.push(x);
                    Ok(Self::String(GcString { data }.alloc()))
                }
                Self::String(x) => {
                    let x = unsafe { &*x };
                    let lhs_str = lhs.to_string();

                    let mut data = String::with_capacity(lhs_str.len() + x.data.len());
                    data.push_str(&lhs_str);
                    data.push_str(x.data.as_str());
                    Ok(Self::String(GcString { data }.alloc()))
                }
                _ => Err((lhs, rhs)),
            },
        }
    }

    fn special_shr(self, rhs: Self) -> Result<Value, (Value, Value)> {
        match rhs {
            Self::Tuple(data) => {
                let data_ref = unsafe { &*data };
                let mut data = data_ref.data.clone();
                data.push(self);
                Ok(Self::Tuple(
                    GcAggregate {
                        data,
                        field_count: data_ref.field_count + 1,
                    }
                    .alloc(),
                ))
            }
            Self::List(data) => {
                let data = unsafe { &*data };
                let mut data = data.data.clone();
                data.push(self);
                Ok(Self::List(GcList { data }.alloc()))
            }
            _ => Err((self, rhs)),
        }
    }

    fn special_shl(self, rhs: Self) -> Result<Value, (Value, Value)> {
        match self {
            Self::Tuple(data) => {
                let data_ref = unsafe { &*data };
                let mut data = data_ref.data.clone();
                data.push(self);
                Ok(Self::Tuple(
                    GcAggregate {
                        data,
                        field_count: data_ref.field_count + 1,
                    }
                    .alloc(),
                ))
            }
            Self::List(data) => {
                let data = unsafe { &*data };
                let mut data = data.data.clone();
                data.push(rhs);
                Ok(Self::List(GcList { data }.alloc()))
            }
            _ => Err((self, rhs)),
        }
    }

    fn pow(self, rhs: Self) -> Result<Value, String> {
        match (self, rhs) {
            (Value::Int(x), Value::Int(y)) => {
                if y < 0 {
                    Ok(Value::Float((x as f64).powf(y as f64)))
                } else {
                    Ok(Value::Int(x.pow(y.cast_unsigned() as u32)))
                }
            }
            (Value::Int(x), Value::UInt(y)) => Ok(Value::Int(x.pow(y as u32))),
            (Value::Int(x), Value::Byte(y)) => Ok(Value::Int(x.pow(y as u32))),
            (Value::Int(x), Value::Float(y)) => Ok(Value::Int((x as f64).powf(y) as i64)),
            (Value::UInt(x), Value::UInt(y)) => Ok(Value::UInt(x.pow(y as u32))),
            (Value::UInt(x), Value::Byte(y)) => Ok(Value::UInt(x.pow(y as u32))),
            (Value::UInt(x), Value::Int(y)) => {
                if y < 0 {
                    Ok(Value::Float((x as f64).powf(y as f64)))
                } else {
                    Ok(Value::UInt(x.pow(y.cast_unsigned() as u32)))
                }
            }
            (Value::UInt(x), Value::Float(y)) => Ok(Value::UInt((x as f64).powf(y) as u64)),
            (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x.powf(y))),
            (Value::Float(x), Value::Int(y)) => Ok(Value::Float(x.powf(y as f64))),
            (Value::Float(x), Value::UInt(y)) => Ok(Value::Float(x.powf(y as f64))),
            (Value::Float(x), Value::Byte(y)) => Ok(Value::Float(x.powf(y as f64))),
            (Value::Byte(x), Value::Byte(y)) => Ok(Value::Byte(x.pow(y as u32))),
            (Value::Byte(x), Value::UInt(y)) => Ok(Value::UInt((x as u64).pow(y as u32))),
            (Value::Byte(x), Value::Int(y)) => {
                if y < 0 {
                    Ok(Value::Float((x as f64).powf(y as f64)))
                } else {
                    Ok(Value::UInt((x as u64).pow(y.cast_unsigned() as u32)))
                }
            }
            (Value::Byte(x), Value::Float(y)) => Ok(Value::Float((x as f64).powf(y))),
            (lhs, rhs) => lhs.panic_operator(&rhs, &BinaryOperator::Pow),
        }
    }
}
