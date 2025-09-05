use crate::runtime::{interpreter::InterpreterErr, scope::Environment, values::RuntimeValue};
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
    left: RuntimeValue,
    right: RuntimeValue,
) -> Result<RuntimeValue, InterpreterErr> {
    let left = left.unwrap_val(env, scope)?;
    let right = right.unwrap_val(env, scope)?;

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
            _ => left.special_and(right, env, scope)?,
        }),
        BinaryOperator::Shl => Ok(match left.clone() << right.clone() {
            Ok(x) => x,
            _ => left.special_shl(right, env, scope)?,
        }),
        BinaryOperator::Shr => Ok(match left.clone() >> right.clone() {
            Ok(x) => x,
            _ => left.special_shr(right, env, scope)?,
        }),
    }?)
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

macro_rules! handle_binop_numeric {
    ($op_trait:ident, $op:tt, $rhs:ident, $self:ident) => {
        match $self {
            RuntimeValue::Double(x) => match $rhs{
                RuntimeValue::Int(y) => Ok(RuntimeValue::Double(x $op y as f64)),
                RuntimeValue::UInt(y) => Ok(RuntimeValue::Double(x $op y as f64)),
                RuntimeValue::Long(y) => Ok(RuntimeValue::Double(x $op y as f64)),
                RuntimeValue::ULong(y) => Ok(RuntimeValue::Double(x $op y as f64)),
                RuntimeValue::Float(y) => Ok(RuntimeValue::Double(x $op y as f64)),
                RuntimeValue::Double(y) => Ok(RuntimeValue::Double(x $op y)),
                _ => $self.panic_operator(&$rhs, &BinaryOperator::$op_trait),
            },
            RuntimeValue::Float(x) => match $rhs{
                RuntimeValue::Int(y) => Ok(RuntimeValue::Float(x $op y as f32)),
                RuntimeValue::UInt(y) => Ok(RuntimeValue::Float(x $op y as f32)),
                RuntimeValue::Long(y) => Ok(RuntimeValue::Double(x as f64 $op y as f64)),
                RuntimeValue::ULong(y) => Ok(RuntimeValue::Double(x as f64 $op y as f64)),
                RuntimeValue::Float(y) => Ok(RuntimeValue::Float(x $op y)),
                RuntimeValue::Double(y) => Ok(RuntimeValue::Double(x as f64 $op y)),
                _ => $self.panic_operator(&$rhs, &BinaryOperator::$op_trait),
            },
            RuntimeValue::ULong(x) => match $rhs{
                RuntimeValue::Int(y) => Ok(RuntimeValue::Long(x as i128 $op y as i128)),
                RuntimeValue::UInt(y) => Ok(RuntimeValue::ULong(x $op y as u128)),
                RuntimeValue::Long(y) => Ok(RuntimeValue::Long(x as i128 $op y as i128)),
                RuntimeValue::ULong(y) => Ok(RuntimeValue::ULong(x $op y)),
                RuntimeValue::Float(y) => Ok(RuntimeValue::Double(x as f64 $op y as f64)),
                RuntimeValue::Double(y) => Ok(RuntimeValue::Double(x as f64 $op y)),
                _ => $self.panic_operator(&$rhs, &BinaryOperator::$op_trait),
            },
            RuntimeValue::Long(x) => match $rhs{
                RuntimeValue::Int(y) => Ok(RuntimeValue::Long(x $op y as i128)),
                RuntimeValue::UInt(y) => Ok(RuntimeValue::Long(x $op y as i128)),
                RuntimeValue::Long(y) => Ok(RuntimeValue::Long(x $op y)),
                RuntimeValue::ULong(y) => Ok(RuntimeValue::Long(x $op y as i128)),
                RuntimeValue::Float(y) => Ok(RuntimeValue::Double(x as f64 $op y as f64)),
                RuntimeValue::Double(y) => Ok(RuntimeValue::Double(x as f64 $op y)),
                _ => $self.panic_operator(&$rhs, &BinaryOperator::$op_trait),
            },
            RuntimeValue::UInt(x) => match $rhs{
                RuntimeValue::Int(y) => Ok(RuntimeValue::Int(x as i64 $op y)),
                RuntimeValue::UInt(y) => Ok(RuntimeValue::UInt(x $op y)),
                RuntimeValue::Long(y) => Ok(RuntimeValue::Long(x as i128 $op y)),
                RuntimeValue::ULong(y) => Ok(RuntimeValue::ULong(x as u128 $op y)),
                RuntimeValue::Float(y) => Ok(RuntimeValue::Float(x as f32 $op y)),
                RuntimeValue::Double(y) => Ok(RuntimeValue::Double(x as f64 $op y)),
                _ => $self.panic_operator(&$rhs, &BinaryOperator::$op_trait),
            },
            RuntimeValue::Int(x) => match $rhs{
                RuntimeValue::Int(y) => Ok(RuntimeValue::Int(x $op y)),
                RuntimeValue::UInt(y) => Ok(RuntimeValue::Int(x $op y as i64)),
                RuntimeValue::Long(y) => Ok(RuntimeValue::Long(x as i128 $op y)),
                RuntimeValue::ULong(y) => Ok(RuntimeValue::Long(x as i128 $op y as i128)),
                RuntimeValue::Float(y) => Ok(RuntimeValue::Float(x as f32 $op y)),
                RuntimeValue::Double(y) => Ok(RuntimeValue::Double(x as f64 $op y)),
                _ => $self.panic_operator(&$rhs, &BinaryOperator::$op_trait),
            },
            _ => $self.panic_operator(&$rhs, &BinaryOperator::$op_trait),
        }
    };
}

macro_rules! impl_binop_numeric {
    ($op_trait:ident, $op_fn:ident, $op:tt) => {
        impl $op_trait for RuntimeValue {
            type Output = Result<RuntimeValue, ASTError>;
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
    fn modulus(self, rhs: Self) -> Result<RuntimeValue, ASTError> {
        handle_binop_numeric!(Mul, %, rhs, self)
    }

    fn special_and(
        self,
        rhs: Self,
        _env: &Environment,
        _scope: &u64,
    ) -> Result<RuntimeValue, ASTError> {
        let add_str = || -> Result<RuntimeValue, ASTError> {
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

    fn special_shr(
        self,
        rhs: Self,
        _env: &Environment,
        _scope: &u64,
    ) -> Result<RuntimeValue, ASTError> {
        match rhs {
            Self::Tuple(mut data) => {
                data.push(self);
                Ok(Self::Tuple(data))
            }
            Self::List {
                mut data,
                data_type,
            } => {
                data.push(self);
                Ok(Self::List {
                    data,
                    data_type: data_type.clone(),
                })
            }
            _ => match rhs {
                _ => self.panic_operator(&rhs, &BinaryOperator::Shl),
            },
        }
    }

    fn special_shl(
        self,
        rhs: Self,
        _env: &Environment,
        _scope: &u64,
    ) -> Result<RuntimeValue, ASTError> {
        match self {
            Self::Tuple(mut data) => {
                data.push(rhs);
                Ok(Self::Tuple(data))
            }
            Self::List {
                mut data,
                data_type,
            } => {
                data.push(rhs);
                Ok(Self::List {
                    data,
                    data_type: data_type.clone(),
                })
            }
            _ => match rhs {
                _ => self.panic_operator(&rhs, &BinaryOperator::Shl),
            },
        }
    }

    fn pow(self, rhs: Self) -> Result<RuntimeValue, ASTError> {
        match self {
            RuntimeValue::Int(x) => match rhs {
                RuntimeValue::Int(y) => Ok(RuntimeValue::Int(x.pow(y as u32))),
                RuntimeValue::UInt(y) => Ok(RuntimeValue::Int(x.pow(y as u32))),
                RuntimeValue::Long(y) => Ok(RuntimeValue::Int(x.pow(y as u32))),
                RuntimeValue::ULong(y) => Ok(RuntimeValue::Int(x.pow(y as u32))),
                RuntimeValue::Float(y) => Ok(RuntimeValue::Float((x as f32).powf(y))),
                RuntimeValue::Double(y) => Ok(RuntimeValue::Double((x as f64).powf(y))),
                _ => self.panic_operator(&rhs, &BinaryOperator::Pow),
            },
            RuntimeValue::Float(x) => match rhs {
                RuntimeValue::Int(y) => Ok(RuntimeValue::Float(x.powf(y as f32))),
                RuntimeValue::UInt(y) => Ok(RuntimeValue::Float(x.powf(y as f32))),
                RuntimeValue::Long(y) => Ok(RuntimeValue::Float(x.powf(y as f32))),
                RuntimeValue::ULong(y) => Ok(RuntimeValue::Float(x.powf(y as f32))),
                RuntimeValue::Float(y) => Ok(RuntimeValue::Float(x.powf(y))),
                RuntimeValue::Double(y) => Ok(RuntimeValue::Double((x as f64).powf(y))),
                _ => self.panic_operator(&rhs, &BinaryOperator::Pow),
            },
            RuntimeValue::Double(x) => match rhs {
                RuntimeValue::Int(y) => Ok(RuntimeValue::Double(x.powf(y as f64))),
                RuntimeValue::UInt(y) => Ok(RuntimeValue::Double(x.powf(y as f64))),
                RuntimeValue::Long(y) => Ok(RuntimeValue::Double(x.powf(y as f64))),
                RuntimeValue::ULong(y) => Ok(RuntimeValue::Double(x.powf(y as f64))),
                RuntimeValue::Float(y) => Ok(RuntimeValue::Double(x.powf(y as f64))),
                RuntimeValue::Double(y) => Ok(RuntimeValue::Double(x.powf(y))),
                _ => self.panic_operator(&rhs, &BinaryOperator::Pow),
            },
            RuntimeValue::Long(x) => match rhs {
                RuntimeValue::Int(y) => Ok(RuntimeValue::Long(x.pow(y as u32))),
                RuntimeValue::UInt(y) => Ok(RuntimeValue::Long(x.pow(y as u32))),
                RuntimeValue::Long(y) => Ok(RuntimeValue::Long(x.pow(y as u32))),
                RuntimeValue::ULong(y) => Ok(RuntimeValue::Long(x.pow(y as u32))),
                RuntimeValue::Float(y) => Ok(RuntimeValue::Double((x as f64).powf(y as f64))),
                RuntimeValue::Double(y) => Ok(RuntimeValue::Double((x as f64).powf(y))),
                _ => self.panic_operator(&rhs, &BinaryOperator::Pow),
            },
            RuntimeValue::UInt(x) => match rhs {
                RuntimeValue::Int(y) => Ok(RuntimeValue::UInt(x.pow(y as u32))),
                RuntimeValue::UInt(y) => Ok(RuntimeValue::UInt(x.pow(y as u32))),
                RuntimeValue::Long(y) => Ok(RuntimeValue::UInt(x.pow(y as u32))),
                RuntimeValue::ULong(y) => Ok(RuntimeValue::UInt(x.pow(y as u32))),
                RuntimeValue::Float(y) => Ok(RuntimeValue::Float((x as f32).powf(y))),
                RuntimeValue::Double(y) => Ok(RuntimeValue::Double((x as f64).powf(y))),
                _ => self.panic_operator(&rhs, &BinaryOperator::Pow),
            },
            RuntimeValue::ULong(x) => match rhs {
                RuntimeValue::Int(y) => Ok(RuntimeValue::ULong(x.pow(y as u32))),
                RuntimeValue::UInt(y) => Ok(RuntimeValue::ULong(x.pow(y as u32))),
                RuntimeValue::Long(y) => Ok(RuntimeValue::ULong(x.pow(y as u32))),
                RuntimeValue::ULong(y) => Ok(RuntimeValue::ULong(x.pow(y as u32))),
                RuntimeValue::Float(y) => Ok(RuntimeValue::Double((x as f64).powf(y as f64))),
                RuntimeValue::Double(y) => Ok(RuntimeValue::Double((x as f64).powf(y))),
                _ => self.panic_operator(&rhs, &BinaryOperator::Pow),
            },
            _ => self.panic_operator(&rhs, &BinaryOperator::Pow),
        }
    }
}
