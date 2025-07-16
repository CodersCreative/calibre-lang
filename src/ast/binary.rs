use std::{
    cell::RefCell,
    ops::{Add, Div, Mul, Sub},
    rc::Rc,
};

use thiserror::Error;

use crate::runtime::{
    interpreter::InterpreterErr,
    scope::{Scope, ScopeErr},
    values::{RuntimeType, RuntimeValue},
};

#[derive(Error, Debug, Clone)]
pub enum ASTError {
    #[error("Cannot {2:?} values of {0:?}, {1:?}.")]
    BinaryOperator(RuntimeValue, RuntimeValue, BinaryOperator),
    #[error("Cannot perform a boolean operation to values of {0:?}, {1:?}.")]
    BooleanOperator(RuntimeValue, RuntimeValue),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinaryOperator {
    Sub,
    Add,
    Mul,
    Div,
    Pow,
    Mod,
}

impl BinaryOperator {
    pub fn from_symbol(symbol: char) -> Option<Self> {
        match symbol {
            '+' => Some(Self::Add),
            '-' => Some(Self::Sub),
            '/' => Some(Self::Div),
            '*' => Some(Self::Mul),
            '^' => Some(Self::Pow),
            '%' => Some(Self::Mod),
            _ => None,
        }
    }

    pub fn to_symbol(&self) -> char {
        match self {
            Self::Add => '+',
            Self::Sub => '-',
            Self::Div => '/',
            Self::Mul => '*',
            Self::Pow => '^',
            Self::Mod => '%',
        }
    }

    pub fn handle(
        &self,
        mut left: RuntimeValue,
        mut right: RuntimeValue,
        scope: Rc<RefCell<Scope>>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        let mut changed = true;

        match left {
            RuntimeValue::Option(Some(x), _) => left = *x,
            RuntimeValue::Result(Ok(x), _) => left = *x,
            RuntimeValue::Result(Err(e), _) => left = *e,
            _ => changed = false,
        }

        match right {
            RuntimeValue::Option(Some(x), _) => right = *x,
            RuntimeValue::Result(Ok(x), _) => right = *x,
            RuntimeValue::Result(Err(e), _) => right = *e,
            _ => changed = false,
        }

        if changed {
            self.handle(left, right, scope)
        } else {
            Ok(match self {
                Self::Add => match left.clone() + right.clone() {
                    Ok(x) => Ok(x),
                    _ => left.special_add(right),
                },
                Self::Sub => left - right,
                Self::Mul => left * right,
                Self::Div => left / right,
                Self::Pow => left.pow(right),
                Self::Mod => left.modulus(right),
            }?)
        }
    }
}

impl RuntimeValue {
    fn panic_operator(
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

    fn special_add(self, rhs: Self) -> Result<RuntimeValue, ASTError> {
        let add_str = || -> Result<RuntimeValue, ASTError> {
            let mut x = rhs.to_string();
            x.push_str(&self.to_string());
            Ok(RuntimeValue::Str(x))
        };

        match self {
            Self::Char(x) => {
                if let RuntimeValue::List { .. } = rhs {
                    Err(ASTError::BinaryOperator(
                        Self::Char(x),
                        rhs.clone(),
                        BinaryOperator::Add,
                    ))
                } else {
                    let mut x = x.to_string();
                    x.push_str(&rhs.to_string());
                    Ok(Self::Str(x))
                }
            }
            Self::Str(mut x) => {
                if let RuntimeValue::List { .. } = rhs {
                    Err(ASTError::BinaryOperator(
                        Self::Str(x),
                        rhs.clone(),
                        BinaryOperator::Add,
                    ))
                } else {
                    x.push_str(&rhs.to_string());
                    Ok(Self::Str(x))
                }
            }
            Self::List {
                mut data,
                data_type,
            } => {
                data.push(rhs);
                Ok(Self::List { data, data_type })
            }
            _ => match rhs {
                Self::Str(_) => add_str(),
                Self::Char(_) => add_str(),
                _ => self.panic_operator(&rhs, &BinaryOperator::Add),
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
