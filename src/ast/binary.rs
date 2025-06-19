use std::ops::{Add, Div, Mul, Sub};

use thiserror::Error;

use crate::runtime::values::RuntimeValue;

#[derive(Error, Debug, Clone)]
pub enum ASTError {
    #[error("Cannot {2:?} values of {0:?}, {1:?}.")]
    BinaryOperator(RuntimeValue, RuntimeValue, BinaryOperator),
    #[error("Cannot perform a boolean operation to values of {0:?}, {1:?}.")]
    BooleanOperator(RuntimeValue, RuntimeValue),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinaryOperator {
    Subtract,
    Add,
    Multiply,
    Divide,
    Power,
    Modulus,
}

impl BinaryOperator {
    pub fn from_symbol(symbol: char) -> Option<Self> {
        match symbol {
            '+' => Some(Self::Add),
            '-' => Some(Self::Subtract),
            '/' => Some(Self::Divide),
            '*' => Some(Self::Multiply),
            '^' => Some(Self::Power),
            '%' => Some(Self::Modulus),
            _ => None,
        }
    }

    pub fn to_symbol(&self) -> char {
        match self {
            Self::Add => '+',
            Self::Subtract => '-',
            Self::Divide => '/',
            Self::Multiply => '*',
            Self::Power => '^',
            Self::Modulus => '%',
        }
    }

    pub fn handle(
        &self,
        left: RuntimeValue,
        right: RuntimeValue,
    ) -> Result<RuntimeValue, ASTError> {
        match self {
            Self::Add => left + right,
            Self::Subtract => left - right,
            Self::Multiply => left * right,
            Self::Divide => left / right,
            Self::Power => left.pow(right),
            Self::Modulus => left.modulus(right),
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

impl Add for RuntimeValue {
    type Output = Result<RuntimeValue, ASTError>;
    fn add(self, rhs: Self) -> Self::Output {
        let add_str = || -> Self::Output {
            let mut x = rhs.to_string();
            x.push_str(&self.to_string());
            Ok(RuntimeValue::Str(x))
        };

        match self {
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
            Self::Integer(x) => match rhs {
                Self::Integer(y) => Ok(RuntimeValue::Integer(x + y)),
                Self::Float(y) => Ok(RuntimeValue::Float(x as f64 + y)),
                Self::Str(_) => add_str(),
                _ => self.panic_operator(&rhs, &BinaryOperator::Add),
            },
            Self::Float(x) => match rhs {
                Self::Integer(y) => Ok(RuntimeValue::Float(x + y as f64)),
                Self::Float(y) => Ok(RuntimeValue::Float(x as f64 + y)),
                Self::Str(_) => add_str(),
                _ => self.panic_operator(&rhs, &BinaryOperator::Add),
            },
            Self::List {
                mut data,
                data_type,
            } => {
                data.push(rhs);
                Ok(Self::List { data, data_type })
            }
            _ => match rhs {
                Self::Str(_) => add_str(),
                _ => self.panic_operator(&rhs, &BinaryOperator::Add),
            },
        }
    }
}
impl Sub for RuntimeValue {
    type Output = Result<RuntimeValue, ASTError>;
    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            Self::Integer(x) => match rhs {
                Self::Integer(y) => Ok(RuntimeValue::Integer(x - y)),
                Self::Float(y) => Ok(RuntimeValue::Float(x as f64 - y)),
                _ => self.panic_operator(&rhs, &BinaryOperator::Subtract),
            },
            Self::Float(x) => match rhs {
                Self::Integer(y) => Ok(RuntimeValue::Float(x + y as f64)),
                Self::Float(y) => Ok(RuntimeValue::Float(x as f64 + y)),
                _ => self.panic_operator(&rhs, &BinaryOperator::Subtract),
            },
            _ => self.panic_operator(&rhs, &BinaryOperator::Subtract),
        }
    }
}
impl Mul for RuntimeValue {
    type Output = Result<RuntimeValue, ASTError>;
    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            Self::Integer(x) => match rhs {
                Self::Integer(y) => Ok(RuntimeValue::Integer(x * y)),
                Self::Float(y) => Ok(RuntimeValue::Float(x as f64 * y)),
                _ => self.panic_operator(&rhs, &BinaryOperator::Multiply),
            },
            Self::Float(x) => match rhs {
                Self::Integer(y) => Ok(RuntimeValue::Float(x * y as f64)),
                Self::Float(y) => Ok(RuntimeValue::Float(x as f64 * y)),
                _ => self.panic_operator(&rhs, &BinaryOperator::Multiply),
            },
            _ => self.panic_operator(&rhs, &BinaryOperator::Multiply),
        }
    }
}
impl Div for RuntimeValue {
    type Output = Result<RuntimeValue, ASTError>;
    fn div(self, rhs: Self) -> Self::Output {
        match self {
            Self::Integer(x) => match rhs {
                Self::Integer(y) => Ok(RuntimeValue::Integer(x / y)),
                Self::Float(y) => Ok(RuntimeValue::Float(x as f64 / y)),
                _ => self.panic_operator(&rhs, &BinaryOperator::Divide),
            },
            Self::Float(x) => match rhs {
                Self::Integer(y) => Ok(RuntimeValue::Float(x / y as f64)),
                Self::Float(y) => Ok(RuntimeValue::Float(x as f64 / y)),
                _ => self.panic_operator(&rhs, &BinaryOperator::Divide),
            },
            _ => self.panic_operator(&rhs, &BinaryOperator::Divide),
        }
    }
}
impl RuntimeValue {
    fn modulus(self, rhs: Self) -> Result<RuntimeValue, ASTError> {
        match self {
            Self::Integer(x) => match rhs {
                Self::Integer(y) => Ok(RuntimeValue::Integer(x % y)),
                Self::Float(y) => Ok(RuntimeValue::Float(x as f64 % y)),
                _ => self.panic_operator(&rhs, &BinaryOperator::Modulus),
            },
            Self::Float(x) => match rhs {
                Self::Integer(y) => Ok(RuntimeValue::Float(x % y as f64)),
                Self::Float(y) => Ok(RuntimeValue::Float(x as f64 % y)),
                _ => self.panic_operator(&rhs, &BinaryOperator::Modulus),
            },
            _ => self.panic_operator(&rhs, &BinaryOperator::Modulus),
        }
    }

    fn pow(self, rhs: Self) -> Result<RuntimeValue, ASTError> {
        match self {
            Self::Integer(x) => match rhs {
                Self::Integer(y) => Ok(RuntimeValue::Integer(x.pow(y as u32))),
                Self::Float(y) => Ok(RuntimeValue::Float((x as f64).powf(y))),
                _ => self.panic_operator(&rhs, &BinaryOperator::Power),
            },
            Self::Float(x) => match rhs {
                Self::Integer(y) => Ok(RuntimeValue::Float(x + y as f64)),
                Self::Float(y) => Ok(RuntimeValue::Float(x as f64 + y)),
                _ => self.panic_operator(&rhs, &BinaryOperator::Power),
            },
            _ => self.panic_operator(&rhs, &BinaryOperator::Power),
        }
    }
}
