use crate::runtime::{
    interpreter::InterpreterErr, scope::InterpreterEnvironment, values::RuntimeValue,
};
use calibre_common::errors::ASTError;
use calibre_parser::ast::binary::BinaryOperator;
use std::ops::{Add, Div, Mul, Sub};

pub fn handle(
    op: &BinaryOperator,
    env: &InterpreterEnvironment,
    scope: &u64,
    left: RuntimeValue,
    right: RuntimeValue,
) -> Result<RuntimeValue, InterpreterErr> {
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
    ) -> Result<RuntimeValue, ASTError<RuntimeValue>> {
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
            type Output = Result<RuntimeValue, ASTError<RuntimeValue>>;
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
    fn modulus(self, rhs: Self) -> Result<RuntimeValue, ASTError<RuntimeValue>> {
        handle_binop_numeric!(Mul, %, rhs, self)
    }

    fn special_and(
        self,
        rhs: Self,
        _env: &InterpreterEnvironment,
        _scope: &u64,
    ) -> Result<RuntimeValue, ASTError<RuntimeValue>> {
        let add_str = || -> Result<RuntimeValue, ASTError<RuntimeValue>> {
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
        _env: &InterpreterEnvironment,
        _scope: &u64,
    ) -> Result<RuntimeValue, ASTError<RuntimeValue>> {
        match rhs {
            Self::Aggregate(None, mut data) => {
                let key = data.len().to_string();
                data.insert(key, self);
                Ok(Self::Aggregate(None, data))
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
        _env: &InterpreterEnvironment,
        _scope: &u64,
    ) -> Result<RuntimeValue, ASTError<RuntimeValue>> {
        match self {
            Self::Aggregate(None, mut data) => {
                let key = data.len().to_string();
                data.insert(key, rhs);
                Ok(Self::Aggregate(None, data))
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

    fn pow(self, rhs: Self) -> Result<RuntimeValue, ASTError<RuntimeValue>> {
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
