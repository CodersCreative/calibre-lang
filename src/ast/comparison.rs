use core::panic;
use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::binary::{ASTError, BinaryOperator},
    runtime::{
        interpreter::InterpreterErr,
        scope::{Scope, links::get_link},
        values::RuntimeValue,
    },
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Comparison {
    Greater,
    Lesser,
    LesserEqual,
    GreaterEqual,
    Equal,
    NotEqual,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum BooleanOperation {
    And,
    Or,
}

impl BooleanOperation {
    pub fn from_operator(txt: &str) -> Option<Self> {
        match txt.trim() {
            "&&" => Some(Self::And),
            "||" => Some(Self::Or),
            _ => None,
        }
    }

    pub fn to_operator(&self) -> &str {
        match self {
            Self::And => "&&",
            Self::Or => "||",
        }
    }

    pub fn handle(
        &self,
        left: &RuntimeValue,
        right: &RuntimeValue,
    ) -> Result<RuntimeValue, ASTError> {
        if let RuntimeValue::Bool(x) = left {
            if let RuntimeValue::Bool(y) = right {
                return Ok(RuntimeValue::Bool(match self {
                    Self::And => *x && *y,
                    Self::Or => *x || *y,
                }));
            }
        }

        Err(ASTError::BooleanOperator(left.clone(), right.clone()))
    }
}

pub fn is_equal(value: &RuntimeValue, other: &RuntimeValue, scope: &Rc<RefCell<Scope>>) -> bool {
    if let Ok(RuntimeValue::Bool(x)) = Comparison::Equal.handle(value.clone(), other.clone(), scope)
    {
        x
    } else {
        false
    }
}

impl RuntimeValue {
    fn panic_comparison(
        &self,
        rhs: &Self,
        operator: &Comparison,
    ) -> Result<RuntimeValue, ASTError> {
        Err(ASTError::BinaryOperator(
            self.clone(),
            rhs.clone(),
            BinaryOperator::Add,
        ))
    }
}

impl Comparison {
    pub fn from_operator(txt: &str) -> Option<Self> {
        match txt.trim() {
            ">=" => Some(Self::GreaterEqual),
            ">" => Some(Self::Greater),
            "<=" => Some(Self::LesserEqual),
            "<" => Some(Self::Lesser),
            "==" => Some(Self::Equal),
            "!=" => Some(Self::NotEqual),
            _ => None,
        }
    }

    pub fn to_operator(&self) -> &str {
        match self {
            Self::GreaterEqual => ">=",
            Self::Greater => ">",
            Self::LesserEqual => "<=",
            Self::Lesser => "<",
            Self::Equal => "==",
            Self::NotEqual => "!=",
        }
    }

    fn value_handle<T: PartialEq + PartialOrd>(&self, left: T, right: T) -> bool {
        match self {
            Self::NotEqual => left != right,
            Self::Equal => left == right,
            Self::Lesser => left < right,
            Self::LesserEqual => left <= right,
            Self::Greater => left > right,
            Self::GreaterEqual => left >= right,
        }
    }

    pub fn handle(
        &self,
        mut left: RuntimeValue,
        mut right: RuntimeValue,
        scope: &Rc<RefCell<Scope>>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        let mut changed = true;

        match left {
            RuntimeValue::Option(Some(x), _) => left = *x,
            RuntimeValue::Result(Ok(x), _) => left = *x,
            RuntimeValue::Result(Err(e), _) => left = *e,
            RuntimeValue::Link(_, _) => left = get_link(scope, &left)?,
            _ => changed = false,
        }

        match right {
            RuntimeValue::Option(Some(x), _) => right = *x,
            RuntimeValue::Result(Ok(x), _) => right = *x,
            RuntimeValue::Result(Err(e), _) => right = *e,
            RuntimeValue::Link(_, _) => right = get_link(scope, &right)?,
            _ => changed = false,
        }

        if changed {
            return self.handle(left, right, scope);
        }

        let (left, right) = if left == RuntimeValue::Null || right == RuntimeValue::Null {
            (left, right)
        } else {
            left.clone().make_similar(right, scope)?
        };

        Ok(RuntimeValue::Bool(match left {
            RuntimeValue::Int(x) => match right {
                RuntimeValue::Int(y) => self.value_handle(x, y),
                _ => panic!(),
            },
            RuntimeValue::UInt(x) => match right {
                RuntimeValue::UInt(y) => self.value_handle(x, y),
                _ => panic!(),
            },
            RuntimeValue::Long(x) => match right {
                RuntimeValue::Long(y) => self.value_handle(x, y),
                _ => panic!(),
            },
            RuntimeValue::ULong(x) => match right {
                RuntimeValue::ULong(y) => self.value_handle(x, y),
                _ => panic!(),
            },
            RuntimeValue::Float(x) => match right {
                RuntimeValue::Float(y) => self.value_handle(x, y),
                _ => panic!(),
            },
            RuntimeValue::Double(x) => match right {
                RuntimeValue::Double(y) => self.value_handle(x, y),
                _ => panic!(),
            },
            _ => self.value_handle(left, right),
        }))
    }
}
