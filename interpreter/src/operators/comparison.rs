use core::panic;

use crate::{
    operators::binary::ASTError,
    runtime::{interpreter::InterpreterErr, scope::Environment, values::RuntimeValue},
};
use calibre_parser::ast::comparison::Comparison;

impl Environment {
    pub fn is_equal(&self, scope: &u64, value: &RuntimeValue, other: &RuntimeValue) -> bool {
        if let Ok(RuntimeValue::Bool(x)) = handle(
            &Comparison::Equal,
            self,
            scope,
            value.clone(),
            other.clone(),
        ) {
            x
        } else {
            false
        }
    }
}

fn value_handle<T: PartialEq + PartialOrd>(op: &Comparison, left: T, right: T) -> bool {
    match op {
        Comparison::NotEqual => left != right,
        Comparison::Equal => left == right,
        Comparison::Lesser => left < right,
        Comparison::LesserEqual => left <= right,
        Comparison::Greater => left > right,
        Comparison::GreaterEqual => left >= right,
    }
}

pub fn handle(
    op: &Comparison,
    env: &Environment,
    scope: &u64,
    left: RuntimeValue,
    right: RuntimeValue,
) -> Result<RuntimeValue, InterpreterErr> {
    let left = left.unwrap_val(env, scope)?;
    let right = right.unwrap_val(env, scope)?;

    let (left, right) = if left == RuntimeValue::Null || right == RuntimeValue::Null {
        (left, right)
    } else {
        left.clone().make_similar(env, scope, right)?
    };

    Ok(RuntimeValue::Bool(match left {
        RuntimeValue::Int(x) => match right {
            RuntimeValue::Int(y) => value_handle(op, x, y),
            _ => panic!(),
        },
        RuntimeValue::UInt(x) => match right {
            RuntimeValue::UInt(y) => value_handle(op, x, y),
            _ => panic!(),
        },
        RuntimeValue::Long(x) => match right {
            RuntimeValue::Long(y) => value_handle(op, x, y),
            _ => panic!(),
        },
        RuntimeValue::ULong(x) => match right {
            RuntimeValue::ULong(y) => value_handle(op, x, y),
            _ => panic!(),
        },
        RuntimeValue::Float(x) => match right {
            RuntimeValue::Float(y) => value_handle(op, x, y),
            _ => panic!(),
        },
        RuntimeValue::Double(x) => match right {
            RuntimeValue::Double(y) => value_handle(op, x, y),
            _ => panic!(),
        },
        _ => value_handle(op, left, right),
    }))
}
