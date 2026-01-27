use crate::runtime::{
    interpreter::InterpreterErr, scope::InterpreterEnvironment, values::RuntimeValue,
};
use calibre_common::errors::ASTError;
use calibre_parser::ast::comparison::ComparisonOperator;

impl InterpreterEnvironment {
    pub fn is_equal(&self, scope: &u64, value: &RuntimeValue, other: &RuntimeValue) -> bool {
        if let Ok(RuntimeValue::Bool(x)) = handle(
            &ComparisonOperator::Equal,
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

fn value_handle<T: PartialEq + PartialOrd>(op: &ComparisonOperator, left: T, right: T) -> bool {
    match op {
        ComparisonOperator::NotEqual => left != right,
        ComparisonOperator::Equal => left == right,
        ComparisonOperator::Lesser => left < right,
        ComparisonOperator::LesserEqual => left <= right,
        ComparisonOperator::Greater => left > right,
        ComparisonOperator::GreaterEqual => left >= right,
    }
}

pub fn handle(
    op: &ComparisonOperator,
    _env: &InterpreterEnvironment,
    _scope: &u64,
    left: RuntimeValue,
    right: RuntimeValue,
) -> Result<RuntimeValue, InterpreterErr> {
    match left {
        RuntimeValue::Int(x) => match right {
            RuntimeValue::Int(y) => Ok(RuntimeValue::Bool(value_handle(op, x, y))),
            _ => Err(ASTError::ComparisonOperation(left, right).into()),
        },
        RuntimeValue::Float(x) => match right {
            RuntimeValue::Float(y) => Ok(RuntimeValue::Bool(value_handle(op, x, y))),
            _ => Err(ASTError::ComparisonOperation(left, right).into()),
        },
        _ => Ok(RuntimeValue::Bool(value_handle(op, left, right))),
    }
}
