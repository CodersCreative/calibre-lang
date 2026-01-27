use calibre_common::errors::ASTError;
use calibre_parser::ast::comparison::BooleanOperator;

use crate::runtime::values::RuntimeValue;

pub fn handle(
    op: &BooleanOperator,
    left: &RuntimeValue,
    right: &RuntimeValue,
) -> Result<RuntimeValue, ASTError<RuntimeValue>> {
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

    Err(ASTError::BooleanOperator(left.clone(), right.clone()))
}
