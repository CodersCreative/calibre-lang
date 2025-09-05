use calibre_parser::ast::comparison::BooleanOperation;

use crate::{operators::binary::ASTError, runtime::values::RuntimeValue};

pub fn handle(
    op: &BooleanOperation,
    left: &RuntimeValue,
    right: &RuntimeValue,
) -> Result<RuntimeValue, ASTError> {
    if let RuntimeValue::Bool(x) = left {
        if *x && op == &BooleanOperation::Or {
            return Ok(RuntimeValue::Bool(true));
        } else if !x && op == &BooleanOperation::And {
            return Ok(RuntimeValue::Bool(false));
        }
        if let RuntimeValue::Bool(y) = right {
            return Ok(RuntimeValue::Bool(match op {
                BooleanOperation::And => *x && *y,
                BooleanOperation::Or => *x || *y,
            }));
        }
    }

    Err(ASTError::BooleanOperator(left.clone(), right.clone()))
}
