use calibre_common::errors::ASTError;
use calibre_parser::ast::comparison::BooleanOperation;

use crate::{runtime::values::{RuntimeType}};

pub fn handle(
    op: &BooleanOperation,
    left: &RuntimeType,
    right: &RuntimeType,
) -> Result<RuntimeType, ASTError<RuntimeType>> {
    if left == &RuntimeType::Dynamic || right == &RuntimeType::Dynamic {
        Ok(RuntimeType::Bool)
    } else if !left.is_type(&right) || (left != &RuntimeType::Int && left != &RuntimeType::Bool) {
        Err(ASTError::BooleanOperator(left.clone(), right.clone()))
    }else{
        Ok(RuntimeType::Bool)
    }
}
