use calibre_parser::ast::comparison::BooleanOperation;

use crate::{operators::binary::ASTError, runtime::values::{RuntimeType}};

pub fn handle(
    op: &BooleanOperation,
    left: &RuntimeType,
    right: &RuntimeType,
) -> Result<RuntimeType, ASTError> {
    if left != right || left != &RuntimeType::Int || left != &RuntimeType::Bool {
        Err(ASTError::BooleanOperator(left.clone(), right.clone()))
    }else{
        Ok(RuntimeType::Bool)
    }
}
