use crate::runtime::{interpreter::InterpreterErr, scope::{CheckerEnvironment}, values::RuntimeType};
use calibre_parser::ast::comparison::Comparison;

pub fn handle(
    op: &Comparison,
    env: &CheckerEnvironment,
    scope: &u64,
    left: RuntimeType,
    right: RuntimeType,
) -> Result<RuntimeType, InterpreterErr> {
    if left.is_type(&right) {
        Ok(RuntimeType::Bool)
    }else{
        panic!()
    }
}
