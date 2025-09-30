use crate::runtime::{interpreter::InterpreterErr, scope::CheckerEnvironment, values::RuntimeType};
use calibre_common::errors::ASTError;
use calibre_parser::ast::binary::BinaryOperator;


pub fn handle(
    op: &BinaryOperator,
    env: &CheckerEnvironment,
    scope: &u64,
    left: RuntimeType,
    right: RuntimeType,
) -> Result<RuntimeType, InterpreterErr> {
    // match op {
    //     BinaryOperator::Add => left + right,
    //     BinaryOperator::Sub => left - right,
    //     BinaryOperator::Mul => left * right,
    //     BinaryOperator::Div => left / right,
    //     BinaryOperator::Pow => left.pow(right),
    //     BinaryOperator::Mod => left.modulus(right),
    //     BinaryOperator::BitXor => left ^ right,
    //     BinaryOperator::BitOr => left | right,
    //     BinaryOperator::BitAnd => Ok(match left.clone() & right.clone() {
    //         Ok(x) => x,
    //         _ => left.special_and(right, env, scope)?,
    //     }),
    //     BinaryOperator::Shl => Ok(match left.clone() << right.clone() {
    //         Ok(x) => x,
    //         _ => left.special_shl(right, env, scope)?,
    //     }),
    //     BinaryOperator::Shr => Ok(match left.clone() >> right.clone() {
    //         Ok(x) => x,
    //         _ => left.special_shr(right, env, scope)?,
    //     }),
    // }?;

    Ok(left)
}

impl RuntimeType {
    pub fn panic_operator(
        &self,
        rhs: &Self,
        operator: &BinaryOperator,
    ) -> Result<RuntimeType, ASTError<RuntimeType>> {
        Err(ASTError::BinaryOperator(
            self.clone(),
            rhs.clone(),
            operator.clone(),
        ))
    }
}
