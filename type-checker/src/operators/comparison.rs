use crate::runtime::{interpreter::InterpreterErr, scope::Environment, values::{RuntimeType}};
use calibre_parser::ast::comparison::Comparison;

pub fn handle(
    op: &Comparison,
    env: &Environment,
    scope: &u64,
    left: RuntimeType,
    right: RuntimeType,
) -> Result<RuntimeType, InterpreterErr> {
    // let left = left.unwrap_val(env, scope)?;
    // let right = right.unwrap_val(env, scope)?;
    //
    // let (left, right) = if left == RuntimeValue::Null || right == RuntimeValue::Null {
    //     (left, right)
    // } else {
    //     left.clone().make_similar(env, scope, right)?
    // };
    //
    // Ok(RuntimeValue::Bool(match left {
    //     RuntimeValue::Int(x) => match right {
    //         RuntimeValue::Int(y) => value_handle(op, x, y),
    //         _ => panic!(),
    //     },
    //     RuntimeValue::UInt(x) => match right {
    //         RuntimeValue::UInt(y) => value_handle(op, x, y),
    //         _ => panic!(),
    //     },
    //     RuntimeValue::Long(x) => match right {
    //         RuntimeValue::Long(y) => value_handle(op, x, y),
    //         _ => panic!(),
    //     },
    //     RuntimeValue::ULong(x) => match right {
    //         RuntimeValue::ULong(y) => value_handle(op, x, y),
    //         _ => panic!(),
    //     },
    //     RuntimeValue::Float(x) => match right {
    //         RuntimeValue::Float(y) => value_handle(op, x, y),
    //         _ => panic!(),
    //     },
    //     RuntimeValue::Double(x) => match right {
    //         RuntimeValue::Double(y) => value_handle(op, x, y),
    //         _ => panic!(),
    //     },
    //     _ => value_handle(op, left, right),
    // }))

    Ok(RuntimeType::Bool)
}
