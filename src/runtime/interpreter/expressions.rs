use crate::{
    ast::{BinaryOperator, NodeType},
    runtime::{interpreter::evaluate, scope::Scope, values::RuntimeValue},
};

pub fn evaluate_identifier(identifier: &str, scope: &mut Scope) -> RuntimeValue {
    scope.get_var(identifier).clone()
}

pub fn evaluate_binary_expression(exp: NodeType, scope: &mut Scope) -> RuntimeValue {
    if let NodeType::BinaryExpression {
        left,
        right,
        operator,
    } = exp
    {
        let left = evaluate(*left, scope);
        let right = evaluate(*right, scope);

        if let RuntimeValue::Number(x) = left {
            if let RuntimeValue::Number(y) = right {
                return RuntimeValue::Number(evaluate_numeric_binary_expression(x, y, operator));
            }
        }

        panic!(
            "Cannot perform binary expression on two values of types : {:?} and {:?}",
            left, right
        );
    } else {
        panic!("Tried to evaluate non-binary-expression node using evaluate_binary_expression.")
    }
}

pub fn evaluate_numeric_binary_expression(left: f64, right: f64, operator: BinaryOperator) -> f64 {
    match operator {
        BinaryOperator::Add => left + right,
        BinaryOperator::Subtract => left - right,
        BinaryOperator::Divide => left / right,
        BinaryOperator::Multiply => left * right,
        BinaryOperator::Power => left.powf(right),
        BinaryOperator::Modulus => left % right,
    }
}
