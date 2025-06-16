use std::collections::HashMap;

use crate::{
    ast::{BinaryOperator, NodeType},
    runtime::{
        interpreter::evaluate,
        scope::Scope,
        values::{self, RuntimeValue},
    },
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

        if left.is_number() && right.is_number() {
            if let Some(value) = evaluate_numeric_binary_expression(&left, &right, operator) {
                return value;
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

pub fn evaluate_assignment_expression(node: NodeType, scope: &mut Scope) -> RuntimeValue {
    if let NodeType::AssignmentExpression { identifier, value } = node {
        if let NodeType::Identifier(identifier) = *identifier {
            let value = evaluate(*value, scope);
            scope.assign_var(identifier, &value);
            value
        } else {
            panic!(
                "Tried to evaluate non-assignment-expression node using evaluate_assignment_expression."
            )
        }
    } else {
        panic!(
            "Tried to evaluate non-assignment-expression node using evaluate_assignment_expression."
        )
    }
}

pub fn evaluate_object_expression(obj: NodeType, scope: &mut Scope) -> RuntimeValue {
    let mut properties = HashMap::new();

    if let NodeType::MapLiteral(props) = obj {
        for (k, v) in props {
            let value = if let Some(value) = v {
                evaluate(value, scope)
            } else {
                scope.get_var(&k).clone()
            };

            properties.insert(k, value);
        }
    }

    RuntimeValue::Map(properties)
}

pub fn evaluate_numeric_binary_expression(
    left: &RuntimeValue,
    right: &RuntimeValue,
    operator: BinaryOperator,
) -> Option<RuntimeValue> {
    match left {
        RuntimeValue::Float(x) => match right {
            RuntimeValue::Float(y) => Some(RuntimeValue::Float(match operator {
                BinaryOperator::Add => x + y,
                BinaryOperator::Subtract => x - y,
                BinaryOperator::Divide => x / y,
                BinaryOperator::Multiply => x * y,
                BinaryOperator::Power => x.powf(*y),
                BinaryOperator::Modulus => x % y,
            })),
            RuntimeValue::Integer(y) => Some(RuntimeValue::Float(match operator {
                BinaryOperator::Add => x + *y as f64,
                BinaryOperator::Subtract => x - *y as f64,
                BinaryOperator::Divide => x / *y as f64,
                BinaryOperator::Multiply => x * *y as f64,
                BinaryOperator::Power => x.powf(*y as f64),
                BinaryOperator::Modulus => x % *y as f64,
            })),
            _ => return None,
        },
        RuntimeValue::Integer(x) => match right {
            RuntimeValue::Integer(y) => Some(RuntimeValue::Integer(match operator {
                BinaryOperator::Add => x + y,
                BinaryOperator::Subtract => x - y,
                BinaryOperator::Divide => x / y,
                BinaryOperator::Multiply => x * y,
                BinaryOperator::Power => x.pow(*y as u32),
                BinaryOperator::Modulus => x % y,
            })),
            RuntimeValue::Float(y) => Some(RuntimeValue::Float(match operator {
                BinaryOperator::Add => *x as f64 + y,
                BinaryOperator::Subtract => *x as f64 - y,
                BinaryOperator::Divide => *x as f64 / y,
                BinaryOperator::Multiply => *x as f64 * y,
                BinaryOperator::Power => (*x as f64).powf(*y),
                BinaryOperator::Modulus => *x as f64 % y,
            })),
            _ => return None,
        },
        _ => return None,
    }
}
