use core::panic;
use std::{collections::HashMap, thread::scope};

use crate::{
    ast::{BinaryOperator, NodeType},
    runtime::{
        interpreter::{evaluate, statements},
        scope::Scope,
        values::{self, NativeFunctions, RuntimeValue},
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

pub fn evaluate_call_expression(exp: NodeType, scope: &mut Scope) -> RuntimeValue {
    println!("{:?}", exp);
    if let NodeType::CallExpression(caller, arguments) = exp {
        let arguments: Vec<RuntimeValue> = arguments
            .iter()
            .map(|x| evaluate(x.clone(), scope))
            .collect();

        if let NodeType::Identifier(caller) = *caller.clone() {
            if let Some(scope) = scope.safe_resolve_var_mut(&caller) {
                if let RuntimeValue::Function {
                    identifier,
                    parameters,
                    body,
                    return_type,
                    is_async,
                } = scope.get_var(&caller)
                {
                    let mut scope = Scope::new(Some(Box::new(scope.clone())));

                    for (i, (k, v)) in parameters.iter().enumerate() {
                        let arg = arguments[i].into_type(&mut scope, v.clone());
                        scope.push_var(k.to_string(), &arg, true);
                    }

                    let mut result: RuntimeValue = RuntimeValue::Null;
                    for statement in body {
                        result = evaluate(statement.clone(), &mut scope);
                    }

                    if let Some(t) = return_type {
                        return result.into_type(&mut scope, t.clone());
                    } else {
                        return RuntimeValue::Null;
                    }
                }

                if scope.variables.contains_key(&caller) {
                    if arguments.len() <= 0 {
                        return scope.get_var(&caller).clone();
                    } else if arguments.len() == 1 {
                        scope.assign_var(caller, &arguments[0]);
                        return RuntimeValue::Null;
                    } else {
                        panic!(
                            "Setters cant have more than one value or function has same identifier as variable."
                        );
                    }
                } else if let Some(var) = scope.constants.get(&caller) {
                    match var {
                        NativeFunctions => {}
                        _ => {
                            if arguments.len() <= 0 {
                                return scope.get_var(&caller).clone();
                            } else {
                                panic!("Cannot set constant");
                            }
                        }
                    }
                }
            }
        }

        let func = evaluate(*caller, scope);
        match func {
            RuntimeValue::NativeFunction(_) => func.call_native(arguments, scope),
            _ => panic!("Cannot call non-variable or function value"),
        }
    } else {
        panic!(
            "Tried to evaluate non-assignment-expression node using evaluate_assignment_expression."
        )
    }
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
