use core::panic;
use std::{collections::HashMap, mem::discriminant, thread::scope};

use crate::{
    ast::{binary::BinaryOperator, NodeType},
    runtime::{
        interpreter::{evaluate, statements},
        scope::Scope,
        values::{self, helper::Map, NativeFunctions, RuntimeValue},
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

        operator.handle(left, right)
    } else {
        panic!("Tried to evaluate non-binary-expression node using evaluate_binary_expression.")
    }
}

pub fn evaluate_comparison_expression(exp: NodeType, scope: &mut Scope) -> RuntimeValue {
    if let NodeType::ComparisonExpression {
        left,
        right,
        operator,
    } = exp
    {
        let left = evaluate(*left, scope);
        let right = evaluate(*right, scope);

        operator.handle(left, right)
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

    RuntimeValue::Map(Map(properties))
}

pub fn evaluate_list_expression(obj: NodeType, scope: &mut Scope) -> RuntimeValue {
    let mut values = Vec::new();

    if let NodeType::ListLiteral(vals) = obj {
        values = vals.iter().map(|v| evaluate(v.clone(), scope)).collect();
    }

    let t = if values.len() > 0 {
        let t = discriminant(&values[0]);
        let filtered: Vec<&RuntimeValue> =
            values.iter().filter(|x| discriminant(*x) == t).collect();
        if values.len() == filtered.len() {
            Some(Box::new(values[0].clone().into()))
        } else {
            None
        }
    } else {
        None
    };

    RuntimeValue::List {
        data: values,
        data_type: t,
    }
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
                    for statement in &body.0 {
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
