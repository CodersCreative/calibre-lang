use std::{cell::RefCell, rc::Rc, str::FromStr};

use crate::{
    ast::NodeType,
    runtime::{
        interpreter::evaluate,
        scope::Scope,
        values::{RuntimeType, RuntimeValue, helper::Block},
    },
};

pub fn evaluate_program(exp: NodeType, scope: Rc<RefCell<Scope>>) -> RuntimeValue {
    let mut last = RuntimeValue::Null;

    if let NodeType::Program(body) = exp {
        for statement in body.into_iter() {
            last = evaluate(statement, scope.clone());
        }
    } else {
        panic!("Tried to evaluate non-program node using evaluate_program.")
    }

    last
}
pub fn evaluate_struct_declaration(
    declaration: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> RuntimeValue {
    if let NodeType::StructDeclaration {
        identifier,
        properties,
    } = declaration
    {
        scope.borrow_mut().push_struct(identifier, &properties);
        RuntimeValue::Null
    } else {
        panic!("Tried to evaluate non-declaration node using evaluate_variable_declaration.")
    }
}

pub fn evaluate_function_declaration(
    declaration: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> RuntimeValue {
    if let NodeType::FunctionDeclaration {
        identifier,
        parameters,
        body,
        return_type,
        is_async,
    } = declaration
    {
        // scope.push_struct(identifier, &properties);
        let value = RuntimeValue::Function {
            identifier: identifier.clone(),
            parameters,
            body: Block(*body),
            return_type,
            is_async,
        };

        scope.borrow_mut().push_var(identifier, &value, false);
        value
    } else {
        panic!("Tried to evaluate non-declaration node using evaluate_variable_declaration.")
    }
}

pub fn evaluate_if_statement(declaration: NodeType, scope: Rc<RefCell<Scope>>) -> RuntimeValue {
    if let NodeType::IfStatement {
        comparisons,
        bodies,
    } = declaration
    {
        for (i, comparison) in comparisons.iter().enumerate() {
            if let RuntimeValue::Bool(x) = evaluate(comparison.clone(), scope.clone()) {
                if x {
                    let mut result: RuntimeValue = RuntimeValue::Null;
                    for statement in bodies[i].iter() {
                        result = evaluate(statement.clone(), scope.clone());
                    }

                    return result;
                }
            } else {
                panic!("Expected a boolean operation");
            }
        }

        if comparisons.len() < bodies.len() {
            if let Some(last) = bodies.last() {
                let mut result: RuntimeValue = RuntimeValue::Null;
                for statement in last.iter() {
                    result = evaluate(statement.clone(), scope.clone());
                }
                return result;
            }
        }

        RuntimeValue::Null
    } else {
        panic!("Tried to evaluate non-declaration node using evaluate_variable_declaration.")
    }
}

pub fn evaluate_variable_declaration(
    declaration: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> RuntimeValue {
    if let NodeType::VariableDeclaration {
        is_mutable,
        identifier,
        value,
        data_type,
    } = declaration
    {
        let mut value = match value {
            Some(x) => evaluate(*x, scope.clone()),
            None => RuntimeValue::Null,
        };

        if let Some(t) = data_type {
            value = value.into_type(scope.clone(), t);
        }

        scope.borrow_mut().push_var(identifier, &value, is_mutable);

        value
    } else {
        panic!("Tried to evaluate non-declaration node using evaluate_variable_declaration.")
    }
}
