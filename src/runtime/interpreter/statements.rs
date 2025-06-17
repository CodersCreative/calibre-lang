use std::str::FromStr;

use crate::{
    ast::NodeType,
    runtime::{
        interpreter::evaluate,
        scope::Scope,
        values::{RuntimeType, RuntimeValue},
    },
};

pub fn evaluate_program(exp: NodeType, scope: &mut Scope) -> RuntimeValue {
    let mut last = RuntimeValue::Null;

    if let NodeType::Program(body) = exp {
        for statement in body.into_iter() {
            last = evaluate(statement, scope);
        }
    } else {
        panic!("Tried to evaluate non-program node using evaluate_program.")
    }

    last
}
pub fn evaluate_struct_declaration(declaration: NodeType, scope: &mut Scope) -> RuntimeValue {
    if let NodeType::StructDeclaration {
        identifier,
        properties,
    } = declaration
    {
        scope.push_struct(identifier, &properties);
        RuntimeValue::Null
    } else {
        panic!("Tried to evaluate non-declaration node using evaluate_variable_declaration.")
    }
}

pub fn evaluate_function_declaration(declaration: NodeType, scope: &mut Scope) -> RuntimeValue {
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
            body: *body,
            return_type,
            is_async,
        };

        scope.push_var(identifier, &value, false);
        value
    } else {
        panic!("Tried to evaluate non-declaration node using evaluate_variable_declaration.")
    }
}

pub fn evaluate_variable_declaration(declaration: NodeType, scope: &mut Scope) -> RuntimeValue {
    if let NodeType::VariableDeclaration {
        is_mutable,
        identifier,
        value,
        data_type,
    } = declaration
    {
        let mut value = match value {
            Some(x) => evaluate(*x, scope),
            None => RuntimeValue::Null,
        };

        if let Some(t) = data_type {
            value = value.into_type(scope, RuntimeType::from_str(&t).unwrap());
        }

        scope.push_var(identifier, &value, is_mutable);

        value
    } else {
        panic!("Tried to evaluate non-declaration node using evaluate_variable_declaration.")
    }
}
