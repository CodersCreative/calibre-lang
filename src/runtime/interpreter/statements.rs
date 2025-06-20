use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::NodeType,
    runtime::{
        interpreter::{InterpreterErr, evaluate},
        scope::Scope,
        values::{RuntimeValue, helper::Block},
    },
};

pub fn evaluate_program(
    exp: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    let mut last = RuntimeValue::Null;

    if let NodeType::Program(body) = exp {
        for statement in body.into_iter() {
            last = evaluate(statement, scope.clone())?;
        }
    } else {
        return Err(InterpreterErr::NotImplemented(exp));
    }

    Ok(last)
}
pub fn evaluate_struct_declaration(
    declaration: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::StructDeclaration {
        identifier,
        properties,
    } = declaration
    {
        let _ = scope.borrow_mut().push_struct(identifier, &properties)?;
        Ok(RuntimeValue::Null)
    } else {
        Err(InterpreterErr::NotImplemented(declaration))
    }
}

pub fn evaluate_impl_declaration(
    declaration: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::ImplDeclaration {
        identifier,
        functions,
    } = declaration
    {
        for function in functions {
            let scope_2 = Rc::new(RefCell::new(Scope::new(Some(scope.clone()))));
            let func = evaluate(function.0, scope_2)?;

            if let RuntimeValue::Function {
                identifier: iden, ..
            } = func.clone()
            {
                let _ = scope
                    .borrow_mut()
                    .push_struct_function(identifier.clone(), (iden, func, function.1))?;
            } else {
                return Err(InterpreterErr::ExpectedFunctions);
            }
        }

        Ok(RuntimeValue::Null)
    } else {
        Err(InterpreterErr::NotImplemented(declaration))
    }
}

pub fn evaluate_function_declaration(
    declaration: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::FunctionDeclaration {
        identifier,
        parameters,
        body,
        return_type,
        is_async,
    } = declaration
    {
        let value = RuntimeValue::Function {
            identifier: identifier.clone(),
            parameters,
            body: Block(*body),
            return_type,
            is_async,
        };

        let _ = scope.borrow_mut().push_var(identifier, &value, false)?;
        Ok(value)
    } else {
        Err(InterpreterErr::NotImplemented(declaration))
    }
}

pub fn evaluate_if_statement(
    declaration: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<(RuntimeValue, bool), InterpreterErr> {
    if let NodeType::IfStatement {
        comparisons,
        bodies,
    } = declaration
    {
        for (i, comparison) in comparisons.iter().enumerate() {
            if let RuntimeValue::Bool(x) = evaluate(comparison.clone(), scope.clone())? {
                if x {
                    let mut result: RuntimeValue = RuntimeValue::Null;
                    for statement in bodies[i].iter() {
                        if let NodeType::Return { value } = statement {
                            return Ok((evaluate(*value.clone(), scope.clone())?, true));
                        }

                        result = evaluate(statement.clone(), scope.clone())?;
                    }

                    return Ok((result, false));
                }
            } else {
                return Err(InterpreterErr::ExpectedOperation(String::from("boolean")));
            }
        }

        if comparisons.len() < bodies.len() {
            if let Some(last) = bodies.last() {
                let mut result: RuntimeValue = RuntimeValue::Null;
                for statement in last.iter() {
                    if let NodeType::Return { value } = statement {
                        return Ok((evaluate(*value.clone(), scope.clone())?, true));
                    }
                    result = evaluate(statement.clone(), scope.clone())?;
                }
                return Ok((result, false));
            }
        }

        Ok((RuntimeValue::Null, false))
    } else {
        Err(InterpreterErr::NotImplemented(declaration))
    }
}

pub fn evaluate_variable_declaration(
    declaration: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::VariableDeclaration {
        is_mutable,
        identifier,
        value,
        data_type,
    } = declaration
    {
        let mut value = match value {
            Some(x) => evaluate(*x, scope.clone())?,
            None => RuntimeValue::Null,
        };

        if let Some(t) = data_type {
            value = value.into_type(scope.clone(), t)?;
        }

        let _ = scope
            .borrow_mut()
            .push_var(identifier, &value, is_mutable)?;

        Ok(value)
    } else {
        Err(InterpreterErr::NotImplemented(declaration))
    }
}
