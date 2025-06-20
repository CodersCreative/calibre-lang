use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{LoopType, NodeType, RefMutability},
    runtime::{
        interpreter::{InterpreterErr, evaluate, expressions::get_new_scope},
        scope::{Scope, variables::get_var},
        values::{RuntimeType, RuntimeValue, helper::Block},
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

pub fn evaluate_loop_declaration(
    declaration: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::LoopDeclaration { loop_type, body } = declaration {
        let handle_body = |new_scope: Rc<RefCell<Scope>>| {
            let mut result = RuntimeValue::Null;
            for statement in body.iter() {
                if let NodeType::IfStatement { .. } = statement {
                    return evaluate_if_statement(statement.clone(), new_scope.clone());
                } else if let NodeType::Return { value } = statement {
                    return Ok((evaluate(*value.clone(), new_scope)?, true));
                } else {
                    result = evaluate(statement.clone(), new_scope.clone())?;
                }
            }

            Ok((result, false))
        };

        let mut result = RuntimeValue::Null;

        if let LoopType::For(identifier, range) = *loop_type {
            if let RuntimeValue::Range(from, to) =
                evaluate(range, scope.clone())?.into_type(scope.clone(), RuntimeType::Range)?
            {
                for i in from..to {
                    let new_scope = get_new_scope(
                        scope.clone(),
                        vec![(
                            identifier.clone(),
                            RuntimeType::Integer,
                            RefMutability::Value,
                        )],
                        vec![NodeType::IntegerLiteral(i as i64)],
                    )?;
                    let value = handle_body(new_scope)?;
                    result = value.0;
                    if value.1 {
                        return Ok(result);
                    }
                }
            }
        } else if let LoopType::ForEach(identifier, (loop_name, mutability)) = *loop_type {
            if let RuntimeValue::List { data, data_type } = get_var(scope.clone(), &loop_name)? {
                for d in data {
                    let new_scope = get_new_scope(scope.clone(), Vec::new(), Vec::new())?;
                    let _ = new_scope.borrow_mut().push_var(
                        identifier.clone(),
                        &d,
                        match mutability {
                            RefMutability::MutRef | RefMutability::MutValue => true,
                            _ => false,
                        },
                    );

                    let value = handle_body(new_scope)?;
                    result = value.0;
                    if value.1 {
                        return Ok(result);
                    }
                }
            }
        } else if let LoopType::While(condition) = *loop_type {
            while let RuntimeValue::Bool(x) = evaluate(condition.clone(), scope.clone())? {
                if !x {
                    break;
                }
                let new_scope = get_new_scope(scope.clone(), Vec::new(), Vec::new())?;
                let value = handle_body(new_scope)?;
                result = value.0;
                if value.1 {
                    return Ok(result);
                }
            }
        }

        Ok(result)
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
