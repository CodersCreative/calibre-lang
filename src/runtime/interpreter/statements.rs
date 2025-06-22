use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{LoopType, NodeType, RefMutability},
    runtime::{
        interpreter::{InterpreterErr, evaluate, expressions::get_new_scope},
        scope::{Object, Scope, variables::get_var},
        values::{
            RuntimeType, RuntimeValue,
            helper::{Block, StopValue, VarType},
        },
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
pub fn evaluate_enum_declaration(
    declaration: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::EnumDeclaration {
        identifier,
        options,
    } = declaration
    {
        let _ = scope
            .borrow_mut()
            .push_object(identifier, Object::Enum(options))?;
        Ok(RuntimeValue::Null)
    } else {
        Err(InterpreterErr::NotImplemented(declaration))
    }
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
        let _ = scope
            .borrow_mut()
            .push_object(identifier, Object::Struct(properties))?;
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
        let handle_body = |new_scope: Rc<RefCell<Scope>>| -> Result<RuntimeValue, InterpreterErr> {
            let mut result = RuntimeValue::Null;
            for statement in body.iter() {
                match new_scope.borrow().stop {
                    Some(StopValue::Return) => break,
                    Some(StopValue::Break) => break,
                    Some(StopValue::Continue) => {
                        new_scope.borrow_mut().stop = None;
                        break;
                    }
                    _ => {}
                };
                if let NodeType::Return { value } = statement {
                    new_scope.borrow_mut().stop = Some(StopValue::Return);
                    result = evaluate(*value.clone(), new_scope.clone())?;
                    break;
                } else if let NodeType::Break = statement {
                    if scope.borrow_mut().stop != Some(StopValue::Return) {
                        new_scope.borrow_mut().stop = Some(StopValue::Break);
                    }
                    break;
                } else if let NodeType::Continue = statement {
                    break;
                } else {
                    result = evaluate(statement.clone(), new_scope.clone())?;
                }
            }

            Ok(result)
        };

        let mut result = RuntimeValue::Null;

        if let LoopType::For(identifier, range) = *loop_type {
            let range = evaluate(range, scope.clone())?;
            if let RuntimeValue::List { data, data_type: _ } = range {
                for d in data.into_iter() {
                    let new_scope = get_new_scope(scope.clone(), Vec::new(), Vec::new())?;
                    let _ = new_scope.borrow_mut().push_var(
                        identifier.clone(),
                        d.clone(),
                        VarType::Immutable(None),
                    );
                    result = handle_body(new_scope.clone())?;

                    match new_scope.borrow().stop {
                        Some(StopValue::Break) => break,
                        Some(StopValue::Return) => {
                            scope.borrow_mut().stop = Some(StopValue::Return);
                            break;
                        }
                        _ => {}
                    };
                }
            } else if let RuntimeValue::Range(from, to) =
                range.into_type(scope.clone(), RuntimeType::Range)?
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
                    result = handle_body(new_scope.clone())?;

                    match new_scope.borrow().stop {
                        Some(StopValue::Break) => break,
                        Some(StopValue::Return) => {
                            scope.borrow_mut().stop = Some(StopValue::Return);
                            break;
                        }
                        _ => {}
                    };
                }
            }
        } else if let LoopType::ForEach(identifier, (loop_name, mutability)) = *loop_type {
            let (var, _) = get_var(scope.clone(), &loop_name)?;
            if let RuntimeValue::List {
                mut data,
                data_type,
            } = var
            {
                for d in data.iter_mut() {
                    let new_scope = get_new_scope(scope.clone(), Vec::new(), Vec::new())?;
                    let _ = new_scope.borrow_mut().push_var(
                        identifier.clone(),
                        d.clone(),
                        match mutability {
                            RefMutability::MutRef | RefMutability::MutValue => {
                                VarType::Mutable(None)
                            }
                            _ => VarType::Immutable(None),
                        },
                    );

                    result = handle_body(new_scope.clone())?;

                    match new_scope.borrow().stop {
                        Some(StopValue::Break) => break,
                        Some(StopValue::Return) => {
                            scope.borrow_mut().stop = Some(StopValue::Return);
                            break;
                        }
                        _ => {}
                    };

                    *d = get_var(new_scope, &identifier)?.0;
                }

                scope.borrow_mut().assign_var(
                    &loop_name,
                    RuntimeValue::List {
                        data: data.clone(),
                        data_type: data_type.clone(),
                    },
                )?;
            }
        } else if let LoopType::While(condition) = *loop_type {
            while let RuntimeValue::Bool(x) = evaluate(condition.clone(), scope.clone())? {
                if !x {
                    break;
                }
                let new_scope = get_new_scope(scope.clone(), Vec::new(), Vec::new())?;
                result = handle_body(new_scope.clone())?;

                match new_scope.borrow().stop {
                    Some(StopValue::Break) => break,
                    Some(StopValue::Return) => {
                        scope.borrow_mut().stop = Some(StopValue::Return);
                        break;
                    }
                    _ => {}
                };
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
                    .push_function(identifier.clone(), (iden, func, function.1))?;
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

        let _ = scope
            .borrow_mut()
            .push_var(identifier, value.clone(), VarType::Immutable(None))?;
        Ok(value)
    } else {
        Err(InterpreterErr::NotImplemented(declaration))
    }
}

pub fn evaluate_if_statement(
    declaration: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
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
                            scope.borrow_mut().stop = Some(StopValue::Return);
                            return Ok(evaluate(*value.clone(), scope.clone())?);
                        } else if let NodeType::Break = statement {
                            if scope.borrow().stop != Some(StopValue::Return) {
                                scope.borrow_mut().stop = Some(StopValue::Break);
                            }
                            return Ok(result);
                        } else if let NodeType::Continue = statement {
                            if scope.borrow().stop == None {
                                scope.borrow_mut().stop = Some(StopValue::Continue);
                            }
                            return Ok(result);
                        }

                        result = evaluate(statement.clone(), scope.clone())?;
                    }

                    return Ok(result);
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
                        scope.borrow_mut().stop = Some(StopValue::Return);
                        return Ok(evaluate(*value.clone(), scope.clone())?);
                    } else if let NodeType::Break = statement {
                        if scope.borrow().stop != Some(StopValue::Return) {
                            scope.borrow_mut().stop = Some(StopValue::Break);
                        }
                        return Ok(result);
                    } else if let NodeType::Continue = statement {
                        if scope.borrow().stop == None {
                            scope.borrow_mut().stop = Some(StopValue::Continue);
                        }
                        return Ok(result);
                    }
                    result = evaluate(statement.clone(), scope.clone())?;
                }
                return Ok(result);
            }
        }

        Ok(RuntimeValue::Null)
    } else {
        Err(InterpreterErr::NotImplemented(declaration))
    }
}

pub fn evaluate_variable_declaration(
    declaration: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::VariableDeclaration {
        var_type,
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
            .push_var(identifier, value.clone(), var_type)?;

        Ok(value)
    } else {
        Err(InterpreterErr::NotImplemented(declaration))
    }
}
