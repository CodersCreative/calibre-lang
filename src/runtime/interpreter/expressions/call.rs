use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::NodeType,
    runtime::{
        interpreter::{InterpreterErr, evaluate, expressions::scope::get_new_scope},
        scope::{Scope, ScopeErr, variables::resolve_var},
        values::{
            RuntimeValue, ValueErr,
            helper::{StopValue, VarType},
        },
    },
};

pub fn evaluate_function(
    scope: Rc<RefCell<Scope>>,
    func: RuntimeValue,
    arguments: Vec<(NodeType, Option<NodeType>)>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let RuntimeValue::Function {
        identifier,
        parameters,
        body,
        return_type,
        is_async,
    } = func
    {
        let new_scope = get_new_scope(scope, parameters, arguments)?;

        let mut result: RuntimeValue = RuntimeValue::Null;
        for statement in &body.0 {
            if let Some(StopValue::Return) = new_scope.borrow().stop {
                break;
            } else if let NodeType::Return { value } = statement {
                result = evaluate(*value.clone(), new_scope.clone())?;
                break;
            } else {
                result = evaluate(statement.clone(), new_scope.clone())?;
            }
        }

        if let Some(t) = return_type {
            return Ok(result.into_type(new_scope, t.clone())?);
        } else {
            return Ok(RuntimeValue::Null);
        }
    } else {
        panic!()
    }
}

pub fn evaluate_call_expression(
    exp: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::CallExpression(caller, arguments) = exp {
        let func = evaluate(*caller.clone(), scope.clone())?;

        match func {
            RuntimeValue::Function { .. } => {
                return evaluate_function(scope, func, *arguments);
            }
            RuntimeValue::List { data, data_type } if arguments.len() == 1 => {
                match evaluate(arguments[0].0.clone(), scope)? {
                    RuntimeValue::Integer(i) if arguments.len() == 1 => {
                        return Ok(data
                            .get(i as usize)
                            .expect("Tried to get index that is larger than list size")
                            .clone());
                    }
                    _ => return Err(InterpreterErr::IndexNonList(arguments[0].0.clone())),
                }
            }
            RuntimeValue::NativeFunction(_) => {
                let mut evaluated_arguments = Vec::new();

                for arg in arguments.iter() {
                    evaluated_arguments.push(if let Some(d) = &arg.1 {
                        if let NodeType::Identifier(name) = &arg.0 {
                            (
                                RuntimeValue::Str(name.clone()),
                                Some(evaluate(d.clone(), scope.clone())?),
                            )
                        } else {
                            panic!()
                        }
                    } else {
                        (evaluate(arg.0.clone(), scope.clone())?, None)
                    });
                }

                return Ok(func.call_native(evaluated_arguments, scope));
            }
            _ => {}
        }

        if let NodeType::Identifier(caller) = *caller.clone() {
            if let Ok(scope_b) = resolve_var(scope, &caller) {
                if let Some((var, var_type)) = scope_b.0.borrow().variables.get(&scope_b.1) {
                    match var_type {
                        VarType::Mutable(_) => {
                            if arguments.len() <= 0 {
                                return Ok(var.clone());
                            } else if arguments.len() == 1 {
                                let _ = scope_b.0.borrow_mut().assign_var(
                                    &caller,
                                    evaluate(arguments[0].0.clone(), scope_b.0.clone())?,
                                )?;
                                return Ok(RuntimeValue::Null);
                            } else {
                                return Err(InterpreterErr::SetterArgs(arguments));
                            }
                        }
                        _ => match var {
                            NativeFunctions => {}
                            _ => {
                                if arguments.len() <= 0 {
                                    return Ok(var.clone());
                                } else {
                                    return Err(InterpreterErr::Value(ValueErr::Scope(
                                        ScopeErr::AssignConstant(scope_b.1),
                                    )));
                                }
                            }
                        },
                    }
                }
            }
        }
        panic!("Cannot call non-variable or function value, {:?}", func);
    } else {
        Err(InterpreterErr::NotImplemented(exp))
    }
}
