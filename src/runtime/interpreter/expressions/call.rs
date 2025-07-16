use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::NodeType,
    runtime::{
        interpreter::{
            InterpreterErr, evaluate,
            expressions::{scope::get_new_scope, structs::evaluate_struct_expression},
        },
        scope::{
            Object, Scope, ScopeErr,
            objects::get_object,
            variables::{assign_var, get_global_scope, resolve_var},
        },
        values::{
            RuntimeValue, ValueErr,
            helper::{ObjectType, StopValue, VarType},
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
        let global = get_global_scope(new_scope.clone());
        for statement in &body.0 {
            if let Some(StopValue::Return) = global.borrow().stop {
                break;
            } else {
                result = evaluate(statement.clone(), new_scope.clone())?;
            }
        }

        global.borrow_mut().stop = None;
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
        if let NodeType::Identifier(object_name) = *caller.clone() {
            if let Ok(Object::Struct(obj)) = get_object(scope.clone(), &object_name) {
                let mut args = Vec::new();
                for arg in arguments {
                    args.push(evaluate(arg.0, scope.clone())?);
                }
                return Ok(RuntimeValue::Struct(
                    ObjectType::Tuple(args),
                    Some(object_name),
                ));
            }
        }
        let func = evaluate(*caller.clone(), scope.clone())?;

        match func {
            RuntimeValue::Function { .. } => {
                return evaluate_function(scope, func, arguments);
            }
            RuntimeValue::List { data, data_type } if arguments.len() == 1 => {
                match evaluate(arguments[0].0.clone(), scope)? {
                    RuntimeValue::Int(i) if arguments.len() == 1 => {
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

                return func.call_native(evaluated_arguments, scope);
            }
            _ => {}
        }

        if let NodeType::Identifier(caller) = *caller.clone() {
            if let Ok((scope, name)) = resolve_var(scope, &caller) {
                if let Some((var, var_type)) = scope.borrow().variables.get(&name).clone() {
                    match var_type {
                        VarType::Mutable(_) => {
                            if arguments.len() <= 0 {
                                return Ok(var.clone());
                            } else if arguments.len() == 1 {
                                let value = evaluate(arguments[0].0.clone(), scope.clone())?;
                                let _ = assign_var(scope.clone(), &caller, value.clone())?;
                                return Ok(value);
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
                                        ScopeErr::AssignConstant(name),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::NodeType;
    use crate::runtime::scope::Scope;
    use crate::runtime::values::RuntimeType;
    use crate::runtime::values::helper::Block;
    use crate::runtime::values::{
        RuntimeValue,
        helper::{StopValue, VarType},
    };
    use std::cell::RefCell;
    use std::rc::Rc;

    fn new_scope() -> Rc<RefCell<Scope>> {
        Rc::new(RefCell::new(Scope::new(None)))
    }

    #[test]
    fn test_evaluate_function_simple_return() {
        let scope = new_scope();
        let func = RuntimeValue::Function {
            identifier: "foo".to_string(),
            parameters: vec![],
            body: Block(vec![NodeType::Return {
                value: Box::new(NodeType::IntLiteral(42)),
            }]),
            return_type: Some(RuntimeType::Int),
            is_async: false,
        };
        let result = evaluate_function(scope, func, Vec::new()).unwrap();
        assert_eq!(result, RuntimeValue::Int(42));
    }

    #[test]
    fn test_evaluate_call_expression_function() {
        let scope = new_scope();
        let func = RuntimeValue::Function {
            identifier: "foo".to_string(),
            parameters: Vec::new(),
            body: Block(vec![NodeType::Return {
                value: Box::new(NodeType::IntLiteral(7)),
            }]),
            return_type: Some(RuntimeType::Int),
            is_async: false,
        };
        scope
            .borrow_mut()
            .push_var("foo".to_string(), func.clone(), VarType::Constant)
            .unwrap();

        let call_node = NodeType::CallExpression(
            Box::new(NodeType::Identifier("foo".to_string())),
            Vec::new(),
        );
        let result = evaluate_call_expression(call_node, scope).unwrap();
        assert_eq!(result, RuntimeValue::Int(7));
    }

    #[test]
    fn test_evaluate_call_expression_variable_get() {
        let scope = new_scope();
        scope
            .borrow_mut()
            .push_var(
                "x".to_string(),
                RuntimeValue::Int(99),
                VarType::Mutable(None),
            )
            .unwrap();

        let call_node =
            NodeType::CallExpression(Box::new(NodeType::Identifier("x".to_string())), Vec::new());
        let result = evaluate_call_expression(call_node, scope).unwrap();
        assert_eq!(result, RuntimeValue::Int(99));
    }

    #[test]
    fn test_evaluate_call_expression_list_index() {
        let scope = new_scope();
        let list = RuntimeValue::List {
            data: vec![RuntimeValue::Int(10), RuntimeValue::Int(20)],
            data_type: Box::new(None),
        };
        scope
            .borrow_mut()
            .push_var("lst".to_string(), list, VarType::Constant)
            .unwrap();

        let call_node = NodeType::CallExpression(
            Box::new(NodeType::Identifier("lst".to_string())),
            vec![(NodeType::IntLiteral(1), None)],
        );
        let result = evaluate_call_expression(call_node, scope).unwrap();
        assert_eq!(result, RuntimeValue::Int(20));
    }
}
