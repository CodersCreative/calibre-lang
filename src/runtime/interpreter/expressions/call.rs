use std::fmt::Arguments;

use rand::seq::IndexedRandom;

use crate::{
    ast::{NodeType, RefMutability},
    runtime::{
        interpreter::InterpreterErr,
        scope::{Environment, ScopeErr, Type},
        values::{
            FunctionType, RuntimeType, RuntimeValue, ValueErr,
            helper::{Block, ObjectType, VarType},
        },
    },
};

impl Environment {
    pub fn evaluate_function(
        &mut self,
        scope: &u64,
        func: RuntimeValue,
        arguments: Vec<(NodeType, Option<NodeType>)>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let RuntimeValue::Function {
            parameters,
            body,
            return_type,
            is_async,
        } = &func
        {
            if parameters.len() > arguments.len() {
                let params: Vec<(String, RuntimeType, RefMutability, Option<RuntimeValue>)> =
                    parameters
                        .iter()
                        .enumerate()
                        .filter(|(i, x)| {
                            for arg in arguments.iter() {
                                if let (NodeType::Identifier(y), Some(z)) = arg {
                                    if &x.0 == y {
                                        return false;
                                    }
                                }
                            }

                            i > &(arguments.len() - 1)
                        })
                        .map(|x| x.1.clone())
                        .collect();

                if params.iter().filter(|x| x.3.is_none()).count() > 0 {
                    let arguments: Vec<(NodeType, Option<NodeType>)> = [
                        arguments,
                        params
                            .iter()
                            .map(|x| (NodeType::Identifier(x.0.clone()), None))
                            .collect(),
                    ]
                    .concat();

                    let body = FunctionType::Regular(Block(Box::new(NodeType::CallExpression(
                        Box::new(NodeType::RuntimeValue(func.clone())),
                        arguments,
                    ))));

                    return Ok(RuntimeValue::Function {
                        parameters: params,
                        body,
                        return_type: return_type.clone(),
                        is_async: *is_async,
                    });
                }
            }

            match body {
                FunctionType::Regular(body) => {
                    let new_scope = self.get_new_scope(scope, parameters.to_vec(), arguments)?;
                    let result = self.evaluate(&new_scope, *body.0.clone())?;
                    self.stop = None;

                    let result = if let Some(t) = return_type {
                        result.into_type(self, &new_scope, &t)?
                    } else {
                        result
                    };

                    let result = result.unwrap_links_val(self, &new_scope, Some(new_scope))?;

                    self.remove_scope(&new_scope);

                    Ok(result)
                }
                FunctionType::Match(body) => {
                    return self.evaluate_match_function(
                        scope,
                        &parameters[0].2,
                        arguments[0].0.clone(),
                        body.0.clone(),
                    );
                }
            }
        } else {
            panic!()
        }
    }

    pub fn evaluate_call_expression(
        &mut self,
        scope: &u64,
        exp: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let NodeType::CallExpression(caller, arguments) = exp {
            if let NodeType::Identifier(object_name) = *caller.clone() {
                if let Ok(Type::Struct(ObjectType::Tuple(params))) =
                    self.get_object_type(scope, &object_name)
                {
                    if arguments.len() == params.len() {
                        let mut args = Vec::new();

                        let params = params.clone();
                        for (i, arg) in arguments.into_iter().enumerate() {
                            args.push(
                                self.evaluate(scope, arg.0)?
                                    .unwrap(self, scope)?
                                    .into_type(self, scope, &params[i])?,
                            );
                        }

                        return Ok(RuntimeValue::Struct(
                            scope.clone(),
                            Some(object_name),
                            ObjectType::Tuple(args),
                        ));
                    }
                }
            }

            let func = self
                .evaluate(scope, *caller.clone())?
                .unwrap(self, scope)?
                .clone();

            match func {
                RuntimeValue::Function { .. } => {
                    return self.evaluate_function(scope, func, arguments);
                }
                RuntimeValue::List { data, data_type } if arguments.len() == 1 => {
                    match self.evaluate(scope, arguments[0].0.clone())? {
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
                                    Some(self.evaluate(scope, d.clone())?),
                                )
                            } else {
                                panic!()
                            }
                        } else {
                            (self.evaluate(scope, arg.0.clone())?, None)
                        });
                    }

                    match func {
                        RuntimeValue::NativeFunction(x) => {
                            return x.run(self, scope, &evaluated_arguments);
                        }
                        _ => {}
                    }
                }
                _ => {}
            }

            if let NodeType::Identifier(caller) = *caller.clone() {
                if let Ok(var) = self.get_var_ref(scope, &caller) {
                    let RuntimeValue::Link(new_scope, _, _) = &var.value else {
                        panic!()
                    };
                    match var.var_type {
                        VarType::Mutable => {
                            if arguments.len() <= 0 {
                                return Ok(var.value);
                            } else if arguments.len() == 1 {
                                let value = self.evaluate(&scope, arguments[0].0.clone())?;
                                let _ = self.assign_var(&new_scope, &caller, value.clone())?;
                                return Ok(value);
                            } else {
                                return Err(InterpreterErr::SetterArgs(arguments));
                            }
                        }
                        _ => match var {
                            NativeFunctions => {}
                            _ => {
                                if arguments.len() <= 0 {
                                    return Ok(var.value.clone());
                                } else {
                                    return Err(InterpreterErr::Value(ValueErr::Scope(
                                        ScopeErr::AssignConstant(caller.clone()),
                                    )));
                                }
                            }
                        },
                    }
                }
            }
            panic!("Cannot call non-variable or function value, {:?}", func);
        } else {
            Err(InterpreterErr::NotImplemented(exp))
        }
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
