use calibre_common::environment::{Type, Variable};
use calibre_parser::ast::{NodeType, ObjectType, RefMutability, VarType};

use crate::runtime::{interpreter::InterpreterErr, scope::InterpreterEnvironment, values::{helper::Block, FunctionType, RuntimeType, RuntimeValue}};


impl InterpreterEnvironment {
    pub fn evaluate_function(
        &mut self,
        scope: &u64,
        func: RuntimeValue,
        mut arguments: Vec<(NodeType, Option<NodeType>)>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let RuntimeValue::Function {
            parameters,
            body,
            return_type,
            is_async,
        } = &func
        {
            if arguments.len() == 1 && parameters.len() > 1 {
                if let Ok(x) = self.evaluate(scope, arguments[0].0.clone()) {
                    if let Ok(RuntimeValue::Tuple(x)) = x.unwrap_val(self, scope) {
                        arguments = x
                            .into_iter()
                            .map(|x| {
                                let counter = self.scopes.get(scope).unwrap().counter;
                                self.scopes.get_mut(scope).unwrap().counter += 1;

                                let _ = self
                                    .force_var(
                                        &scope,
                                        format!("$-{}", counter),
                                        Variable {
                                            value: x,
                                            var_type: VarType::Mutable,
                                        },
                                    )
                                    .unwrap();

                                (NodeType::Identifier(format!("$-{}", counter)), None)
                            })
                            .collect();
                    }
                }
            };

            if parameters.len() > arguments.len() {
                let params: Vec<(String, RuntimeType, RefMutability, Option<RuntimeValue>)> =
                    parameters
                        .iter()
                        .enumerate()
                        .filter(|(i, x)| {
                            for arg in arguments.iter() {
                                if let (NodeType::Identifier(y), Some(_)) = arg {
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

                    let counter = self.scopes.get(scope).unwrap().counter;
                    self.scopes.get_mut(scope).unwrap().counter += 1;

                    let _ = self
                        .force_var(
                            &scope,
                            format!("$-{}", counter),
                            Variable {
                                value: func.clone(),
                                var_type: VarType::Mutable,
                            },
                        )
                        .unwrap();

                    let body = FunctionType::Regular(Block(Box::new(NodeType::CallExpression(
                        Box::new(NodeType::Identifier(format!("$-{}", counter))),
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
        caller : NodeType,
        arguments : Vec<(NodeType, Option<NodeType>)>,
    ) -> Result<RuntimeValue, InterpreterErr> {
            if let NodeType::Identifier(object_name) = caller.clone() {
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
                .evaluate(scope, caller.clone())?
                .unwrap(self, scope)?
                .clone();

            match func {
                RuntimeValue::Function { .. } => {
                    return self.evaluate_function(scope, func, arguments);
                }
                RuntimeValue::List { data, .. } if arguments.len() == 1 => {
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

            panic!("Cannot call non-variable or function value, {:?}", func);
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;
    use std::str::FromStr;

    use calibre_parser::ast::{NodeType, VarType};

    use crate::runtime::scope::{Environment, Variable};
    use crate::runtime::values::RuntimeValue;
    use crate::runtime::values::helper::Block;
    use crate::runtime::values::{FunctionType, RuntimeType};

    fn get_new_env() -> (Environment, u64) {
        let mut env = Environment::new();
        let scope = env.new_scope_with_stdlib(None, PathBuf::from_str("./main.cl").unwrap(), None);
        (env, scope)
    }

    #[test]
    fn test_evaluate_function_simple_return() {
        let (mut env, scope) = get_new_env();
        let func = RuntimeValue::Function {
            parameters: vec![],
            body: FunctionType::Regular(Block(Box::new(NodeType::Return {
                value: Box::new(NodeType::IntLiteral(42)),
            }))),
            return_type: Some(RuntimeType::Int),
            is_async: false,
        };
        let result = env.evaluate_function(&scope, func, Vec::new()).unwrap();
        assert_eq!(result, RuntimeValue::Int(42));
    }

    #[test]
    fn test_evaluate_call_expression_function() {
        let (mut env, scope) = get_new_env();
        let func = RuntimeValue::Function {
            parameters: Vec::new(),
            body: FunctionType::Regular(Block(Box::new(NodeType::Return {
                value: Box::new(NodeType::IntLiteral(7)),
            }))),
            return_type: Some(RuntimeType::Int),
            is_async: false,
        };

        env.push_var(
            &scope,
            "foo".to_string(),
            Variable {
                value: func.clone(),
                var_type: VarType::Constant,
            },
        )
        .unwrap();

        let call_node = NodeType::CallExpression(
            Box::new(NodeType::Identifier("foo".to_string())),
            Vec::new(),
        );
        let result = env.evaluate(&scope, call_node).unwrap();
        assert_eq!(result, RuntimeValue::Int(7));
    }

    #[test]
    fn test_evaluate_call_expression_variable_get() {
        let (mut env, scope) = get_new_env();
        env.push_var(
            &scope,
            "x".to_string(),
            Variable {
                value: RuntimeValue::Int(99),
                var_type: VarType::Mutable,
            },
        )
        .unwrap();

        let call_node =
            NodeType::CallExpression(Box::new(NodeType::Identifier("x".to_string())), Vec::new());
        let result = env
            .evaluate(&scope, call_node)
            .unwrap()
            .unwrap_val(&env, &scope)
            .unwrap();
        assert_eq!(result, RuntimeValue::Int(99));
    }

    #[test]
    fn test_evaluate_call_expression_list_index() {
        let (mut env, scope) = get_new_env();
        let list = RuntimeValue::List {
            data: vec![RuntimeValue::Int(10), RuntimeValue::Int(20)],
            data_type: Box::new(None),
        };
        env.push_var(
            &scope,
            "lst".to_string(),
            Variable {
                value: list,
                var_type: VarType::Constant,
            },
        )
        .unwrap();

        let call_node = NodeType::MemberExpression {
            path: vec![
                (NodeType::Identifier("lst".to_string()), false),
                (NodeType::IntLiteral(1), true),
            ],
        };

        let result = env
            .evaluate(&scope, call_node)
            .unwrap()
            .unwrap_val(&env, &scope)
            .unwrap();
        assert_eq!(result, RuntimeValue::Int(20));
    }
}
