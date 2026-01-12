use calibre_common::{environment::Variable, errors::RuntimeErr};
use calibre_mir_ty::{MiddleNode, MiddleNodeType};
use calibre_parser::ast::VarType;
use rand::random_range;

use crate::runtime::{
    interpreter::InterpreterErr,
    scope::InterpreterEnvironment,
    values::{RuntimeType, RuntimeValue, helper::Block},
};

impl InterpreterEnvironment {
    pub fn evaluate_function(
        &mut self,
        scope: &u64,
        func: RuntimeValue,
        mut arguments: Vec<(MiddleNode, Option<MiddleNode>)>,
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
                    if let RuntimeValue::Aggregate(None, x) = x {
                        arguments =
                            x.0.into_iter()
                                .map(|x| {
                                    let counter = random_range(0..1000000000);

                                    let _ = self
                                        .force_var(
                                            &scope,
                                            format!("$-{}", counter),
                                            Variable {
                                                value: x.1,
                                                var_type: VarType::Mutable,
                                            },
                                        )
                                        .unwrap();

                                    (
                                        MiddleNode::new_from_type(MiddleNodeType::Identifier(
                                            format!("$-{}", counter).into(),
                                        )),
                                        None,
                                    )
                                })
                                .collect();
                    }
                }
            };

            if parameters.len() > arguments.len() {
                let params: Vec<(String, RuntimeType, Option<RuntimeValue>)> = parameters
                    .iter()
                    .enumerate()
                    .filter(|(i, x)| {
                        for arg in arguments.iter() {
                            if let (MiddleNodeType::Identifier(y), Some(_)) =
                                (&arg.0.node_type, &arg.1)
                            {
                                if x.0 == y.to_string() {
                                    return false;
                                }
                            }
                        }

                        i > &(arguments.len() - 1)
                    })
                    .map(|x| x.1.clone())
                    .collect();

                if params.iter().filter(|x| x.2.is_none()).count() > 0 {
                    let arguments: Vec<(MiddleNode, Option<MiddleNode>)> = [
                        arguments,
                        params
                            .iter()
                            .map(|x| {
                                (
                                    MiddleNode::new_from_type(MiddleNodeType::Identifier(
                                        x.0.clone().into(),
                                    )),
                                    None,
                                )
                            })
                            .collect(),
                    ]
                    .concat();

                    let counter = random_range(0..100000000);

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

                    let body = Block(Box::new(MiddleNode::new_from_type(
                        MiddleNodeType::CallExpression(
                            Box::new(MiddleNode::new_from_type(MiddleNodeType::Identifier(
                                format!("$-{}", counter).into(),
                            ))),
                            arguments,
                        ),
                    )));

                    return Ok(RuntimeValue::Function {
                        parameters: params,
                        body,
                        return_type: return_type.clone(),
                        is_async: *is_async,
                    });
                }
            }

            let new_scope = self.get_new_scope(scope, parameters.to_vec(), arguments)?;
            let result = self.evaluate(&new_scope, *body.0.clone())?;
            self.stop = None;

            self.remove_scope(&new_scope);

            Ok(result)
        } else {
            unreachable!()
        }
    }

    pub fn evaluate_call_expression(
        &mut self,
        scope: &u64,
        caller: MiddleNode,
        arguments: Vec<(MiddleNode, Option<MiddleNode>)>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        let func = self.evaluate(scope, caller.clone())?;

        let func = match func {
            RuntimeValue::Ref(x, _) => self.get_var(&x)?.value.clone(),
            x => x,
        };
        match func {
            RuntimeValue::Function { .. } => {
                return self.evaluate_function(scope, func, arguments);
            }
            RuntimeValue::NativeFunction(_) => {
                let mut evaluated_arguments = Vec::new();

                for arg in arguments.iter() {
                    evaluated_arguments.push(if let Some(d) = &arg.1 {
                        if let MiddleNodeType::Identifier(name) = &arg.0.node_type {
                            (
                                RuntimeValue::Str(name.to_string()),
                                Some(self.evaluate(scope, d.clone())?),
                            )
                        } else {
                            return Err(RuntimeErr::InvalidDefaultFuncArg);
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

        Err(RuntimeErr::CantCallNonFunc(func))
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
