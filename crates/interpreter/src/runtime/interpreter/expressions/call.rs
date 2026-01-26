use calibre_common::{environment::InterpreterFrom, environment::Variable, errors::RuntimeErr};
use calibre_mir_ty::{MiddleNode, MiddleNodeType, renaming::AlphaRenameState};
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
        mut arguments: Vec<MiddleNode>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        let func = MiddleNode::interpreter_from(self, scope, func)?;
        let func = func.rename(&mut AlphaRenameState::default());
        let func = self.evaluate(scope, func)?;
        if let RuntimeValue::Function {
            parameters,
            body,
            return_type,
            is_async,
        } = &func
        {
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
        arguments: Vec<MiddleNode>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        let func = match self.evaluate(scope, caller.clone()) {
            Ok(x) => x,
            Err(e) => {
                let (object, func) = match caller.node_type.clone() {
                    MiddleNodeType::MemberExpression { path } if path.len() == 2 => (
                        path[0].0.node_type.to_string(),
                        path[1].0.node_type.to_string(),
                    ),
                    _ => panic!("{:?}", caller),
                    _ => return Err(e),
                };

                let func =
                    self.evaluate(scope, self.get_function(&object, &func).unwrap().0.clone())?;
                return self.evaluate_function(scope, func, arguments);
            }
        };

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
                    evaluated_arguments.push(self.evaluate(scope, arg.clone())?);
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
