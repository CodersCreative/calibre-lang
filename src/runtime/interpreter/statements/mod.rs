use crate::{
    ast::NodeType,
    runtime::{
        interpreter::InterpreterErr,
        scope::{Environment, Variable},
        values::{
            FunctionType, RuntimeValue,
            helper::{Block, MatchBlock},
        },
    },
};

pub mod comparisons;
pub mod import;
pub mod loops;
pub mod matching;
pub mod structs;

impl Environment {
    pub fn evaluate_program(
        &mut self,
        scope: &u64,
        exp: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        let mut last = RuntimeValue::Null;

        if let NodeType::Program(body) = exp {
            for statement in body.into_iter() {
                last = self.evaluate(scope, statement)?;
            }
        } else {
            return Err(InterpreterErr::NotImplemented(exp));
        }

        Ok(last)
    }

    pub fn evaluate_match_declaration(
        &mut self,
        scope: &u64,
        declaration: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let NodeType::MatchDeclaration {
            parameters,
            body,
            return_type,
            is_async,
        } = declaration
        {
            let mut params = Vec::new();

            let default = if let Some(node) = parameters.3 {
                Some(self.evaluate(scope, *node)?)
            } else {
                None
            };

            params.push((parameters.0, parameters.1, parameters.2, default));

            Ok(RuntimeValue::Function {
                parameters: params,
                body: FunctionType::Match(MatchBlock(body)),
                return_type,
                is_async,
            })
        } else {
            Err(InterpreterErr::NotImplemented(declaration))
        }
    }

    pub fn evaluate_function_declaration(
        &mut self,
        scope: &u64,
        declaration: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let NodeType::FunctionDeclaration {
            parameters,
            body,
            return_type,
            is_async,
        } = declaration
        {
            let mut params = Vec::new();

            for p in parameters.into_iter() {
                let default = if let Some(node) = p.3 {
                    Some(self.evaluate(scope, node)?)
                } else {
                    None
                };

                params.push((p.0, p.1, p.2, default));
            }

            Ok(RuntimeValue::Function {
                parameters: params,
                body: FunctionType::Regular(Block(body)),
                return_type,
                is_async,
            })
        } else {
            Err(InterpreterErr::NotImplemented(declaration))
        }
    }

    pub fn evaluate_variable_declaration(
        &mut self,
        scope: &u64,
        declaration: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let NodeType::VariableDeclaration {
            var_type,
            identifier,
            value,
            data_type,
        } = declaration
        {
            let mut value = self.evaluate(scope, *value)?;

            if let Some(t) = data_type {
                value = value.unwrap(self, scope)?.into_type(self, scope, &t)?;
            }

            Ok(self.push_var(scope, identifier, Variable { value, var_type })?)
        } else {
            Err(InterpreterErr::NotImplemented(declaration))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::NodeType;
    use crate::runtime::values::{RuntimeValue, helper::VarType};
    use std::path::PathBuf;
    use std::str::FromStr;

    fn get_new_env() -> (Environment, u64) {
        let mut env = Environment::new();
        let scope = env.new_scope_with_stdlib(None, PathBuf::from_str("./main.cl").unwrap(), None);
        (env, scope)
    }

    #[test]
    fn test_evaluate_variable_declaration() {
        let (mut env, scope) = get_new_env();
        let node = NodeType::VariableDeclaration {
            var_type: VarType::Mutable,
            identifier: "x".to_string(),
            value: Box::new(NodeType::IntLiteral(42)),
            data_type: None,
        };

        let result = env
            .evaluate_variable_declaration(&scope, node)
            .unwrap()
            .unwrap_val(&env, &scope)
            .unwrap();

        assert!(env.is_equal(&scope, &result, &RuntimeValue::Int(42)));
        assert!(env.is_equal(
            &scope,
            &env.get_var(&scope, "x").unwrap().value,
            &RuntimeValue::Int(42)
        ));
    }

    #[test]
    fn test_evaluate_function_declaration() {
        let (mut env, scope) = get_new_env();
        let node = NodeType::FunctionDeclaration {
            parameters: vec![],
            body: Box::new(NodeType::Return {
                value: Box::new(NodeType::IntLiteral(1)),
            }),
            return_type: None,
            is_async: false,
        };
        let result = env.evaluate_function_declaration(&scope, node).unwrap();

        match result {
            RuntimeValue::Function { .. } => {}
            _ => panic!("Expected RuntimeValue::Function"),
        }
    }
}
