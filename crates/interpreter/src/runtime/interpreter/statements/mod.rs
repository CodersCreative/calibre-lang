use calibre_common::environment::Variable;
use calibre_parser::ast::{Node, NodeType, VarType};

use crate::runtime::{
    interpreter::InterpreterErr,
    scope::InterpreterEnvironment,
    values::{
        FunctionType, RuntimeType, RuntimeValue,
        helper::{Block, MatchBlock},
    },
};

pub mod comparisons;
pub mod import;
pub mod loops;
pub mod matching;
pub mod structs;

impl InterpreterEnvironment {
    pub fn evaluate_match_declaration(
        &mut self,
        scope: &u64,
        declaration: Node,
    ) -> Result<RuntimeValue, InterpreterErr> {
        let NodeType::MatchDeclaration {
            parameters,
            body,
            return_type,
            is_async,
        } = declaration.node_type
        else {
            unreachable!()
        };

        let mut params = Vec::new();

        let default = if let Some(node) = parameters.2 {
            Some(self.evaluate(scope, *node)?)
        } else {
            None
        };

        params.push((parameters.0, RuntimeType::from(parameters.1), default));

        Ok(RuntimeValue::Function {
            parameters: params,
            body: FunctionType::Match(MatchBlock(body)),
            return_type: match return_type {
                Some(x) => Some(RuntimeType::from(x)),
                None => None,
            },
            is_async,
        })
    }

    pub fn evaluate_function_declaration(
        &mut self,
        scope: &u64,
        declaration: Node,
    ) -> Result<RuntimeValue, InterpreterErr> {
        let NodeType::FunctionDeclaration {
            parameters,
            body,
            return_type,
            is_async,
        } = declaration.node_type
        else {
            unreachable!()
        };
        let mut params = Vec::new();

        for p in parameters.into_iter() {
            let default = if let Some(node) = p.2 {
                Some(self.evaluate(scope, node)?)
            } else {
                None
            };

            params.push((p.0, RuntimeType::from(p.1), default));
        }

        Ok(RuntimeValue::Function {
            parameters: params,
            body: FunctionType::Regular(Block(body)),
            return_type: match return_type {
                Some(x) => Some(RuntimeType::from(x)),
                None => None,
            },
            is_async,
        })
    }

    pub fn evaluate_variable_declaration(
        &mut self,
        scope: &u64,
        var_type: VarType,
        identifier: String,
        value: RuntimeValue,
        data_type: Option<RuntimeType>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let Some(t) = data_type {
            if !value.is_type(self, scope, &t) {
                return Err(InterpreterErr::ExpectedType(value.clone(), t));
            }
        }

        Ok(self.push_var(
            scope,
            identifier,
            Variable {
                value,
                var_type,
                location: self.current_location.clone(),
            },
        )?)
    }
}

#[cfg(test)]
mod tests {
    use calibre_parser::ast::VarType;

    use super::*;
    use crate::runtime::values::RuntimeValue;
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
            .evaluate(&scope, node)
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
        let result = env.evaluate(&scope, node).unwrap();

        match result {
            RuntimeValue::Function { .. } => {}
            _ => panic!("Expected RuntimeValue::Function"),
        }
    }
}
