use calibre_common::environment::{InterpreterFrom, Variable};
use calibre_mir_ty::{MiddleNode, MiddleNodeType};
use calibre_parser::ast::VarType;

use crate::runtime::{
    interpreter::InterpreterErr,
    scope::InterpreterEnvironment,
    values::{
        RuntimeType, RuntimeValue,
        helper::{Block, StopValue},
    },
};

pub mod comparisons;

impl InterpreterEnvironment {
    pub fn evaluate_loop_declaration(
        &mut self,
        scope: &u64,
        state: Option<MiddleNode>,
        body: MiddleNode,
    ) -> Result<RuntimeValue, InterpreterErr> {
        let mut result = RuntimeValue::Null;
        let new_scope = self.get_new_scope(scope, Vec::new(), Vec::new())?;

        if let Some(state) = state {
            let _ = self.evaluate(scope, state)?;
        }

        loop {
            result = self.evaluate(&new_scope, body.clone())?;
            self.remove_scope(&new_scope);

            match self.stop {
                Some(StopValue::Break) => break,
                Some(StopValue::Return) => {
                    self.stop = Some(StopValue::Return);
                    break;
                }
                _ => {}
            };
        }

        Ok(result)
    }

    pub fn evaluate_function_declaration(
        &mut self,
        scope: &u64,
        declaration: MiddleNode,
    ) -> Result<RuntimeValue, InterpreterErr> {
        let MiddleNodeType::FunctionDeclaration {
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

            params.push((
                p.0.to_string(),
                RuntimeType::interpreter_from(self, scope, p.1)?,
                default,
            ));
        }

        Ok(RuntimeValue::Function {
            parameters: params,
            body: Block(body),
            return_type: RuntimeType::interpreter_from(self, scope, return_type)?,
            is_async,
        })
    }

    pub fn evaluate_variable_declaration(
        &mut self,
        scope: &u64,
        var_type: VarType,
        identifier: String,
        mut value: RuntimeValue,
        data_type: Option<RuntimeType>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let Some(t) = data_type {
            value = match value {
                RuntimeValue::List { data, data_type } if data.len() <= 0 => {
                    RuntimeValue::List { data, data_type }.into_type(self, &t)?
                }
                x => x,
            };
            if !value.is_type(self, &t) {
                return Err(InterpreterErr::ExpectedType(value.clone(), t));
            }
        }

        Ok(self.push_var(scope, identifier, Variable { value, var_type })?)
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
