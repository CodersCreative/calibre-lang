use calibre_common::environment::Variable;
use calibre_mir_ty::{MiddleNode, MiddleNodeType};
use calibre_parser::ast::{RefMutability, VarType};

use crate::runtime::{
    interpreter::InterpreterErr,
    scope::InterpreterEnvironment,
    values::{RuntimeType, RuntimeValue},
};

impl InterpreterEnvironment {
    pub fn get_new_scope_with_values(
        &mut self,
        scope: &u64,
        arguments: Vec<(String, RuntimeValue)>,
    ) -> Result<u64, InterpreterErr> {
        let new_scope = self.new_scope(Some(*scope));
        for (k, v) in arguments {
            let mutability = if let RuntimeValue::Ref(_, _) = v {
                RefMutability::MutRef
            } else {
                RefMutability::MutValue
            };
            let _ = self.push_var(
                &new_scope,
                k.to_string(),
                Variable {
                    value: v,
                    var_type: match mutability {
                        RefMutability::MutRef | RefMutability::MutValue => VarType::Mutable,
                        _ => VarType::Immutable,
                    },
                },
            )?;
        }

        Ok(new_scope)
    }

    pub fn get_new_scope(
        &mut self,
        scope: &u64,
        parameters: Vec<(String, RuntimeType)>,
        arguments: Vec<MiddleNode>,
    ) -> Result<u64, InterpreterErr> {
        let new_scope = self.new_scope(Some(*scope));

        for (i, (k, v)) in parameters.iter().enumerate() {
            let m = match v {
                RuntimeType::Ref(_) => RefMutability::MutRef,
                _ => RefMutability::MutValue,
            };

            if let Some(arg) = arguments.get(i) {
                let arg = self.evaluate(&new_scope, arg.clone())?;
                self.push_var(
                    &new_scope,
                    k.to_string(),
                    Variable {
                        value: arg,
                        var_type: match m {
                            RefMutability::MutRef | RefMutability::MutValue => VarType::Mutable,
                            _ => VarType::Immutable,
                        },
                    },
                )?;
                continue;
            }

            if !arguments.is_empty() {
                return Err(InterpreterErr::RefNonVar(arguments[0].node_type.clone()));
            }
        }

        Ok(new_scope)
    }

    pub fn evaluate_scope(
        &mut self,
        scope: &u64,
        body: Vec<MiddleNode>,
        is_temp: bool,
        create_new_scope: bool,
    ) -> Result<RuntimeValue, InterpreterErr> {
        let new_scope = if create_new_scope {
            self.new_scope(Some(*scope))
        } else {
            scope.clone()
        };

        let mut result: RuntimeValue = RuntimeValue::Null;
        for statement in body.into_iter() {
            result = match self.stop {
                Some(_) if is_temp => return Ok(result),
                _ if !is_temp => self.evaluate_global(&new_scope, statement)?,
                _ => self.evaluate(&new_scope, statement)?,
            }
        }

        if create_new_scope {
            self.remove_scope(&new_scope);
        }

        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::values::{RuntimeType, RuntimeValue};
    use calibre_parser::ast::{NodeType, RefMutability};
    use std::path::PathBuf;
    use std::str::FromStr;

    fn get_new_env() -> (Environment, u64) {
        let mut env = Environment::new();
        let scope = env.new_scope_with_stdlib(None, PathBuf::from_str("./main.cl").unwrap(), None);
        (env, scope)
    }

    #[test]
    fn test_get_new_scope_basic_value() {
        let (mut env, scope) = get_new_env();

        let params = vec![(
            "x".to_string(),
            RuntimeType::Int,
            RefMutability::Value,
            None,
        )];

        let args = vec![(NodeType::IntLiteral(5), None)];
        let scope = env.get_new_scope(&scope, params, args).unwrap();
        let val = env.get_var(&scope, "x").unwrap().value.clone();
        assert_eq!(val, RuntimeValue::Int(5));
    }

    #[test]
    fn test_get_new_scope_default_value() {
        let (mut env, scope) = get_new_env();

        let params = vec![(
            "x".to_string(),
            RuntimeType::Int,
            RefMutability::Value,
            Some(RuntimeValue::Int(10)),
        )];

        let args = vec![];
        let scope = env.get_new_scope(&scope, params, args).unwrap();
        let val = env.get_var(&scope, "x").unwrap().value.clone();
        assert_eq!(val, RuntimeValue::Int(10));
    }

    #[test]
    fn test_get_new_scope_named_argument() {
        let (mut env, scope) = get_new_env();

        let params = vec![(
            "x".to_string(),
            RuntimeType::UInt,
            RefMutability::Value,
            Some(RuntimeValue::UInt(1)),
        )];
        let args = vec![(
            NodeType::Identifier("x".to_string()),
            Some(NodeType::IntLiteral(99)),
        )];

        let scope = env.get_new_scope(&scope, params, args).unwrap();
        let val = env.get_var(&scope, "x").unwrap().value.clone();
        assert_eq!(val, RuntimeValue::UInt(99));
    }

    #[test]
    fn test_get_new_scope_mut_ref_error() {
        let (mut env, scope) = get_new_env();

        let params = vec![(
            "a".to_string(),
            RuntimeType::UInt,
            RefMutability::MutRef,
            None,
        )];

        let args = vec![(NodeType::IntLiteral(5), None)];
        let result = env.get_new_scope(&scope, params, args);
        assert!(result.is_err());
    }

    #[test]
    fn test_evaluate_scope_return() {
        let (mut env, scope) = get_new_env();
        let node = NodeType::ScopeDeclaration {
            body: vec![NodeType::Return {
                value: Box::new(NodeType::IntLiteral(42)),
            }],
            is_temp: true,
        };
        let result = env.evaluate(&scope, node).unwrap();
        assert_eq!(result, RuntimeValue::UInt(42));
    }

    #[test]
    fn test_evaluate_scope_no_return() {
        let (mut env, scope) = get_new_env();
        let node = NodeType::ScopeDeclaration {
            body: vec![NodeType::IntLiteral(7)],
            is_temp: true,
        };
        let result = env.evaluate(&scope, node).unwrap();
        assert_eq!(result, RuntimeValue::UInt(7));
    }
}
