use crate::runtime::{
    interpreter::InterpreterErr,
    scope::InterpreterEnvironment,
    values::{RuntimeType, RuntimeValue},
};
use calibre_common::environment::InterpreterFrom;
use calibre_mir::ast::MiddleNode;
use calibre_parser::ast::ParserDataType;
use std::mem::discriminant;

impl InterpreterEnvironment {
    pub fn evaluate_list_expression(
        &mut self,
        scope: &u64,
        data_type: ParserDataType,
        vals: Vec<MiddleNode>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        let mut values = Vec::new();

        for val in vals.iter() {
            values.push(self.evaluate(scope, val.clone())?);
        }

        Ok(RuntimeValue::List {
            data: values,
            data_type: Box::new(RuntimeType::interpreter_from(self, scope, data_type)?),
        })
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::values::RuntimeValue;
    use calibre_parser::ast::NodeType;
    use std::{path::PathBuf, str::FromStr};

    fn get_new_env() -> (Environment, u64) {
        let mut env = Environment::new();
        let scope = env.new_scope_with_stdlib(None, PathBuf::from_str("./main.cl").unwrap(), None);
        (env, scope)
    }

    #[test]
    fn test_evaluate_tuple_expression_basic() {
        let (mut env, scope) = get_new_env();
        let node =
            NodeType::TupleLiteral(vec![NodeType::IntLiteral(1), NodeType::FloatLiteral(2.0)]);
        let result = env.evaluate(&scope, node).unwrap();
        assert_eq!(
            result,
            RuntimeValue::Tuple(vec![RuntimeValue::UInt(1), RuntimeValue::Float(2.0)])
        );
    }

    #[test]
    fn test_evaluate_list_expression_homogeneous() {
        let (mut env, scope) = get_new_env();
        let node = NodeType::ListLiteral(vec![
            NodeType::IntLiteral(1),
            NodeType::IntLiteral(2),
            NodeType::IntLiteral(3),
        ]);
        let result = env.evaluate(&scope, node).unwrap();
        match result {
            RuntimeValue::List { data, data_type } => {
                assert_eq!(
                    data,
                    vec![
                        RuntimeValue::UInt(1),
                        RuntimeValue::UInt(2),
                        RuntimeValue::UInt(3)
                    ]
                );
                assert!(data_type.is_some());
            }
            _ => panic!("Expected RuntimeValue::List"),
        }
    }

    #[test]
    fn test_evaluate_list_expression_heterogeneous() {
        let (mut env, scope) = get_new_env();
        let node =
            NodeType::ListLiteral(vec![NodeType::IntLiteral(1), NodeType::FloatLiteral(2.0)]);
        let result = env.evaluate(&scope, node).unwrap();
        match result {
            RuntimeValue::List { data, data_type } => {
                assert_eq!(data, vec![RuntimeValue::UInt(1), RuntimeValue::Float(2.0)]);
                assert!(data_type.is_none());
            }
            _ => panic!("Expected RuntimeValue::List"),
        }
    }

    #[test]
    fn test_evaluate_list_expression_empty() {
        let (mut env, scope) = get_new_env();
        let node = NodeType::ListLiteral(Vec::new());
        let result = env.evaluate(&scope, node).unwrap();
        match result {
            RuntimeValue::List { data, data_type } => {
                assert!(data.is_empty());
                assert!(data_type.is_none());
            }
            _ => panic!("Expected RuntimeValue::List"),
        }
    }
}
