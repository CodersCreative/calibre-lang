use crate::runtime::{
    interpreter::InterpreterErr,
    scope::InterpreterEnvironment,
    values::{RuntimeType, RuntimeValue},
};
use calibre_common::environment::Variable;
use calibre_parser::ast::{LoopType, Node, NodeType, VarType};
use std::mem::discriminant;

impl InterpreterEnvironment {
    pub fn evaluate_tuple_expression(
        &mut self,
        scope: &u64,
        vals: Vec<Node>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        let mut values = Vec::new();

        for val in vals.iter() {
            values.push(self.evaluate(scope, val.clone())?);
        }

        Ok(RuntimeValue::Tuple(values))
    }

    pub fn evaluate_list_expression(
        &mut self,
        scope: &u64,
        vals: Vec<Node>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        let mut values = Vec::new();

        for val in vals.iter() {
            values.push(self.evaluate(scope, val.clone())?);
        }

        let t = if values.len() > 0 {
            let t = discriminant(&values[0]);
            let filtered: Vec<&RuntimeValue> =
                values.iter().filter(|x| discriminant(*x) == t).collect();
            if values.len() == filtered.len() {
                Some((&values[0]).into())
            } else {
                None
            }
        } else {
            None
        };

        Ok(RuntimeValue::List {
            data: values,
            data_type: Box::new(t),
        })
    }

    pub fn evaluate_iter_expression(
        &mut self,
        scope: &u64,
        map: Node,
        loop_type: LoopType,
        conditionals: Vec<Node>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        let mut result = Vec::new();

        if let LoopType::For(identifier, range_node) = loop_type {
            let range = self.evaluate(scope, range_node.clone())?;
            let location = self.get_location(scope, range_node.span);
            if let RuntimeValue::List { data, data_type: _ } = range {
                for d in data.into_iter() {
                    let new_scope = self.get_new_scope(scope, Vec::new(), Vec::new())?;
                    let _ = self.push_var(
                        &new_scope,
                        identifier.clone(),
                        Variable {
                            value: d.clone(),
                            var_type: VarType::Immutable,
                            location: location.clone(),
                        },
                    );

                    if self.handle_conditionals(&new_scope, conditionals.clone())? {
                        result.push(self.evaluate(&new_scope, map.clone())?);
                    }

                    self.remove_scope(&new_scope);
                }
            } else if let RuntimeValue::Range(from, to) = range {
                for i in from..to {
                    let new_scope = self.get_new_scope(
                        scope,
                        vec![(identifier.clone(), RuntimeType::Int, None)],
                        vec![(Node::new(NodeType::IntLiteral(i), range_node.span), None)],
                    )?;
                    if self.handle_conditionals(&new_scope, conditionals.clone())? {
                        result.push(self.evaluate(&new_scope, map.clone())?);
                    }
                    self.remove_scope(&new_scope);
                }
            }
        } else if let LoopType::While(_) = loop_type {
            panic!()
        }

        Ok(RuntimeValue::List {
            data: result,
            data_type: Box::new(None),
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
