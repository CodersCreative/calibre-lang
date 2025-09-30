use crate::runtime::{
    interpreter::InterpreterErr,
    scope::{Environment, Variable},
    values::{RuntimeType},
};
use calibre_parser::ast::{LoopType, NodeType, RefMutability, VarType};
use std::mem::discriminant;

impl Environment {
    pub fn evaluate_tuple_expression(
        &mut self,
        scope: &u64,
        vals : Vec<NodeType>
    ) -> Result<RuntimeType, InterpreterErr> {
        let mut values = Vec::new();

        for val in vals.iter() {
            values.push(self.evaluate(scope, val.clone())?);
        }

        Ok(RuntimeType::Tuple(values))
    }

    pub fn evaluate_list_expression(
        &mut self,
        scope: &u64,
        vals : Vec<NodeType>
    ) -> Result<RuntimeType, InterpreterErr> {
        let mut values = Vec::new();

            for val in vals.iter() {
                values.push(self.evaluate(scope, val.clone())?);
            }

        let t = if values.len() > 0 {
            let t = discriminant(&values[0]);
            let filtered: Vec<&RuntimeType> =
                values.iter().filter(|x| discriminant(*x) == t).collect();
            if values.len() == filtered.len() {
                Some(values[0].clone())
            } else {
                None
            }
        } else {
            None
        };

        Ok(RuntimeType::List(Box::new(t)))
    }

    pub fn evaluate_iter_expression(
        &mut self,
        scope: &u64,
        map : NodeType,
        loop_type : LoopType,
        conditionals : Vec<NodeType>
    ) -> Result<RuntimeType, InterpreterErr> {
            let mut result = None;

            if let LoopType::For(identifier, range) = loop_type {
                let range = self.evaluate(scope, range)?;
                if let RuntimeType::List(d) = range {
                    let new_scope = self.get_new_scope(scope, Vec::new(), Vec::new())?;
                    let _ = self.push_var(
                        &new_scope,
                        identifier.clone(),
                        Variable {
                            value: d.unwrap_or(RuntimeType::Dynamic),
                            var_type: VarType::Immutable,
                        },
                    );

                    if self.handle_conditionals(&new_scope, conditionals.clone())? {
                        result = Some(
                            self.evaluate(&new_scope, map.clone())?);
                    }

                    self.remove_scope(&new_scope);
                } else if let RuntimeType::Range =
                    range.into_type(self, scope, &RuntimeType::Range)?
                {
                    let new_scope = self.get_new_scope(
                        scope,
                        vec![(
                            identifier.clone(),
                            RuntimeType::Int,
                            RefMutability::Value,
                            None,
                        )],
                        vec![(NodeType::IntLiteral(1 as i128), None)],
                    )?;

                    if self.handle_conditionals(&new_scope, conditionals.clone())? {
                        result = Some(self.evaluate(&new_scope, map.clone())?);
                    }
                    self.remove_scope(&new_scope);
                }
            } else if let LoopType::ForEach(identifier, (loop_name, mutability)) = loop_type {
                let var = self.get_var(scope, &loop_name)?.clone();
                if let RuntimeType::List(d) = var.value.clone() {
                    let new_scope = self.get_new_scope(scope, Vec::new(), Vec::new())?;
                    let _ = self.push_var(
                        &new_scope,
                        identifier.clone(),
                        Variable {
                            value: d.unwrap_or(RuntimeType::Dynamic),
                            var_type: match mutability {
                                RefMutability::MutRef | RefMutability::MutValue => {
                                    VarType::Mutable
                                }
                                _ => VarType::Immutable,
                            },
                        },
                    );

                    if self.handle_conditionals(&new_scope, conditionals.clone())? {
                        result = Some(
                            self.evaluate(&new_scope, map.clone())?
                        );
                    }
                    self.remove_scope(&new_scope);
                }
            } else if let LoopType::While(_) = loop_type {
                panic!()
            }

            Ok(RuntimeType::List(Box::new(result)))
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
