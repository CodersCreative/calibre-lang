use std::{cell::RefCell, mem::discriminant, rc::Rc};

use crate::{
    ast::{LoopType, NodeType, RefMutability},
    runtime::{
        interpreter::InterpreterErr,
        scope::{Environment, Scope, Variable},
        values::{RuntimeType, RuntimeValue, helper::VarType},
    },
};

impl Environment {
    pub fn evaluate_tuple_expression(
        &mut self,
        scope: &u64,
        obj: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        let mut values = Vec::new();

        if let NodeType::TupleLiteral(vals) = obj {
            for val in vals.iter() {
                values.push(self.evaluate(scope, val.clone())?);
            }
        }

        Ok(RuntimeValue::Tuple(values))
    }

    pub fn evaluate_list_expression(
        &mut self,
        scope: &u64,
        obj: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        let mut values = Vec::new();

        if let NodeType::ListLiteral(vals) = obj {
            for val in vals.iter() {
                values.push(self.evaluate(scope, val.clone())?);
            }
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
        declaration: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let NodeType::IterExpression {
            map,
            loop_type,
            conditionals,
        } = declaration
        {
            let mut result = Vec::new();

            if let LoopType::For(identifier, range) = *loop_type {
                let range = self.evaluate(scope, range)?;
                if let RuntimeValue::List { data, data_type: _ } = range {
                    for d in data.into_iter() {
                        let new_scope = self.get_new_scope(scope, Vec::new(), Vec::new())?;
                        let _ = self.push_var(
                            &new_scope,
                            identifier.clone(),
                            Variable {
                                value: d.clone(),
                                var_type: VarType::Immutable,
                            },
                        );

                        if self.handle_conditionals(&new_scope, conditionals.clone())? {
                            result.push(self.evaluate(&new_scope, *map.clone())?);
                        }
                    }
                } else if let RuntimeValue::Range(from, to) =
                    range.into_type(self, scope, &RuntimeType::Range)?
                {
                    for i in from..to {
                        let new_scope = self.get_new_scope(
                            scope,
                            vec![(
                                identifier.clone(),
                                RuntimeType::Int,
                                RefMutability::Value,
                                None,
                            )],
                            vec![(NodeType::IntLiteral(i as i128), None)],
                        )?;
                        if self.handle_conditionals(&new_scope, conditionals.clone())? {
                            result.push(self.evaluate(&new_scope, *map.clone())?);
                        }
                    }
                }
            } else if let LoopType::ForEach(identifier, (loop_name, mutability)) = *loop_type {
                let var = self.get_var(scope, &loop_name)?.clone();
                if let RuntimeValue::List { data, data_type } = &var.value {
                    for d in data.into_iter() {
                        let new_scope = self.get_new_scope(scope, Vec::new(), Vec::new())?;
                        let _ = self.push_var(
                            &new_scope,
                            identifier.clone(),
                            Variable {
                                value: d.clone(),
                                var_type: match mutability {
                                    RefMutability::MutRef | RefMutability::MutValue => {
                                        VarType::Mutable
                                    }
                                    _ => VarType::Immutable,
                                },
                            },
                        );

                        if self.handle_conditionals(&new_scope, conditionals.clone())? {
                            result.push(self.evaluate(&new_scope, *map.clone())?);
                        }
                    }
                }
            } else if let LoopType::While(condition) = *loop_type {
                panic!()
            }

            Ok(RuntimeValue::List {
                data: result,
                data_type: Box::new(None),
            })
        } else {
            Err(InterpreterErr::NotImplemented(declaration))
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::NodeType;
    use crate::runtime::scope::Scope;
    use crate::runtime::values::RuntimeValue;
    use std::cell::RefCell;
    use std::rc::Rc;

    fn new_scope() -> Rc<RefCell<Scope>> {
        Rc::new(RefCell::new(Scope::new(None)))
    }

    #[test]
    fn test_evaluate_tuple_expression_basic() {
        let scope = new_scope();
        let node =
            NodeType::TupleLiteral(vec![NodeType::IntLiteral(1), NodeType::FloatLiteral(2.0)]);
        let result = evaluate_tuple_expression(node, scope).unwrap();
        assert_eq!(
            result,
            RuntimeValue::Tuple(vec![RuntimeValue::Int(1), RuntimeValue::Float(2.0)])
        );
    }

    #[test]
    fn test_evaluate_list_expression_homogeneous() {
        let scope = new_scope();
        let node = NodeType::ListLiteral(vec![
            NodeType::IntLiteral(1),
            NodeType::IntLiteral(2),
            NodeType::IntLiteral(3),
        ]);
        let result = evaluate_list_expression(node, scope).unwrap();
        match result {
            RuntimeValue::List { data, data_type } => {
                assert_eq!(
                    data,
                    vec![
                        RuntimeValue::Int(1),
                        RuntimeValue::Int(2),
                        RuntimeValue::Int(3)
                    ]
                );
                assert!(data_type.is_some());
            }
            _ => panic!("Expected RuntimeValue::List"),
        }
    }

    #[test]
    fn test_evaluate_list_expression_heterogeneous() {
        let scope = new_scope();
        let node =
            NodeType::ListLiteral(vec![NodeType::IntLiteral(1), NodeType::FloatLiteral(2.0)]);
        let result = evaluate_list_expression(node, scope).unwrap();
        match result {
            RuntimeValue::List { data, data_type } => {
                assert_eq!(data, vec![RuntimeValue::Int(1), RuntimeValue::Float(2.0)]);
                assert!(data_type.is_none());
            }
            _ => panic!("Expected RuntimeValue::List"),
        }
    }

    #[test]
    fn test_evaluate_list_expression_empty() {
        let scope = new_scope();
        let node = NodeType::ListLiteral(Vec::new());
        let result = evaluate_list_expression(node, scope).unwrap();
        match result {
            RuntimeValue::List { data, data_type } => {
                assert!(data.is_empty());
                assert!(data_type.is_none());
            }
            _ => panic!("Expected RuntimeValue::List"),
        }
    }
}
