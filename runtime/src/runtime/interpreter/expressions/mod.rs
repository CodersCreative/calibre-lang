use crate::{
    ast::NodeType,
    runtime::{
        interpreter::InterpreterErr,
        scope::{Environment, Variable},
        values::{RuntimeType, RuntimeValue, helper::VarType},
    },
};

pub mod call;
pub mod lists;
pub mod member;
pub mod scope;
pub mod structs;

impl Environment {
    pub fn evaluate_identifier(
        &mut self,
        scope: &u64,
        identifier: &str,
    ) -> Result<RuntimeValue, InterpreterErr> {
        Ok(self.get_var_ref(scope, identifier)?.value.clone())
    }

    pub fn evaluate_not<'a>(
        &mut self,
        scope: &u64,
        exp: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let NodeType::NotExpression { value } = exp {
            let value = self.evaluate(scope, *value)?.unwrap_val(self, scope)?;

            match value {
                RuntimeValue::Bool(x) => Ok(RuntimeValue::Bool(!x)),
                RuntimeValue::Int(x) => Ok(RuntimeValue::Int(-x)),
                RuntimeValue::UInt(x) => Ok(RuntimeValue::Int(-(x as i64))),
                RuntimeValue::Float(x) => Ok(RuntimeValue::Float(-x)),
                RuntimeValue::Double(x) => Ok(RuntimeValue::Double(-x)),
                RuntimeValue::ULong(x) => Ok(RuntimeValue::Long(-(x as i128))),
                RuntimeValue::Long(x) => Ok(RuntimeValue::Long(-x)),
                RuntimeValue::Range(f, t) => Ok(RuntimeValue::Range(t, f)),
                RuntimeValue::List {
                    mut data,
                    data_type,
                } => {
                    data.reverse();
                    Ok(RuntimeValue::List { data, data_type })
                }
                _ => Err(InterpreterErr::UnexpectedType(value)),
            }
        } else {
            Err(InterpreterErr::NotImplemented(exp))
        }
    }

    pub fn evaluate_as_expression(
        &mut self,
        scope: &u64,
        exp: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let NodeType::AsExpression { value, typ } = exp {
            let value = self.evaluate(scope, *value)?;
            Ok(match value.into_type(self, scope, &typ) {
                Ok(x) => RuntimeValue::Result(
                    Ok(Box::new(x.clone())),
                    RuntimeType::Result(Box::new(RuntimeType::Dynamic), Box::new((&x).into())),
                ),
                Err(e) => RuntimeValue::Result(
                    Err(Box::new(RuntimeValue::Str(String::from(e.to_string())))),
                    RuntimeType::Result(Box::new(RuntimeType::Str), Box::new(RuntimeType::Dynamic)),
                ),
            })
        } else {
            Err(InterpreterErr::NotImplemented(exp))
        }
    }

    pub fn evaluate_binary_expression(
        &mut self,
        scope: &u64,
        exp: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let NodeType::BinaryExpression {
            left,
            right,
            operator,
        } = exp
        {
            let left = self.evaluate(scope, *left)?;
            let right = self.evaluate(scope, *right)?;

            operator.handle(self, scope, left, right)
        } else {
            Err(InterpreterErr::NotImplemented(exp))
        }
    }

    pub fn evaluate_range_expression(
        &mut self,
        scope: &u64,
        exp: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let NodeType::RangeDeclaration {
            from,
            to,
            inclusive,
        } = exp
        {
            if let RuntimeValue::Int(from) =
                self.evaluate(scope, *from.clone())?
                    .into_type(self, scope, &RuntimeType::Int)?
            {
                if let RuntimeValue::Int(to) =
                    self.evaluate(scope, *to.clone())?
                        .into_type(self, scope, &RuntimeType::Int)?
                {
                    let to = if inclusive { to + 1 } else { to };

                    Ok(RuntimeValue::Range(from as i32, to as i32))
                } else {
                    Err(InterpreterErr::NotImplemented(*to))
                }
            } else {
                Err(InterpreterErr::NotImplemented(*from))
            }
        } else {
            Err(InterpreterErr::NotImplemented(exp))
        }
    }

    pub fn evaluate_pipe_expression(
        &mut self,
        scope: &u64,
        exp: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let NodeType::PipeExpression { nodes } = exp {
            let mut current = RuntimeValue::Null;
            let new_scope = self.new_scope_from_parent_shallow(*scope);

            for (i, node) in nodes.iter().enumerate() {
                let mut value = self.evaluate(&new_scope, node.clone())?;

                value = value.unwrap_val(self, &new_scope)?;

                if let Ok(RuntimeValue::Function { .. }) = current.unwrap(self, &new_scope) {
                    let temp = current.unwrap_val(self, &new_scope)?;
                    current = value;
                    value = temp;
                }

                self.force_var(
                    &new_scope,
                    format!("$-{}", i),
                    Variable {
                        value: current.clone(),
                        var_type: VarType::Mutable,
                    },
                )?;
                self.force_var(
                    &new_scope,
                    "$".to_string(),
                    Variable {
                        value: current.clone(),
                        var_type: VarType::Mutable,
                    },
                )?;
                if current != RuntimeValue::Null {
                    if let RuntimeValue::Function { .. } = value {
                        current = self.evaluate_function(
                            &new_scope,
                            value,
                            vec![(NodeType::Identifier(format!("$-{}", i)), None)],
                        )?;
                        self.force_var(
                            &new_scope,
                            "$".to_string(),
                            Variable {
                                value: current.clone(),
                                var_type: VarType::Mutable,
                            },
                        )?;
                        continue;
                    }
                }
                current = value;
            }

            let res = Ok(current.unwrap_links_val(self, &new_scope, Some(new_scope))?);
            self.remove_scope(&new_scope);
            res
        } else {
            Err(InterpreterErr::NotImplemented(exp))
        }
    }

    pub fn evaluate_boolean_expression(
        &mut self,
        scope: &u64,
        exp: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let NodeType::BooleanExpression {
            left,
            right,
            operator,
        } = exp
        {
            let left = self
                .evaluate(scope, *left)?
                .into_type(self, scope, &RuntimeType::Bool)?;
            let right = match self.evaluate(scope, *right) {
                Ok(x) => x
                    .into_type(self, scope, &RuntimeType::Bool)
                    .unwrap_or(RuntimeValue::Null),
                _ => RuntimeValue::Null,
            };

            Ok(operator.handle(&left, &right)?)
        } else {
            Err(InterpreterErr::NotImplemented(exp))
        }
    }

    pub fn evaluate_is_expression(
        &mut self,
        scope: &u64,
        exp: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let NodeType::IsDeclaration { value, data_type } = exp {
            let value = self
                .evaluate(scope, *value)?
                .unwrap_links_val(self, scope, None)?;

            match data_type {
                RuntimeType::Struct(_, Some(x)) if &x == "number" => {
                    return Ok(RuntimeValue::Bool(value.is_number()));
                }
                RuntimeType::Struct(_, Some(x)) if &x == "decimal" => {
                    return Ok(RuntimeValue::Bool(
                        [RuntimeType::Float, RuntimeType::Double].contains(&(&value).into()),
                    ));
                }
                RuntimeType::Struct(_, Some(x)) if &x == "integer" => {
                    return Ok(RuntimeValue::Bool(
                        value.is_number()
                            && ![RuntimeType::Float, RuntimeType::Double]
                                .contains(&(&value).into()),
                    ));
                }
                _ => {}
            }
            Ok(RuntimeValue::Bool(value.is_type(self, scope, &data_type)))
        } else {
            Err(InterpreterErr::NotImplemented(exp))
        }
    }

    pub fn evaluate_comparison_expression(
        &mut self,
        scope: &u64,
        exp: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let NodeType::ComparisonExpression {
            left,
            right,
            operator,
        } = exp
        {
            let left = self.evaluate(scope, *left)?;
            let right = self.evaluate(scope, *right)?;
            operator.handle(self, scope, left, right)
        } else {
            Err(InterpreterErr::NotImplemented(exp))
        }
    }
    pub fn evaluate_assignment_expression(
        &mut self,
        scope: &u64,
        node: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let NodeType::AssignmentExpression { identifier, value } = node {
            let value = self.evaluate(scope, *value)?;
            if let NodeType::Identifier(identifier) = *identifier {
                let _ = self.assign_var(scope, &identifier, value.clone())?;
                return Ok(value);
            } else if let NodeType::MemberExpression { .. } = *identifier {
                let _ = self.assign_member_expression(scope, *identifier, value.clone())?;
                return Ok(value);
            } else {
                Err(InterpreterErr::AssignNonVariable(*identifier))
            }
        } else {
            Err(InterpreterErr::NotImplemented(node))
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{NodeType, binary::BinaryOperator};
    use crate::runtime::values::RuntimeValue;
    use crate::runtime::values::helper::VarType;
    use std::path::PathBuf;
    use std::str::FromStr;

    fn get_new_env() -> (Environment, u64) {
        let mut env = Environment::new();
        let scope = env.new_scope_with_stdlib(None, PathBuf::from_str("./main.cl").unwrap(), None);
        (env, scope)
    }

    #[test]
    fn test_evaluate_identifier() {
        let (mut env, scope) = get_new_env();
        env.push_var(
            &scope,
            "x".to_string(),
            Variable {
                value: RuntimeValue::Int(5),
                var_type: VarType::Mutable,
            },
        )
        .unwrap();
        let result = env.evaluate_identifier(&scope, "x").unwrap().unwrap_val(&env, &scope).unwrap();
        assert_eq!(result, RuntimeValue::Int(5));
    }

    #[test]
    fn test_evaluate_not_bool() {
        let (mut env, scope) = get_new_env();
        let node = NodeType::NotExpression {
            value: Box::new(NodeType::Identifier(String::from("true"))),
        };
        let result = env.evaluate_not(&scope, node).unwrap();
        assert_eq!(result, RuntimeValue::Bool(false));
    }

    #[test]
    fn test_evaluate_not_int() {
        let (mut env, scope) = get_new_env();
        let node = NodeType::NotExpression {
            value: Box::new(NodeType::IntLiteral(7)),
        };
        let result = env.evaluate_not(&scope, node).unwrap();
        assert_eq!(result, RuntimeValue::Int(-7));
    }

    #[test]
    fn test_evaluate_not_float() {
        let (mut env, scope) = get_new_env();
        let node = NodeType::NotExpression {
            value: Box::new(NodeType::FloatLiteral(3.5)),
        };
        let result = env.evaluate_not(&scope, node).unwrap();
        assert_eq!(result, RuntimeValue::Float(-3.5));
    }

    #[test]
    fn test_evaluate_not_range() {
        let (mut env, scope) = get_new_env();

        let node = NodeType::NotExpression {
            value: Box::new(NodeType::RangeDeclaration {
                from: Box::new(NodeType::IntLiteral(1)),
                to: Box::new(NodeType::IntLiteral(3)),
                inclusive: false,
            }),
        };

        let result = env.evaluate_not(&scope, node).unwrap();
        assert_eq!(result, RuntimeValue::Range(3, 1));
    }

    #[test]
    fn test_evaluate_not_list() {
        let (mut env, scope) = get_new_env();
        let node = NodeType::NotExpression {
            value: Box::new(NodeType::ListLiteral(vec![
                NodeType::IntLiteral(1),
                NodeType::IntLiteral(2),
                NodeType::IntLiteral(3),
            ])),
        };
        let result = env.evaluate_not(&scope, node).unwrap();
        match result {
            RuntimeValue::List { data, .. } => assert_eq!(
                data,
                vec![
                    RuntimeValue::UInt(3),
                    RuntimeValue::UInt(2),
                    RuntimeValue::UInt(1)
                ]
            ),
            _ => panic!("Expected List"),
        }
    }

    #[test]
    fn test_evaluate_binary_expression_add() {
        let (mut env, scope) = get_new_env();
        let node = NodeType::BinaryExpression {
            left: Box::new(NodeType::IntLiteral(2)),
            right: Box::new(NodeType::IntLiteral(3)),
            operator: BinaryOperator::Add,
        };
        let result = env.evaluate_binary_expression(&scope, node).unwrap();
        assert_eq!(result, RuntimeValue::UInt(5));
    }

    #[test]
    fn test_evaluate_range_expression_inclusive() {
        let (mut env, scope) = get_new_env();
        let node = NodeType::RangeDeclaration {
            from: Box::new(NodeType::IntLiteral(1)),
            to: Box::new(NodeType::IntLiteral(3)),
            inclusive: true,
        };
        let result = env.evaluate_range_expression(&scope, node).unwrap();
        assert_eq!(result, RuntimeValue::Range(1, 4));
    }

    #[test]
    fn test_evaluate_range_expression_exclusive() {
        let (mut env, scope) = get_new_env();

        let node = NodeType::RangeDeclaration {
            from: Box::new(NodeType::IntLiteral(1)),
            to: Box::new(NodeType::IntLiteral(3)),
            inclusive: false,
        };
        let result = env.evaluate_range_expression(&scope, node).unwrap();
        assert_eq!(result, RuntimeValue::Range(1, 3));
    }

    #[test]
    fn test_evaluate_boolean_expression_and() {
        let (mut env, scope) = get_new_env();
        let node = NodeType::BooleanExpression {
            left: Box::new(NodeType::Identifier(String::from("true"))),
            right: Box::new(NodeType::Identifier(String::from("false"))),
            operator: crate::ast::comparison::BooleanOperation::And,
        };
        let result = env.evaluate_boolean_expression(&scope, node).unwrap();
        assert_eq!(result, RuntimeValue::Bool(false));
    }

    #[test]
    fn test_evaluate_comparison_expression_lesser() {
        let (mut env, scope) = get_new_env();
        let node = NodeType::ComparisonExpression {
            left: Box::new(NodeType::IntLiteral(3)),
            right: Box::new(NodeType::IntLiteral(2)),
            operator: crate::ast::comparison::Comparison::Greater,
        };
        let result = env.evaluate_comparison_expression(&scope, node).unwrap();
        assert_eq!(result, RuntimeValue::Bool(true));
    }

    #[test]
    fn test_evaluate_assignment_expression_identifier() {
        let (mut env, scope) = get_new_env();
        env.push_var(
            &scope,
            "x".to_string(),
            Variable {
                value: RuntimeValue::UInt(0),
                var_type: VarType::Mutable,
            },
        )
        .unwrap();

        let node = NodeType::AssignmentExpression {
            identifier: Box::new(NodeType::Identifier("x".to_string())),
            value: Box::new(NodeType::IntLiteral(42)),
        };

        let result = env.evaluate_assignment_expression(&scope, node).unwrap().unwrap_val(&env, &scope).unwrap();
        assert_eq!(result, RuntimeValue::UInt(42));
        assert_eq!(
            env.get_var(&scope, "x").unwrap().value,
            RuntimeValue::UInt(42)
        );
    }
}
