use crate::{
    operators,
    runtime::{
        interpreter::{InterpreterErr, expressions::member::MembrExprPathRes},
        scope::InterpreterEnvironment,
        values::{RuntimeType, RuntimeValue},
    },
};
use calibre_parser::ast::{
    Node, NodeType,
    binary::BinaryOperator,
    comparison::{BooleanOperation, Comparison},
};
pub mod call;
pub mod lists;
pub mod member;
pub mod scope;
pub mod structs;

impl InterpreterEnvironment {
    pub fn evaluate_identifier(
        &mut self,
        scope: &u64,
        identifier: &str,
    ) -> Result<RuntimeValue, InterpreterErr> {
        Ok(self.get_var(scope, identifier)?.value.clone())
    }

    pub fn evaluate_not<'a>(
        &mut self,
        _scope: &u64,
        value: RuntimeValue,
    ) -> Result<RuntimeValue, InterpreterErr> {
        match value {
            RuntimeValue::Bool(x) => Ok(RuntimeValue::Bool(!x)),
            RuntimeValue::Int(x) => Ok(RuntimeValue::Int(-x)),
            RuntimeValue::Float(x) => Ok(RuntimeValue::Float(-x)),
            RuntimeValue::Range(f, t) => Ok(RuntimeValue::Range(t, f)),
            RuntimeValue::List {
                mut data,
                data_type,
            } => {
                data.reverse();
                Ok(RuntimeValue::List { data, data_type })
            }
            _ => Err(InterpreterErr::CantPerformNot(value)),
        }
    }

    pub fn evaluate_as_expression(
        &mut self,
        scope: &u64,
        value: RuntimeValue,
        data_type: RuntimeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        Ok(match value.into_type(self, scope, &data_type) {
            Ok(x) => RuntimeValue::Result(
                Ok(Box::new(x.clone())),
                RuntimeType::Result(Box::new(RuntimeType::Dynamic), Box::new((&x).into())),
            ),
            Err(e) => RuntimeValue::Result(
                Err(Box::new(RuntimeValue::Str(String::from(e.to_string())))),
                RuntimeType::Result(Box::new(RuntimeType::Str), Box::new(RuntimeType::Dynamic)),
            ),
        })
    }

    pub fn evaluate_binary_expression(
        &mut self,
        scope: &u64,
        left: RuntimeValue,
        right: RuntimeValue,
        operator: BinaryOperator,
    ) -> Result<RuntimeValue, InterpreterErr> {
        operators::binary::handle(&operator, self, scope, left, right)
    }

    pub fn evaluate_range_expression(
        &mut self,
        _scope: &u64,
        from: RuntimeValue,
        to: RuntimeValue,
        inclusive: bool,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let RuntimeValue::Int(from) = from {
            if let RuntimeValue::Int(to) = to {
                let to = if inclusive { to + 1 } else { to };

                Ok(RuntimeValue::Range(from, to))
            } else {
                Err(InterpreterErr::UnexpectedType(to))
            }
        } else {
            Err(InterpreterErr::UnexpectedType(from))
        }
    }

    pub fn evaluate_pipe_expression(
        &mut self,
        scope: &u64,
        values: Vec<Node>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        let mut pre_values: Vec<Node> = Vec::new();
        for value in values.into_iter() {
            match value.node_type {
                NodeType::Identifier(_)
                | NodeType::MatchDeclaration { .. }
                | NodeType::FunctionDeclaration { .. }
                | NodeType::MemberExpression { .. }
                | NodeType::EnumExpression { .. } => {
                    let calculated = self.evaluate(scope, value.clone());
                    match calculated {
                        Ok(RuntimeValue::Function { .. }) => {
                            let args: Vec<(Node, Option<Node>)> =
                                pre_values.iter().map(|x| (x.clone(), None)).collect();
                            pre_values.clear();
                            pre_values.push(Node::new(
                                NodeType::CallExpression(Box::new(value.clone()), args),
                                value.span,
                            ));
                        }
                        _ => pre_values.push(value),
                    }
                }
                _ => {
                    pre_values.push(value);
                }
            }
        }

        self.evaluate(scope, pre_values.pop().unwrap())
    }

    pub fn evaluate_boolean_expression(
        &mut self,
        _scope: &u64,
        left: RuntimeValue,
        right: RuntimeValue,
        operator: BooleanOperation,
    ) -> Result<RuntimeValue, InterpreterErr> {
        Ok(operators::boolean::handle(&operator, &left, &right)?)
    }

    pub fn evaluate_is_expression(
        &mut self,
        scope: &u64,
        value: RuntimeValue,
        data_type: RuntimeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        match data_type {
            RuntimeType::Struct(_, Some(x)) if &x == "number" => {
                Ok(RuntimeValue::Bool(value.is_number()))
            }
            _ => Ok(RuntimeValue::Bool(value.is_type(
                self,
                scope,
                &data_type.into(),
            ))),
        }
    }

    pub fn evaluate_comparison_expression(
        &mut self,
        scope: &u64,
        left: RuntimeValue,
        right: RuntimeValue,
        operator: Comparison,
    ) -> Result<RuntimeValue, InterpreterErr> {
        operators::comparison::handle(&operator, self, scope, left, right)
    }

    pub fn evaluate_assignment_expression(
        &mut self,
        scope: &u64,
        identifier: Node,
        value: RuntimeValue,
    ) -> Result<RuntimeValue, InterpreterErr> {
        match identifier.node_type {
            NodeType::DerefStatement { value: node } => {
                if let RuntimeValue::Ref(pointer, _) = match node.node_type.clone() {
                    NodeType::Identifier(x) => self.get_var_ref(scope, &x)?,
                    NodeType::MemberExpression { path } => {
                        let MembrExprPathRes::Path(path) =
                            self.get_member_expression_path(scope, path)?
                        else {
                            return Err(InterpreterErr::RefNonVar(node.node_type));
                        };

                        self.get_member_ref(scope, &path)?
                    }
                    _ => return Err(InterpreterErr::RefNonVar(node.node_type)),
                } {
                    let _ = self.assign_var_from_ref_pointer(&pointer, value)?;
                    Ok(RuntimeValue::Null)
                } else {
                    Err(InterpreterErr::AssignNonVariable(node.node_type))
                }
            }
            NodeType::Identifier(identifier) => {
                let _ = self.assign_var(scope, &identifier, value.clone())?;
                Ok(RuntimeValue::Null)
            }
            NodeType::MemberExpression { .. } | NodeType::ScopeMemberExpression { .. } => {
                let _ = self.assign_member_expression(scope, identifier, value.clone())?;
                Ok(RuntimeValue::Null)
            }
            _ => Err(InterpreterErr::AssignNonVariable(identifier.node_type)),
        }
    }
}

#[cfg(test)]
mod tests {
    use calibre_parser::ast::VarType;
    use calibre_parser::ast::binary::BinaryOperator;
    use calibre_parser::ast::comparison::{BooleanOperation, Comparison};

    use super::*;
    use crate::runtime::scope::Variable;
    use crate::runtime::values::RuntimeValue;
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
        let result = env
            .evaluate_identifier(&scope, "x")
            .unwrap()
            .unwrap_val(&env, &scope)
            .unwrap();
        assert_eq!(result, RuntimeValue::Int(5));
    }

    #[test]
    fn test_evaluate_not_bool() {
        let (mut env, scope) = get_new_env();
        let node = NodeType::NotExpression {
            value: Box::new(NodeType::Identifier(String::from("true"))),
        };
        let result = env.evaluate(&scope, node).unwrap();
        assert_eq!(result, RuntimeValue::Bool(false));
    }

    #[test]
    fn test_evaluate_not_int() {
        let (mut env, scope) = get_new_env();
        let node = NodeType::NotExpression {
            value: Box::new(NodeType::IntLiteral(7)),
        };
        let result = env.evaluate(&scope, node).unwrap();
        assert_eq!(result, RuntimeValue::Int(-7));
    }

    #[test]
    fn test_evaluate_not_float() {
        let (mut env, scope) = get_new_env();
        let node = NodeType::NotExpression {
            value: Box::new(NodeType::FloatLiteral(3.5)),
        };
        let result = env.evaluate(&scope, node).unwrap();
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

        let result = env.evaluate(&scope, node).unwrap();
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
        let result = env.evaluate(&scope, node).unwrap();
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
        let result = env.evaluate(&scope, node).unwrap();
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
        let result = env.evaluate(&scope, node).unwrap();
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
        let result = env.evaluate(&scope, node).unwrap();
        assert_eq!(result, RuntimeValue::Range(1, 3));
    }

    #[test]
    fn test_evaluate_boolean_expression_and() {
        let (mut env, scope) = get_new_env();
        let node = NodeType::BooleanExpression {
            left: Box::new(NodeType::Identifier(String::from("true"))),
            right: Box::new(NodeType::Identifier(String::from("false"))),
            operator: BooleanOperation::And,
        };
        let result = env.evaluate(&scope, node).unwrap();
        assert_eq!(result, RuntimeValue::Bool(false));
    }

    #[test]
    fn test_evaluate_comparison_expression_lesser() {
        let (mut env, scope) = get_new_env();
        let node = NodeType::ComparisonExpression {
            left: Box::new(NodeType::IntLiteral(3)),
            right: Box::new(NodeType::IntLiteral(2)),
            operator: Comparison::Greater,
        };
        let result = env.evaluate(&scope, node).unwrap();
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

        let result = env
            .evaluate(&scope, node)
            .unwrap()
            .unwrap_val(&env, &scope)
            .unwrap();
        assert_eq!(result, RuntimeValue::UInt(42));
        assert_eq!(
            env.get_var(&scope, "x").unwrap().value,
            RuntimeValue::UInt(42)
        );
    }
}
