use crate::{
    operators,
    runtime::{interpreter::InterpreterErr, scope::CheckerEnvironment, values::RuntimeType},
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

impl CheckerEnvironment {
    pub fn evaluate_identifier(
        &mut self,
        scope: &u64,
        identifier: &str,
    ) -> Result<RuntimeType, InterpreterErr> {
        Ok(self.get_var(scope, identifier)?.value.clone())
    }

    pub fn evaluate_not<'a>(
        &mut self,
        _scope: &u64,
        value: RuntimeType,
    ) -> Result<RuntimeType, InterpreterErr> {
        return Ok(value);
    }

    pub fn evaluate_as_expression(
        &mut self,
        scope: &u64,
        value: RuntimeType,
        data_type: RuntimeType,
    ) -> Result<RuntimeType, InterpreterErr> {
        Ok(match value.into_type(self, scope, &data_type) {
            Ok(x) => RuntimeType::Result(Box::new(RuntimeType::Dynamic), Box::new(x)),
            Err(e) => return Err(e.into()),
        })
    }

    pub fn evaluate_binary_expression(
        &mut self,
        scope: &u64,
        left: RuntimeType,
        right: RuntimeType,
        operator: BinaryOperator,
    ) -> Result<RuntimeType, InterpreterErr> {
        operators::binary::handle(&operator, self, scope, left, right)
    }

    pub fn evaluate_range_expression(
        &mut self,
        _scope: &u64,
        from: RuntimeType,
        to: RuntimeType,
        _inclusive: bool,
    ) -> Result<RuntimeType, InterpreterErr> {
        match (&from, &to) {
            (RuntimeType::Int, RuntimeType::Int) => Ok(RuntimeType::Range),
            _ => Err(InterpreterErr::ExpectedType(from, RuntimeType::Int)),
        }
    }

    pub fn evaluate_pipe_expression(
        &mut self,
        scope: &u64,
        values: Vec<Node>,
    ) -> Result<RuntimeType, InterpreterErr> {
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
                        Ok(RuntimeType::Function { .. }) => {
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
        left: RuntimeType,
        right: RuntimeType,
        operator: BooleanOperation,
    ) -> Result<RuntimeType, InterpreterErr> {
        Ok(operators::boolean::handle(&operator, &left, &right)?)
    }

    pub fn evaluate_comparison_expression(
        &mut self,
        scope: &u64,
        left: RuntimeType,
        right: RuntimeType,
        operator: Comparison,
    ) -> Result<RuntimeType, InterpreterErr> {
        operators::comparison::handle(&operator, self, scope, left, right)
    }

    pub fn evaluate_assignment_expression(
        &mut self,
        scope: &u64,
        identifier: Node,
        value: RuntimeType,
    ) -> Result<RuntimeType, InterpreterErr> {
        if let NodeType::Identifier(identifier) = identifier.node_type {
            let _ = self.assign_var(scope, &identifier, value.clone())?;
            return Ok(value);
        } else if let NodeType::MemberExpression { .. } = identifier.node_type {
            let _ = self.assign_member_expression(scope, identifier, value.clone())?;
            return Ok(value);
        } else {
            Err(InterpreterErr::AssignNonVariable(identifier.node_type))
        }
    }
}
#[cfg(test)]
mod tests {
    use calibre_common::environment::{Location, Variable};
    use calibre_parser::ast::VarType;
    use calibre_parser::ast::binary::BinaryOperator;
    use calibre_parser::ast::comparison::{BooleanOperation, Comparison};
    use calibre_parser::lexer::Span;

    use super::*;
    use std::path::PathBuf;
    use std::str::FromStr;

    fn get_new_env() -> (CheckerEnvironment, u64) {
        let mut env = CheckerEnvironment::new();
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
                value: RuntimeType::Int,
                var_type: VarType::Mutable,
                location: None,
            },
        )
        .unwrap();
        let result = env
            .evaluate(
                &scope,
                Node::new_from_type(NodeType::Identifier(String::from("x"))),
            )
            .unwrap();
        assert_eq!(result, RuntimeType::Int);
    }

    #[test]
    fn test_evaluate_not_bool() {
        let (mut env, scope) = get_new_env();
        let node = Node::new_from_type(NodeType::NotExpression {
            value: Box::new(Node::new_from_type(NodeType::Identifier(String::from(
                "true",
            )))),
        });
        let result = env.evaluate(&scope, node).unwrap();
        assert_eq!(result, RuntimeType::Bool);
    }

    #[test]
    fn test_evaluate_not_int() {
        let (mut env, scope) = get_new_env();
        let node = Node::new_from_type(NodeType::NotExpression {
            value: Box::new(Node::new_from_type(NodeType::IntLiteral(7))),
        });
        let result = env.evaluate(&scope, node).unwrap();
        assert_eq!(result, RuntimeType::Int);
    }

    #[test]
    fn test_evaluate_not_float() {
        let (mut env, scope) = get_new_env();
        let node = Node::new_from_type(NodeType::NotExpression {
            value: Box::new(Node::new_from_type(NodeType::FloatLiteral(3.5))),
        });
        let result = env.evaluate(&scope, node).unwrap();
        assert_eq!(result, RuntimeType::Float);
    }

    #[test]
    fn test_evaluate_not_range() {
        let (mut env, scope) = get_new_env();

        let node = Node::new_from_type(NodeType::NotExpression {
            value: Box::new(Node::new_from_type(NodeType::RangeDeclaration {
                from: Box::new(Node::new(NodeType::IntLiteral(1), Span::default())),
                to: Box::new(Node::new(NodeType::IntLiteral(3), Span::default())),
                inclusive: false,
            })),
        });

        let result = env.evaluate(&scope, node).unwrap();
        assert_eq!(result, RuntimeType::Range);
    }

    #[test]
    fn test_evaluate_not_list() {
        let (mut env, scope) = get_new_env();
        let node = Node::new_from_type(NodeType::NotExpression {
            value: Box::new(Node::new_from_type(NodeType::ListLiteral(vec![
                Node::new_from_type(NodeType::IntLiteral(1)),
                Node::new_from_type(NodeType::IntLiteral(2)),
                Node::new_from_type(NodeType::IntLiteral(3)),
            ]))),
        });
        let result = env.evaluate(&scope, node).unwrap();
        match result {
            RuntimeType::List(data) => assert_eq!(data, Box::new(Some(RuntimeType::Int)),),
            _ => panic!("Expected List"),
        }
    }

    #[test]
    fn test_evaluate_binary_expression_add() {
        let (mut env, scope) = get_new_env();
        let node = Node::new_from_type(NodeType::BinaryExpression {
            left: Box::new(Node::new_from_type(NodeType::IntLiteral(2))),
            right: Box::new(Node::new_from_type(NodeType::IntLiteral(3))),
            operator: BinaryOperator::Add,
        });
        let result = env.evaluate(&scope, node).unwrap();
        assert_eq!(result, RuntimeType::Int);
    }

    #[test]
    fn test_evaluate_range_expression() {
        let (mut env, scope) = get_new_env();

        let node = Node::new_from_type(NodeType::RangeDeclaration {
            from: Box::new(Node::new_from_type(NodeType::IntLiteral(1))),
            to: Box::new(Node::new_from_type(NodeType::IntLiteral(3))),
            inclusive: false,
        });
        let result = env.evaluate(&scope, node).unwrap();
        assert_eq!(result, RuntimeType::Range);
    }

    #[test]
    fn test_evaluate_boolean_expression_and() {
        let (mut env, scope) = get_new_env();
        let node = Node::new_from_type(NodeType::BooleanExpression {
            left: Box::new(Node::new_from_type(NodeType::Identifier(String::from(
                "true",
            )))),
            right: Box::new(Node::new_from_type(NodeType::Identifier(String::from(
                "false",
            )))),
            operator: BooleanOperation::And,
        });
        let result = env.evaluate(&scope, node).unwrap();
        assert_eq!(result, RuntimeType::Bool);
    }

    #[test]
    fn test_evaluate_comparison_expression_lesser() {
        let (mut env, scope) = get_new_env();
        let node = Node::new_from_type(NodeType::ComparisonExpression {
            left: Box::new(Node::new_from_type(NodeType::IntLiteral(3))),
            right: Box::new(Node::new_from_type(NodeType::IntLiteral(2))),
            operator: Comparison::Greater,
        });
        let result = env.evaluate(&scope, node).unwrap();
        assert_eq!(result, RuntimeType::Bool);
    }
}
