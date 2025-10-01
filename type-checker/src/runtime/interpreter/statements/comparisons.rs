use crate::runtime::{
    interpreter::InterpreterErr, scope::CheckerEnvironment, values::RuntimeType
};
use calibre_parser::ast::{IfComparisonType, Node, NodeType};

impl CheckerEnvironment {
    pub fn evaluate_in_statement(
        &mut self,
        scope: &u64,
        left: RuntimeType,
        right: RuntimeType,
    ) -> Result<RuntimeType, InterpreterErr> {
            Ok(RuntimeType::Bool)
    }

    pub fn evaluate_if_statement(
        &mut self,
        scope: &u64,
        comparison : IfComparisonType,
        then : Node,
        otherwise: Option<Node>,
    ) -> Result<RuntimeType, InterpreterErr> {
            let mut result = RuntimeType::Null;
            match comparison {
                IfComparisonType::If(comparison) => {
                    let res = self
                        .evaluate(scope, comparison.clone());
                    if let Ok(RuntimeType::Bool) = res
                    {
                        result = self.evaluate(scope, then)?;
                    } else {
                        println!("{:?} - {:?}", self.current_location, res);
                        return Err(InterpreterErr::ExpectedOperation(String::from("boolean")));
                    }
                }
                IfComparisonType::IfLet {
                    mutability,
                    value,
                    pattern,
                } => {
                    let path = match &value.node_type {
                        NodeType::Identifier(x) => vec![x.clone()],
                        _ => Vec::new(),
                    };

                    let value = self.evaluate(scope, value.clone())?;
                    result = RuntimeType::Dynamic;
                }
            }

            if let Some(last) = otherwise {
                result = self.evaluate(scope, last)?;
            }

            Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use calibre_parser::ast::{IfComparisonType, NodeType};

    use crate::runtime::{scope::Environment, values::RuntimeValue};
    use std::{path::PathBuf, str::FromStr};

    fn get_new_env() -> (Environment, u64) {
        let mut env = Environment::new();
        let scope = env.new_scope_with_stdlib(None, PathBuf::from_str("./main.cl").unwrap(), None);
        (env, scope)
    }

    #[test]
    fn test_evaluate_if_statement_true_branch() {
        let (mut env, scope) = get_new_env();
        let node = NodeType::IfStatement {
            comparison: Box::new(IfComparisonType::If(NodeType::Identifier(String::from(
                "true",
            )))),
            then: Box::new(NodeType::IntLiteral(123)),
            otherwise : None
        };
        let result = env.evaluate(&scope, node).unwrap();

        assert!(env.is_equal(&scope, &result, &RuntimeValue::Int(123)));
    }

    #[test]
    fn test_evaluate_if_statement_false_else() {
        let (mut env, scope) = get_new_env();
        let node = NodeType::IfStatement {
            comparison: Box::new(IfComparisonType::If(NodeType::Identifier(String::from(
                "false",
            )))),
            then: Box::new(NodeType::IntLiteral(123)),
            otherwise : Some(Box::new(NodeType::IntLiteral(2)))
        };
        let result = env.evaluate(&scope, node).unwrap();

        assert!(env.is_equal(&scope, &result, &RuntimeValue::Int(2)));
    }
}
