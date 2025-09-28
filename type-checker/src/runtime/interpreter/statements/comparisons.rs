use crate::runtime::{
    interpreter::InterpreterErr,
    scope::Environment,
    values::{RuntimeType},
};
use calibre_parser::ast::{IfComparisonType, NodeType};

impl Environment {
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
        then : NodeType,
        otherwise: Option<NodeType>,
    ) -> Result<RuntimeType, InterpreterErr> {
            match comparison {
                IfComparisonType::If(comparison) => {
                    if let Ok(RuntimeType::Bool) = self
                        .evaluate(scope, comparison.clone())?
                        .into_type(self, scope, &RuntimeType::Bool)
                    {
                        return self.evaluate(scope, then);
                    } else {
                        return Err(InterpreterErr::ExpectedOperation(String::from("boolean")));
                    }
                }
                IfComparisonType::IfLet {
                    mutability,
                    value,
                    pattern,
                } => {
                    let path = match &value {
                        NodeType::Identifier(x) => vec![x.clone()],
                        _ => Vec::new(),
                    };

                    let value = self.evaluate(scope, value.clone())?;

                    if let Some(result) = self.match_pattern(
                        scope,
                        &pattern.0,
                        &value,
                        &mutability,
                        path.clone(),
                        &pattern.1,
                        then,
                    ) {
                        match result {
                            Ok(x) => return Ok(x),
                            Err(InterpreterErr::ExpectedFunctions) => {}
                            Err(e) => return Err(e),
                        }
                    }
                }
            }

            if let Some(last) = otherwise {
                return self.evaluate(scope, last);
            }

            Ok(RuntimeType::Null)
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
