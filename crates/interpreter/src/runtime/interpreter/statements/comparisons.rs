use crate::runtime::{
    interpreter::InterpreterErr, scope::InterpreterEnvironment, values::RuntimeValue,
};
use calibre_mir::ast::MiddleNode;

impl InterpreterEnvironment {
    fn value_in_list(&self, scope: &u64, value: &RuntimeValue, list: &[RuntimeValue]) -> bool {
        for val in list {
            if self.is_equal(scope, value, val) {
                return true;
            }
        }

        false
    }

    pub fn is_value_in(&self, scope: &u64, value: &RuntimeValue, other: &RuntimeValue) -> bool {
        match other {
            RuntimeValue::Str(x) => match value {
                RuntimeValue::Char(y) => return x.contains(*y),
                RuntimeValue::Str(y) => return x.contains(y),
                _ => false,
            },
            RuntimeValue::List { data, .. } => self.value_in_list(scope, value, data),
            RuntimeValue::Tuple(data) => self.value_in_list(scope, value, data),
            RuntimeValue::Range(from, to) => {
                let num: f64 = match value {
                    RuntimeValue::Range(x, y) => (*x as f64 + *y as f64) / 2.0,
                    RuntimeValue::Int(x) => *x as f64,
                    RuntimeValue::Float(x) => *x as f64,
                    _ => return false,
                };
                num >= *from as f64 && num < *to as f64
            }
            _ => false,
        }
    }

    pub fn evaluate_in_statement(
        &mut self,
        scope: &u64,
        left: RuntimeValue,
        right: RuntimeValue,
    ) -> Result<RuntimeValue, InterpreterErr> {
        Ok(RuntimeValue::Bool(self.is_value_in(scope, &left, &right)))
    }

    pub fn evaluate_if_statement(
        &mut self,
        scope: &u64,
        comparison: MiddleNode,
        then: MiddleNode,
        otherwise: Option<MiddleNode>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let RuntimeValue::Bool(x) = self.evaluate(scope, comparison.clone())? {
            if x {
                return self.evaluate(scope, then);
            }
        } else {
            return Err(InterpreterErr::ExpectedOperation(String::from("boolean")));
        }

        if let Some(last) = otherwise {
            return self.evaluate(scope, last);
        }

        Ok(RuntimeValue::Null)
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
            otherwise: None,
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
            otherwise: Some(Box::new(NodeType::IntLiteral(2))),
        };
        let result = env.evaluate(&scope, node).unwrap();

        assert!(env.is_equal(&scope, &result, &RuntimeValue::Int(2)));
    }
}
