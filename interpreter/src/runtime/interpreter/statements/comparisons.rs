use crate::runtime::{
    interpreter::InterpreterErr,
    scope::Environment,
    values::{RuntimeType, RuntimeValue},
};
use calibre_parser::ast::{IfComparisonType, NodeType};

impl Environment {
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
                    RuntimeValue::UInt(x) => *x as f64,
                    RuntimeValue::Long(x) => *x as f64,
                    RuntimeValue::ULong(x) => *x as f64,
                    RuntimeValue::Float(x) => *x as f64,
                    RuntimeValue::Double(x) => *x,
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
        declaration: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let NodeType::InDeclaration {
            identifier,
            expression,
        } = declaration
        {
            let value = self.evaluate(scope, *expression)?;
            let ident = self.evaluate(scope, *identifier)?;

            Ok(RuntimeValue::Bool(self.is_value_in(scope, &ident, &value)))
        } else {
            Err(InterpreterErr::NotImplemented(declaration))
        }
    }

    pub fn evaluate_if_statement(
        &mut self,
        scope: &u64,
        declaration: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let NodeType::IfStatement {
            comparison,
            then,
            otherwise,
        } = declaration
        {
            match *comparison {
                IfComparisonType::If(comparison) => {
                    if let Ok(RuntimeValue::Bool(x)) = self
                        .evaluate(scope, comparison.clone())?
                        .unwrap_val(self, scope)?
                        .into_type(self, scope, &RuntimeType::Bool)
                    {
                        if x {
                            return self.evaluate(scope, *then.clone());
                        }
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
                        *then.clone(),
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
                return self.evaluate(scope, *last.clone());
            }

            Ok(RuntimeValue::Null)
        } else {
            Err(InterpreterErr::NotImplemented(declaration))
        }
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
            comparisons: vec![IfComparisonType::If(NodeType::Identifier(String::from(
                "true",
            )))],
            bodies: vec![NodeType::IntLiteral(123)],
        };
        let result = env.evaluate_if_statement(&scope, node).unwrap();

        assert!(env.is_equal(&scope, &result, &RuntimeValue::Int(123)));
    }

    #[test]
    fn test_evaluate_if_statement_false_else() {
        let (mut env, scope) = get_new_env();
        let node = NodeType::IfStatement {
            comparisons: vec![IfComparisonType::If(NodeType::Identifier(String::from(
                "false",
            )))],
            bodies: vec![NodeType::IntLiteral(123), NodeType::IntLiteral(2)],
        };
        let result = env.evaluate_if_statement(&scope, node).unwrap();

        assert!(env.is_equal(&scope, &result, &RuntimeValue::Int(2)));
    }
}
