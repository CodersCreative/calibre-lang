use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{
        comparison::{self, Comparison}, IfComparisonType, NodeType
    },
    runtime::{
        interpreter::{
             InterpreterErr
        },
        scope::{Environment, Scope},
        values::{helper::StopValue, RuntimeValue},
    },
};

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
            RuntimeValue::List { data, data_type } => self.value_in_list(scope, value, data),
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
            comparisons,
            bodies,
        } = declaration
        {
            for (i, comparison) in comparisons.iter().enumerate() {
                match comparison {
                    IfComparisonType::If(comparison) => {
                        if let RuntimeValue::Bool(x) = self.evaluate(scope, comparison.clone())? {
                            if x {
                                return self.evaluate(
                                    scope,
                                    bodies[i].clone(),
                                );
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
                        let path = match &*value {
                            NodeType::Identifier(x) => vec![x.clone()],
                            _ => Vec::new(),
                        };

                        let value = self.evaluate(scope, value.clone(), )?;

                        if let Some(result) = self.match_pattern(
                            scope,
                            &pattern.0,
                            &value,
                            &mutability,
                            path.clone(),
                            &pattern.1,
                            bodies[i].clone(),
                        ) {
                            match result {
                                Ok(x) => return Ok(x),
                                Err(InterpreterErr::ExpectedFunctions) => continue,
                                Err(e) => return Err(e),
                            }
                        }
                    }
                }
            }

            if comparisons.len() < bodies.len() {
                if let Some(last) = bodies.last() {
                    return self.evaluate(
                        scope,
                        last.clone(),
                    );
                }
            }

            Ok(RuntimeValue::Null)
        } else {
            Err(InterpreterErr::NotImplemented(declaration))
        }
    }
}




#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};

    use crate::{
        ast::NodeType,
        runtime::{
            interpreter::statements::comparisons::evaluate_if_statement, scope::Scope,
            values::RuntimeValue,
        },
    };

    fn new_scope() -> Rc<RefCell<Scope>> {
        Rc::new(RefCell::new(Scope::new(None)))
    }

    #[test]
    fn test_evaluate_if_statement_true_branch() {
        let scope = new_scope();
        let node = NodeType::IfStatement {
            comparisons: vec![NodeType::Identifier(String::from("true"))],
            bodies: vec![vec![NodeType::IntLiteral(123)]],
        };
        let result = evaluate_if_statement(node, &scope).unwrap();
        assert_eq!(result, RuntimeValue::Int(123));
    }

    #[test]
    fn test_evaluate_if_statement_false_else() {
        let scope = new_scope();
        let node = NodeType::IfStatement {
            comparisons: vec![NodeType::Identifier(String::from("false"))],
            bodies: vec![vec![NodeType::IntLiteral(1)], vec![NodeType::IntLiteral(2)]],
        };
        let result = evaluate_if_statement(node, &scope).unwrap();
        assert_eq!(result, RuntimeValue::Int(2));
    }
}
