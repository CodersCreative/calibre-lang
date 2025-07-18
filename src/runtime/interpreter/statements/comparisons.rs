use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{
        IfComparisonType, NodeType,
        comparison::{self, Comparison, is_equal},
    },
    runtime::{
        interpreter::{
            InterpreterErr, evaluate, expressions::scope::evaluate_scope,
            statements::matching::match_pattern,
        },
        scope::Scope,
        values::{RuntimeValue, helper::StopValue},
    },
};

fn value_in_list(value: RuntimeValue, list: Vec<RuntimeValue>, scope: Rc<RefCell<Scope>>) -> bool {
    for val in list.into_iter() {
        if is_equal(value.clone(), val, scope.clone()) {
            return true;
        }
    }

    false
}

pub fn is_value_in(value: RuntimeValue, other: RuntimeValue, scope: Rc<RefCell<Scope>>) -> bool {
    match other {
        RuntimeValue::Str(x) => match value {
            RuntimeValue::Char(y) => return x.contains(y),
            RuntimeValue::Str(y) => return x.contains(&y),
            _ => false,
        },
        RuntimeValue::List { data, data_type } => value_in_list(value, data, scope),
        RuntimeValue::Tuple(data) => value_in_list(value, data, scope),
        RuntimeValue::Range(from, to) => {
            let num: f64 = match value {
                RuntimeValue::Range(x, y) => (x as f64 + y as f64) / 2.0,
                RuntimeValue::Int(x) => x as f64,
                RuntimeValue::UInt(x) => x as f64,
                RuntimeValue::Long(x) => x as f64,
                RuntimeValue::ULong(x) => x as f64,
                RuntimeValue::Float(x) => x as f64,
                RuntimeValue::Double(x) => x,
                _ => return false,
            };
            num >= from as f64 && num < to as f64
        }
        _ => false,
    }
}

pub fn evaluate_in_statement(
    declaration: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::InDeclaration {
        identifier,
        expression,
    } = declaration
    {
        let value = evaluate(*expression, scope.clone())?;
        let ident = evaluate(*identifier, scope.clone())?;

        Ok(RuntimeValue::Bool(is_value_in(ident, value, scope)))
    } else {
        Err(InterpreterErr::NotImplemented(declaration))
    }
}

pub fn evaluate_if_statement(
    declaration: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::IfStatement {
        comparisons,
        bodies,
    } = declaration
    {
        for (i, comparison) in comparisons.iter().enumerate() {
            match comparison {
                IfComparisonType::If(comparison) => {
                    if let RuntimeValue::Bool(x) = evaluate(comparison.clone(), scope.clone())? {
                        if x {
                            return evaluate_scope(
                                NodeType::ScopeDeclaration {
                                    body: bodies[i].to_vec(),
                                },
                                scope,
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

                    let value = evaluate(value.clone(), scope.clone())?;

                    if let Some(result) = match_pattern(
                        &pattern.0,
                        &value,
                        mutability.clone(),
                        path.clone(),
                        scope.clone(),
                        &pattern.1,
                        &bodies[i],
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
                return evaluate_scope(
                    NodeType::ScopeDeclaration {
                        body: last.to_vec(),
                    },
                    scope,
                );
            }
        }

        Ok(RuntimeValue::Null)
    } else {
        Err(InterpreterErr::NotImplemented(declaration))
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
        let result = evaluate_if_statement(node, scope.clone()).unwrap();
        assert_eq!(result, RuntimeValue::Int(123));
    }

    #[test]
    fn test_evaluate_if_statement_false_else() {
        let scope = new_scope();
        let node = NodeType::IfStatement {
            comparisons: vec![NodeType::Identifier(String::from("false"))],
            bodies: vec![vec![NodeType::IntLiteral(1)], vec![NodeType::IntLiteral(2)]],
        };
        let result = evaluate_if_statement(node, scope.clone()).unwrap();
        assert_eq!(result, RuntimeValue::Int(2));
    }
}
