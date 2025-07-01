use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::NodeType,
    runtime::{
        interpreter::{InterpreterErr, evaluate},
        scope::Scope,
        values::{RuntimeValue, helper::StopValue},
    },
};

pub fn evaluate_in_statement(
    declaration: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::InDeclaration { identifier, expression } = declaration
    {
        let value = evaluate(*expression, scope.clone())?;
        let ident = evaluate(*identifier, scope.clone())?;

        match value{
            RuntimeValue::Str(x) => match ident{
                RuntimeValue::Char(y) => return Ok(RuntimeValue::Bool(x.contains(y))), 
                RuntimeValue::Str(y) => return Ok(RuntimeValue::Bool(x.contains(&y))), 
                _ => {},
            }
            RuntimeValue::List { data, data_type } => {
                return Ok(RuntimeValue::Bool(data.contains(&ident))); 
            }
            RuntimeValue::Tuple(data) => {
                return Ok(RuntimeValue::Bool(data.contains(&ident))); 
            }
            RuntimeValue::Range(from, to) => {
                let num : f32 = match ident {
                    RuntimeValue::Range(x, y) => (x as f32 + y as f32) / 2.0,
                    RuntimeValue::Integer(x) => x as f32,
                    RuntimeValue::Float(x) => x as f32,
                    _ => return Ok(RuntimeValue::Bool(false)),
                };
                return Ok(RuntimeValue::Bool(num >= from as f32 && num < to as f32)); 
            }
            _ => {}
        }

        Ok(RuntimeValue::Bool(false))
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
            if let RuntimeValue::Bool(x) = evaluate(comparison.clone(), scope.clone())? {
                if x {
                    let mut result: RuntimeValue = RuntimeValue::Null;
                    for statement in bodies[i].iter() {
                        if let NodeType::Return { value } = statement {
                            scope.borrow_mut().stop = Some(StopValue::Return);
                            return Ok(evaluate(*value.clone(), scope.clone())?);
                        } else if let NodeType::Break = statement {
                            if scope.borrow().stop != Some(StopValue::Return) {
                                scope.borrow_mut().stop = Some(StopValue::Break);
                            }
                            return Ok(result);
                        } else if let NodeType::Continue = statement {
                            if scope.borrow().stop == None {
                                scope.borrow_mut().stop = Some(StopValue::Continue);
                            }
                            return Ok(result);
                        }

                        result = evaluate(statement.clone(), scope.clone())?;
                    }

                    return Ok(result);
                }
            } else {
                return Err(InterpreterErr::ExpectedOperation(String::from("boolean")));
            }
        }

        if comparisons.len() < bodies.len() {
            if let Some(last) = bodies.last() {
                let mut result: RuntimeValue = RuntimeValue::Null;
                for statement in last.iter() {
                    if let NodeType::Return { value } = statement {
                        scope.borrow_mut().stop = Some(StopValue::Return);
                        return Ok(evaluate(*value.clone(), scope.clone())?);
                    } else if let NodeType::Break = statement {
                        if scope.borrow().stop != Some(StopValue::Return) {
                            scope.borrow_mut().stop = Some(StopValue::Break);
                        }
                        return Ok(result);
                    } else if let NodeType::Continue = statement {
                        if scope.borrow().stop == None {
                            scope.borrow_mut().stop = Some(StopValue::Continue);
                        }
                        return Ok(result);
                    }
                    result = evaluate(statement.clone(), scope.clone())?;
                }
                return Ok(result);
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
            bodies: vec![vec![NodeType::IntegerLiteral(123)]],
        };
        let result = evaluate_if_statement(node, scope.clone()).unwrap();
        assert_eq!(result, RuntimeValue::Integer(123));
    }

    #[test]
    fn test_evaluate_if_statement_false_else() {
        let scope = new_scope();
        let node = NodeType::IfStatement {
            comparisons: vec![NodeType::Identifier(String::from("false"))],
            bodies: vec![
                vec![NodeType::IntegerLiteral(1)],
                vec![NodeType::IntegerLiteral(2)],
            ],
        };
        let result = evaluate_if_statement(node, scope.clone()).unwrap();
        assert_eq!(result, RuntimeValue::Integer(2));
    }
}
