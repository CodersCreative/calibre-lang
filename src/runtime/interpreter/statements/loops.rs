use std::{cell::RefCell, rc::Rc};

use rand::seq::IndexedRandom;

use crate::{
    ast::{LoopType, NodeType, RefMutability},
    runtime::{
        interpreter::{InterpreterErr, evaluate, expressions::scope::get_new_scope},
        scope::{
            Scope,
            variables::{assign_var, get_var},
        },
        values::{
            RuntimeType, RuntimeValue,
            helper::{StopValue, VarType},
        },
    },
};

pub fn evaluate_loop_declaration(
    declaration: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::LoopDeclaration { loop_type, body } = declaration {
        let handle_body = |new_scope: Rc<RefCell<Scope>>| -> Result<RuntimeValue, InterpreterErr> {
            let mut result = RuntimeValue::Null;
            for statement in body.iter() {
                match new_scope.borrow().stop {
                    Some(StopValue::Return) => break,
                    Some(StopValue::Break) => break,
                    Some(StopValue::Continue) => {
                        new_scope.borrow_mut().stop = None;
                        break;
                    }
                    _ => {}
                };
                if let NodeType::Return { value } = statement {
                    new_scope.borrow_mut().stop = Some(StopValue::Return);
                    result = evaluate(*value.clone(), new_scope.clone())?;
                    break;
                } else if let NodeType::Break = statement {
                    if scope.borrow_mut().stop != Some(StopValue::Return) {
                        new_scope.borrow_mut().stop = Some(StopValue::Break);
                    }
                    break;
                } else if let NodeType::Continue = statement {
                    break;
                } else {
                    result = evaluate(statement.clone(), new_scope.clone())?;
                }
            }

            Ok(result)
        };

        let mut result = RuntimeValue::Null;

        if let LoopType::For(identifier, range) = *loop_type {
            let range = evaluate(range, scope.clone())?;
            if let RuntimeValue::List { data, data_type: _ } = range {
                for d in data.into_iter() {
                    let new_scope = get_new_scope(scope.clone(), Vec::new(), Vec::new())?;
                    let _ = new_scope.borrow_mut().push_var(
                        identifier.clone(),
                        d.clone(),
                        VarType::Immutable(None),
                    );
                    result = handle_body(new_scope.clone())?;

                    match new_scope.borrow().stop {
                        Some(StopValue::Break) => break,
                        Some(StopValue::Return) => {
                            scope.borrow_mut().stop = Some(StopValue::Return);
                            break;
                        }
                        _ => {}
                    };
                }
            } else if let RuntimeValue::Range(from, to) =
                range.into_type(scope.clone(), RuntimeType::Range)?
            {
                for i in from..to {
                    let new_scope = get_new_scope(
                        scope.clone(),
                        vec![(
                            identifier.clone(),
                            RuntimeType::Int,
                            RefMutability::Value,
                            None,
                        )],
                        vec![(NodeType::IntLiteral(i as i128), None)],
                    )?;
                    result = handle_body(new_scope.clone())?;

                    match new_scope.borrow().stop {
                        Some(StopValue::Break) => break,
                        Some(StopValue::Return) => {
                            scope.borrow_mut().stop = Some(StopValue::Return);
                            break;
                        }
                        _ => {}
                    };
                }
            }
        } else if let LoopType::ForEach(identifier, (loop_name, mutability)) = *loop_type {
            let (var, _) = get_var(scope.clone(), &loop_name)?;
            if let RuntimeValue::List {
                mut data,
                data_type,
            } = var
            {
                for (i, d) in data.iter().enumerate() {
                    let new_scope = get_new_scope(scope.clone(), Vec::new(), Vec::new())?;

                    let _ = new_scope.borrow_mut().push_var(
                        identifier.clone(),
                        RuntimeValue::Link(vec![loop_name.clone(), i.to_string()], d.into()),
                        match mutability {
                            RefMutability::MutRef | RefMutability::MutValue => {
                                VarType::Mutable(None)
                            }
                            _ => VarType::Immutable(None),
                        },
                    );

                    result = handle_body(new_scope.clone())?;

                    match new_scope.borrow().stop {
                        Some(StopValue::Break) => break,
                        Some(StopValue::Return) => {
                            scope.borrow_mut().stop = Some(StopValue::Return);
                            break;
                        }
                        _ => {}
                    };
                }
            }
        } else if let LoopType::While(condition) = *loop_type {
            match evaluate(condition.clone(), scope.clone())? {
                RuntimeValue::Bool(_) => {
                    while let RuntimeValue::Bool(x) = evaluate(condition.clone(), scope.clone())? {
                        if !x {
                            break;
                        }
                        let new_scope = get_new_scope(scope.clone(), Vec::new(), Vec::new())?;
                        result = handle_body(new_scope.clone())?;

                        match new_scope.borrow().stop {
                            Some(StopValue::Break) => break,
                            Some(StopValue::Return) => {
                                scope.borrow_mut().stop = Some(StopValue::Return);
                                break;
                            }
                            _ => {}
                        };
                    }
                }
                RuntimeValue::Range(from, to) => {
                    return evaluate_loop_declaration(
                        NodeType::LoopDeclaration {
                            loop_type: Box::new(LoopType::For(
                                String::from("hidden_index"),
                                NodeType::RangeDeclaration {
                                    from: Box::new(NodeType::IntLiteral(from as i128)),
                                    to: Box::new(NodeType::IntLiteral(to as i128)),
                                    inclusive: false,
                                },
                            )),
                            body,
                        },
                        scope,
                    );
                }
                RuntimeValue::Int(x) => {
                    return evaluate_loop_declaration(
                        NodeType::LoopDeclaration {
                            loop_type: Box::new(LoopType::For(
                                String::from("hidden_index"),
                                NodeType::IntLiteral(x as i128),
                            )),
                            body,
                        },
                        scope,
                    );
                }
                RuntimeValue::Float(x) => {
                    return evaluate_loop_declaration(
                        NodeType::LoopDeclaration {
                            loop_type: Box::new(LoopType::For(
                                String::from("hidden_index"),
                                NodeType::FloatLiteral(x as f64),
                            )),
                            body,
                        },
                        scope,
                    );
                }
                _ => {}
            }
        }

        Ok(result)
    } else {
        Err(InterpreterErr::NotImplemented(declaration))
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};

    use crate::{
        ast::{LoopType, NodeType, RefMutability},
        runtime::{
            interpreter::statements::loops::evaluate_loop_declaration,
            scope::Scope,
            values::{RuntimeType, RuntimeValue, helper::VarType},
        },
    };

    fn new_scope() -> Rc<RefCell<Scope>> {
        Rc::new(RefCell::new(Scope::new(None)))
    }

    #[test]
    fn test_evaluate_loop_declaration_for_range() {
        let scope = new_scope();
        let loop_type = Box::new(LoopType::For(
            "i".to_string(),
            NodeType::RangeDeclaration {
                from: Box::new(NodeType::IntLiteral(0)),
                to: Box::new(NodeType::IntLiteral(3)),
                inclusive: false,
            },
        ));
        let body = vec![NodeType::VariableDeclaration {
            var_type: VarType::Mutable(None),
            identifier: "x".to_string(),
            value: Some(Box::new(NodeType::Identifier("i".to_string()))),
            data_type: None,
        }];
        let node = NodeType::LoopDeclaration { loop_type, body };
        let result = evaluate_loop_declaration(node, scope.clone()).unwrap();
        assert_eq!(result, RuntimeValue::Int(2));
    }
}
