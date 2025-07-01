use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{NodeType, RefMutability},
    runtime::{
        interpreter::{InterpreterErr, evaluate},
        scope::{Scope, variables::resolve_var},
        values::{
            RuntimeType, RuntimeValue,
            helper::{StopValue, VarType},
        },
    },
};

pub fn get_new_scope(
    scope: Rc<RefCell<Scope>>,
    parameters: Vec<(String, RuntimeType, RefMutability, Option<RuntimeValue>)>,
    arguments: Vec<(NodeType, Option<NodeType>)>,
) -> Result<Rc<RefCell<Scope>>, InterpreterErr> {
    let new_scope = Rc::new(RefCell::new(Scope::new(Some(scope.clone()))));

    for (i, (k, v, m, d)) in parameters.iter().enumerate() {
        if m == &RefMutability::MutRef || m == &RefMutability::Ref {
            if let Some(arg) = &arguments.get(i) {
                if let None = arg.1 {
                    if let NodeType::Identifier(x) = &arg.0 {
                        let (env, name) = resolve_var(new_scope.clone(), x)?;

                        let (var, var_type) = env.borrow().variables.get(&name).unwrap().clone();

                        match var_type {
                            VarType::Mutable(_) => {}
                            _ if m == &RefMutability::MutRef => {
                                return Err(InterpreterErr::MutRefNonMut(var));
                            }
                            _ => {}
                        }
                        let x = name.clone();
                        if var.is_type(scope.clone(), v.clone()) {
                            let _ = new_scope.borrow_mut().push_var(
                                k.to_string(),
                                RuntimeValue::Null,
                                match m {
                                    RefMutability::MutRef | RefMutability::MutValue => {
                                        VarType::Mutable(Some(x))
                                    }
                                    _ => VarType::Immutable(Some(x)),
                                },
                            )?;
                        } else {
                            return Err(InterpreterErr::UnexpectedType(var));
                        }

                        continue;
                    } else {
                        return Err(InterpreterErr::RefNonVar(arguments[0].0.clone()));
                    }
                }
            }
        }
        if let Some(arg) = arguments.get(i) {
            if let None = arg.1 {
                let arg = evaluate(arg.0.clone(), new_scope.clone())?
                    .into_type(new_scope.clone(), v.clone())?;
                new_scope.borrow_mut().push_var(
                    k.to_string(),
                    arg,
                    match m {
                        RefMutability::MutRef | RefMutability::MutValue => VarType::Mutable(None),
                        _ => VarType::Immutable(None),
                    },
                )?;
                continue;
            }
        }
        if let Some(d) = arguments.iter().find(|x| {
            if let NodeType::Identifier(key) = &x.0 {
                key == k && x.1.is_some()
            } else {
                false
            }
        }) {
            let _ = new_scope.borrow_mut().push_var(
                k.to_string(),
                evaluate(d.1.clone().unwrap(), scope.clone())?,
                match m {
                    RefMutability::MutRef | RefMutability::MutValue => VarType::Mutable(None),
                    _ => VarType::Immutable(None),
                },
            )?;
            continue;
        }

        if let Some(d) = d {
            let _ = new_scope.borrow_mut().push_var(
                k.to_string(),
                d.clone(),
                match m {
                    RefMutability::MutRef | RefMutability::MutValue => VarType::Mutable(None),
                    _ => VarType::Immutable(None),
                },
            )?;
            continue;
        }

        return Err(InterpreterErr::RefNonVar(arguments[0].0.clone()));
    }

    Ok(new_scope)
}

pub fn evaluate_scope(
    node: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::ScopeDeclaration { body } = node {
        let new_scope = get_new_scope(scope, Vec::new(), Vec::new())?;

        let mut result: RuntimeValue = RuntimeValue::Null;
        for statement in body.into_iter() {
            if let Some(StopValue::Return) = new_scope.borrow().stop {
                break;
            } else if let NodeType::Return { value } = statement {
                result = evaluate(*value.clone(), new_scope.clone())?;
                new_scope.borrow_mut().stop = Some(StopValue::Return);
                break;
            } else {
                result = evaluate(statement, new_scope.clone())?;
            }
        }

        Ok(result)
    } else {
        panic!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::scope::Scope;
    use crate::runtime::values::{RuntimeValue, RuntimeType, helper::{VarType}};
    use crate::ast::{NodeType, RefMutability};
    use std::cell::RefCell;
    use std::rc::Rc;

    fn new_scope() -> Rc<RefCell<Scope>> {
        Rc::new(RefCell::new(Scope::new(None)))
    }

    #[test]
    fn test_get_new_scope_basic_value() {
        let parent_scope = new_scope();
        let params = vec![
            ("x".to_string(), RuntimeType::Integer, RefMutability::Value, None)
        ];
        let args = vec![(NodeType::IntegerLiteral(5), None)];
        let scope = get_new_scope(parent_scope.clone(), params, args).unwrap();
        let val = scope.borrow().variables.get("x").unwrap().0.clone();
        assert_eq!(val, RuntimeValue::Integer(5));
    }

    #[test]
    fn test_get_new_scope_default_value() {
        let parent_scope = new_scope();
        let params = vec![
            ("y".to_string(), RuntimeType::Integer, RefMutability::Value, Some(RuntimeValue::Integer(10)))
        ];
        let args = vec![];
        let scope = get_new_scope(parent_scope.clone(), params, args).unwrap();
        let val = scope.borrow().variables.get("y").unwrap().0.clone();
        assert_eq!(val, RuntimeValue::Integer(10));
    }

    #[test]
    fn test_get_new_scope_named_argument() {
        let parent_scope = new_scope();
        let params = vec![
            ("z".to_string(), RuntimeType::Integer, RefMutability::Value, Some(RuntimeValue::Integer(1)))
        ];
        let args = vec![(NodeType::Identifier("z".to_string()), Some(NodeType::IntegerLiteral(99)))];
        let scope = get_new_scope(parent_scope.clone(), params, args).unwrap();
        let val = scope.borrow().variables.get("z").unwrap().0.clone();
        assert_eq!(val, RuntimeValue::Integer(99));
    }

    #[test]
    fn test_get_new_scope_mut_ref_error() {
        let parent_scope = new_scope();
        let params = vec![
            ("a".to_string(), RuntimeType::Integer, RefMutability::MutRef, None)
        ];
        let args = vec![(NodeType::IntegerLiteral(5), None)];
        let result = get_new_scope(parent_scope.clone(), params, args);
        assert!(result.is_err());
    }

    #[test]
    fn test_evaluate_scope_return() {
        let parent_scope = new_scope();
        let node = NodeType::ScopeDeclaration {
            body: vec![
                NodeType::Return { value: Box::new(NodeType::IntegerLiteral(42)) }
            ]
        };
        let result = evaluate_scope(node, parent_scope).unwrap();
        assert_eq!(result, RuntimeValue::Integer(42));
    }

    #[test]
    fn test_evaluate_scope_no_return() {
        let parent_scope = new_scope();
        let node = NodeType::ScopeDeclaration {
            body: vec![
                NodeType::IntegerLiteral(7)
            ]
        };
        let result = evaluate_scope(node, parent_scope).unwrap();
        assert_eq!(result, RuntimeValue::Integer(7));
    }
}
