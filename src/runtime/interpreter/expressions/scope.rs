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
    // println!("{:?}", arguments);
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
