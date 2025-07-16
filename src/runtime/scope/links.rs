use crate::{
    ast::NodeType,
    runtime::{
        interpreter::InterpreterErr,
        scope::{Scope, ScopeErr, VarType, variables::resolve_var},
        values::{RuntimeValue, helper::ObjectType},
    },
};
use core::panic;
use std::{cell::RefCell, rc::Rc};

pub fn get_link<F>(
    this: Rc<RefCell<Scope>>,
    link: RuntimeValue,
) -> Result<RuntimeValue, InterpreterErr> {
    if let RuntimeValue::Link(path, _) = link {
        let (env, name) = resolve_var(this, &path[0])
            .map_err(|e| InterpreterErr::Value(crate::runtime::values::ValueErr::Scope(e)))?;
        let mut new_env = env.borrow_mut();
        let mut current = match new_env.variables.get_mut(&name) {
            Some(x) => &mut x.0,
            None => panic!(),
        };

        for key in path.iter().skip(1) {
            match current {
                RuntimeValue::Struct(ObjectType::Map(map), _) => {
                    current = map.get_mut(key).unwrap()
                }
                RuntimeValue::Struct(ObjectType::Tuple(tuple), _) => {
                    let idx = key.parse::<usize>().map_err(|_| {
                        InterpreterErr::IndexNonList(NodeType::Identifier(key.clone()))
                    })?;
                    current = tuple.get_mut(idx).unwrap()
                }
                RuntimeValue::Tuple(tuple) => {
                    let idx = key.parse::<usize>().map_err(|_| {
                        InterpreterErr::IndexNonList(NodeType::Identifier(key.clone()))
                    })?;
                    current = tuple.get_mut(idx).unwrap()
                }
                RuntimeValue::List { data, .. } => {
                    let idx = key.parse::<usize>().map_err(|_| {
                        InterpreterErr::IndexNonList(NodeType::Identifier(key.clone()))
                    })?;
                    current = data.get_mut(idx).unwrap()
                }
                _ => {
                    panic!()
                }
            }
        }

        Ok(current.clone())
    } else {
        panic!()
    }
}

pub fn update_link<F>(
    this: Rc<RefCell<Scope>>,
    link: RuntimeValue,
    mut f: F,
) -> Result<(), InterpreterErr>
where
    F: FnMut(&mut RuntimeValue) -> Result<(), InterpreterErr>,
{
    if let RuntimeValue::Link(path, _) = link {
        let (env, name) = resolve_var(this, &path[0])
            .map_err(|e| InterpreterErr::Value(crate::runtime::values::ValueErr::Scope(e)))?;
        let mut new_env = env.borrow_mut();
        let mut current = match new_env.variables.get_mut(&name) {
            Some(x) => &mut x.0,
            None => panic!(),
        };

        for key in path.iter().skip(1) {
            match current {
                RuntimeValue::Struct(ObjectType::Map(map), _) => {
                    current = map.get_mut(key).unwrap()
                }
                RuntimeValue::Struct(ObjectType::Tuple(tuple), _) => {
                    let idx = key.parse::<usize>().map_err(|_| {
                        InterpreterErr::IndexNonList(NodeType::Identifier(key.clone()))
                    })?;
                    current = tuple.get_mut(idx).unwrap()
                }
                RuntimeValue::Tuple(tuple) => {
                    let idx = key.parse::<usize>().map_err(|_| {
                        InterpreterErr::IndexNonList(NodeType::Identifier(key.clone()))
                    })?;
                    current = tuple.get_mut(idx).unwrap()
                }
                RuntimeValue::List { data, .. } => {
                    let idx = key.parse::<usize>().map_err(|_| {
                        InterpreterErr::IndexNonList(NodeType::Identifier(key.clone()))
                    })?;
                    current = data.get_mut(idx).unwrap()
                }
                _ => {
                    panic!()
                }
            }
        }

        f(current)
    } else {
        panic!()
    }
}

pub fn get_link_parent(
    this: Rc<RefCell<Scope>>,
    link: RuntimeValue,
) -> Result<(RuntimeValue, VarType), ScopeErr> {
    if let RuntimeValue::Link(path, _) = link {
        let scope = resolve_var(this, &path[0])?;
        Ok(
            if let Some(value) = scope.0.borrow().variables.get(&scope.1) {
                value.clone()
            } else {
                return Err(ScopeErr::Variable(path[0].to_string()));
            },
        )
    } else {
        panic!()
    }
}
