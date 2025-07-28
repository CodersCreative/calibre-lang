use crate::{
    ast::NodeType,
    runtime::{
        interpreter::InterpreterErr,
        scope::{Scope, ScopeErr, VarType, children::get_next_scope, variables::resolve_var},
        values::{RuntimeValue, helper::ObjectType},
    },
};
use core::panic;
use std::{any::Any, cell::RefCell, rc::Rc};

pub fn progress<'a>(
    mut current: &'a mut RuntimeValue,
    key: &str,
) -> Result<&'a mut RuntimeValue, InterpreterErr> {
    match current {
        RuntimeValue::Struct(ObjectType::Map(map), _) => current = map.get_mut(key).unwrap(),
        RuntimeValue::Enum(_, _, Some(ObjectType::Map(map))) => current = map.get_mut(key).unwrap(),
        RuntimeValue::Result(Err(x), _) if key == "Err" => {
            current = x;
        }
        RuntimeValue::Result(Ok(x), _) if key == "Ok" => {
            current = x;
        }
        RuntimeValue::Option(Some(x), _) if key == "Some" => {
            current = x;
        }
        RuntimeValue::Struct(ObjectType::Tuple(tuple), _) => {
            let idx = key
                .parse::<usize>()
                .map_err(|_| InterpreterErr::IndexNonList(NodeType::Identifier(key.to_string())))?;
            current = tuple.get_mut(idx).unwrap()
        }
        RuntimeValue::Enum(_, _, Some(ObjectType::Tuple(tuple))) => {
            let idx = key
                .parse::<usize>()
                .map_err(|_| InterpreterErr::IndexNonList(NodeType::Identifier(key.to_string())))?;
            current = tuple.get_mut(idx).unwrap()
        }
        RuntimeValue::Tuple(tuple) => {
            let idx = key
                .parse::<usize>()
                .map_err(|_| InterpreterErr::IndexNonList(NodeType::Identifier(key.to_string())))?;
            current = tuple.get_mut(idx).unwrap()
        }
        RuntimeValue::List { data, .. } => {
            let idx = key
                .parse::<usize>()
                .map_err(|_| InterpreterErr::IndexNonList(NodeType::Identifier(key.to_string())))?;
            current = data.get_mut(idx).unwrap()
        }
        _ => {
            panic!()
        }
    }

    Ok(current)
}

pub fn get_link_path(
    this: &Rc<RefCell<Scope>>,
    path: &[String],
) -> Result<RuntimeValue, InterpreterErr> {
    let (env, name) = match resolve_var(this, &path[0])
        .map_err(|e| InterpreterErr::Value(crate::runtime::values::ValueErr::Scope(e)))
    {
        Ok(x) => x,
        Err(e) => {
            let scope = get_next_scope(this, &path[0]).map_err(|_| e)?;
            return get_link_path(&scope, &path[1..]);
        }
    };

    if let (RuntimeValue::Link(x, _), _) = env.borrow().variables.get(&name).unwrap() {
        return get_link_path(&env, x);
    }

    let mut new_env = env.borrow_mut();
    let mut current = match new_env.variables.get_mut(&name) {
        Some(x) => &mut x.0,
        None => panic!(),
    };

    for key in path.iter().skip(1) {
        current = progress(current, key)?;
    }

    Ok(current.clone())
}

pub fn get_link(
    this: &Rc<RefCell<Scope>>,
    link: &RuntimeValue,
) -> Result<RuntimeValue, InterpreterErr> {
    if let RuntimeValue::Link(path, _) = link {
        get_link_path(this, &path)
    } else {
        panic!()
    }
}

pub fn update_link<F>(
    this: &Rc<RefCell<Scope>>,
    link: &RuntimeValue,
    f: F,
) -> Result<(), InterpreterErr>
where
    F: FnMut(&mut RuntimeValue) -> Result<(), InterpreterErr>,
{
    if let RuntimeValue::Link(path, _) = link {
        update_link_path(this, &path, f)
    } else {
        panic!()
    }
}

pub fn update_link_path<F>(
    this: &Rc<RefCell<Scope>>,
    path: &[String],
    mut f: F,
) -> Result<(), InterpreterErr>
where
    F: FnMut(&mut RuntimeValue) -> Result<(), InterpreterErr>,
{
    let (env, name) = resolve_var(this, &path[0])
        .map_err(|e| InterpreterErr::Value(crate::runtime::values::ValueErr::Scope(e)))?;

    if let (RuntimeValue::Link(x, _), _) = env.borrow().variables.get(&name).unwrap() {
        return update_link_path(&env, x, f);
    }

    let mut new_env = env.borrow_mut();
    let mut current = match new_env.variables.get_mut(&name) {
        Some(x) => &mut x.0,
        None => panic!(),
    };

    for key in path.iter().skip(1) {
        current = progress(current, key)?;
    }

    f(current)
}

pub fn get_link_parent(
    this: &Rc<RefCell<Scope>>,
    link: &RuntimeValue,
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
