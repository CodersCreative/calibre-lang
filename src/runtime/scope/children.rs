use crate::{
    parser::{self, Parser},
    runtime::{
        interpreter::{InterpreterErr, evaluate},
        scope::{ScopeErr, VarType, links::update_link, variables::get_global_scope},
        values::{RuntimeType, RuntimeValue, helper::StopValue},
    },
};
use std::{cell::RefCell, fs, mem::discriminant, rc::Rc};

use super::Scope;

impl Scope {
    pub fn push_child(&mut self, key: String, value: Rc<RefCell<Self>>) -> Result<(), ScopeErr> {
        if !self.children.contains_key(&key) {
            self.children.insert(key, value);
        }

        Ok(())
    }
}

pub fn import_scope_list(
    this: &Rc<RefCell<Scope>>,
    mut list: Vec<String>,
) -> Result<Rc<RefCell<Scope>>, InterpreterErr> {
    if list.len() <= 0 {
        return Ok(this.clone());
    }
    let first = list.remove(0);

    import_scope_list(&import_next_scope(this, first.as_str())?, list)
}

pub fn import_next_scope(
    this: &Rc<RefCell<Scope>>,
    key: &str,
) -> Result<Rc<RefCell<Scope>>, InterpreterErr> {
    Ok(match key {
        "root" => get_global_scope(this),
        "super" => this.borrow().parent.clone().unwrap(),
        _ => {
            if let Some(x) = this.borrow().children.get(key) {
                x.clone().clone()
            } else {
                let scope = Scope::new_from_parent(this.clone(), key.to_string());
                let mut parser = parser::Parser::default();
                let program = parser
                    .produce_ast(fs::read_to_string(scope.borrow().path.clone()).unwrap())
                    .unwrap();
                let _ = evaluate(program, &scope)?;
                scope
            }
        }
    })
}

pub fn get_scope_list(
    this: &Rc<RefCell<Scope>>,
    mut list: Vec<String>,
) -> Result<Rc<RefCell<Scope>>, ScopeErr> {
    if list.len() <= 0 {
        return Ok(this.clone());
    }
    let first = list.remove(0);
    get_scope_list(&get_next_scope(this, first.as_str())?, list)
}

pub fn get_next_scope(
    this: &Rc<RefCell<Scope>>,
    key: &str,
) -> Result<Rc<RefCell<Scope>>, ScopeErr> {
    Ok(match key {
        "root" => get_global_scope(this),
        "super" => this.borrow().parent.clone().unwrap(),
        _ => match this.borrow().children.get(key) {
            Some(x) => x.clone(),
            _ => return Err(ScopeErr::Scope(key.to_string())), // _ => Scope::new_from_parent(this.clone(), key.to_string()),
        },
    })
}
