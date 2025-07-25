use std::{cell::RefCell, mem::discriminant, rc::Rc};
use crate::runtime::{
    interpreter::InterpreterErr,
    scope::{links::update_link, variables::get_global_scope, ScopeErr, VarType},
    values::{helper::StopValue, RuntimeType, RuntimeValue},
};

use super::Scope;

impl Scope {
    pub fn push_child(
        &mut self,
        key: String,
        value: Rc<RefCell<Self>>,
    ) -> Result<(), ScopeErr> {
        if !self.children.contains_key(&key){
            if let Some(parent) = &self.parent{
                if let Ok(x) = get_scope(&parent, &key){
                    self.children.insert(key.clone(), x);
                    return Ok(())
                }
            }

            self.children.insert(key, value);
        }


        Ok(())
    }
}

pub fn get_scope(this: &Rc<RefCell<Scope>>, key: &str) -> Result<Rc<RefCell<Scope>>, ScopeErr> {
    if let Some(scope) = this.borrow().children.get(key) {
        return Ok(scope.clone());
    } else if let Some(parent) = &this.borrow().parent {
        return get_scope(&parent, &key);
    } else {
        Err(ScopeErr::Variable(key.to_string()))
    }
}
