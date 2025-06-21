use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::runtime::{
    scope::ScopeErr,
    values::RuntimeType,
};

use super::Scope;

impl Scope {
    pub fn push_enum(
        &mut self,
        key: String,
        value: Vec<(String, Option<HashMap<String, RuntimeType>>)>,
    ) -> Result<(), ScopeErr> {
        if !self.enums.contains_key(&key) {
            self.enums.insert(key, value);
        } else {
            return Err(ScopeErr::Struct(key));
        }

        Ok(())
    }
}

pub fn get_enum(
    this: Rc<RefCell<Scope>>,
    key: &str,
) -> Result<Vec<(String, Option<HashMap<String, RuntimeType>>)>, ScopeErr> {
    let scope = resolve_enum(this, key)?;
    if let Some(value) = scope.borrow().enums.get(key) {
        Ok(value.clone())
    } else {
        Err(ScopeErr::Struct(key.to_string()))
    }
}

pub fn resolve_enum(
    this: Rc<RefCell<Scope>>,
    og_key: &str,
) -> Result<Rc<RefCell<Scope>>, ScopeErr> {
    if this.borrow().enums.contains_key(og_key) {
        Ok(this)
    } else if let Some(parent) = &this.borrow().parent {
        resolve_enum(parent.clone(), og_key)
    } else {
        Err(ScopeErr::Struct(og_key.to_string()))
    }
}
