use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::runtime::{
    scope::ScopeErr,
    values::{RuntimeType, RuntimeValue},
};

use super::Scope;

impl Scope {
    pub fn push_struct(
        &mut self,
        key: String,
        value: HashMap<String, RuntimeType>,
    ) -> Result<(), ScopeErr> {
        if !self.structs.contains_key(&key) {
            self.structs.insert(key, value);
        } else {
            return Err(ScopeErr::Struct(key));
        }

        Ok(())
    }

    pub fn push_struct_function(
        &mut self,
        key: String,
        value: (String, RuntimeValue, bool),
    ) -> Result<(), ScopeErr> {
        if let Some(x) = self.structs_functions.get_mut(&key) {
            x.insert(value.0, (value.1, value.2));
        } else if self.structs.contains_key(&key) {
            self.structs_functions
                .insert(key, HashMap::from([(value.0, (value.1, value.2))]));
        } else {
            return Err(ScopeErr::Struct(key));
        }

        Ok(())
    }
}

pub fn get_struct_function(
    this: Rc<RefCell<Scope>>,
    strct: &str,
    func: &str,
) -> Result<(RuntimeValue, bool), ScopeErr> {
    let scope = resolve_struct(this, strct)?;
    if let Some(fns) = scope.borrow().structs_functions.get(strct) {
        if let Some(val) = fns.get(func) {
            Ok(val.clone())
        } else {
            Err(ScopeErr::StructFunction(func.to_string()))
        }
    } else {
        Err(ScopeErr::Struct(strct.to_string()))
    }
}

pub fn resolve_struct_function(
    this: Rc<RefCell<Scope>>,
    og_key: &str,
) -> Result<Rc<RefCell<Scope>>, ScopeErr> {
    if this.borrow().structs.contains_key(og_key) {
        Ok(this)
    } else if let Some(parent) = &this.borrow().parent {
        resolve_struct_function(parent.clone(), og_key)
    } else {
        Err(ScopeErr::StructFunction(og_key.to_string()))
    }
}

pub fn get_struct(
    this: Rc<RefCell<Scope>>,
    key: &str,
) -> Result<HashMap<String, RuntimeType>, ScopeErr> {
    let scope = resolve_struct(this, key)?;
    if let Some(value) = scope.borrow().structs.get(key) {
        Ok(value.clone())
    } else {
        Err(ScopeErr::Struct(key.to_string()))
    }
}

pub fn resolve_struct(
    this: Rc<RefCell<Scope>>,
    og_key: &str,
) -> Result<Rc<RefCell<Scope>>, ScopeErr> {
    if this.borrow().structs.contains_key(og_key) {
        Ok(this)
    } else if let Some(parent) = &this.borrow().parent {
        resolve_struct(parent.clone(), og_key)
    } else {
        Err(ScopeErr::Struct(og_key.to_string()))
    }
}
