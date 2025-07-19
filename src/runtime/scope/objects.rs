use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::runtime::{
    scope::{Object, ScopeErr},
    values::RuntimeValue,
};

use super::Scope;

impl Scope {
    pub fn push_object(&mut self, key: String, value: Object) -> Result<(), ScopeErr> {
        if !self.objects.contains_key(&key) {
            self.objects.insert(key, value);
        } else {
            return Err(ScopeErr::Object(key));
        }

        Ok(())
    }

    pub fn push_function(
        &mut self,
        key: String,
        value: (String, RuntimeValue, bool),
    ) -> Result<(), ScopeErr> {
        if let Some(x) = self.functions.get_mut(&key) {
            x.insert(value.0, (value.1, value.2));
        } else if self.objects.contains_key(&key) {
            self.functions
                .insert(key, HashMap::from([(value.0, (value.1, value.2))]));
        } else {
            return Err(ScopeErr::Object(key));
        }

        Ok(())
    }
}

pub fn get_function(
    this: &Rc<RefCell<Scope>>,
    strct: &str,
    func: &str,
) -> Result<(RuntimeValue, bool), ScopeErr> {
    let scope = resolve_object(this, strct)?;
    if let Some(fns) = scope.borrow().functions.get(strct) {
        if let Some(val) = fns.get(func) {
            Ok(val.clone())
        } else {
            Err(ScopeErr::Function(func.to_string()))
        }
    } else {
        Err(ScopeErr::Object(strct.to_string()))
    }
}

pub fn resolve_function(
    this: &Rc<RefCell<Scope>>,
    og_key: &str,
) -> Result<Rc<RefCell<Scope>>, ScopeErr> {
    if this.borrow().objects.contains_key(og_key) {
        Ok(this.clone())
    } else if let Some(parent) = &this.borrow().parent {
        resolve_function(&parent, og_key)
    } else {
        Err(ScopeErr::Function(og_key.to_string()))
    }
}

pub fn get_object(this: &Rc<RefCell<Scope>>, key: &str) -> Result<Object, ScopeErr> {
    let scope = resolve_object(this, key)?;
    if let Some(value) = scope.borrow().objects.get(key) {
        Ok(value.clone())
    } else {
        Err(ScopeErr::Object(key.to_string()))
    }
}

pub fn resolve_object(
    this: &Rc<RefCell<Scope>>,
    og_key: &str,
) -> Result<Rc<RefCell<Scope>>, ScopeErr> {
    if this.borrow().objects.contains_key(og_key) {
        Ok(this.clone())
    } else if let Some(parent) = &this.borrow().parent {
        resolve_object(&parent, og_key)
    } else {
        Err(ScopeErr::Object(og_key.to_string()))
    }
}
