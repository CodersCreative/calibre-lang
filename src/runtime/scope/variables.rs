use std::{cell::RefCell, mem::discriminant, rc::Rc};

use thiserror::Error;

use crate::runtime::{scope::ScopeErr, values::RuntimeValue};

use super::Scope;

impl Scope {
    pub fn push_var(
        &mut self,
        key: String,
        value: &RuntimeValue,
        is_mutable: bool,
    ) -> Result<(), ScopeErr> {
        if !self.constants.contains_key(&key) {
            if is_mutable {
                self.variables.insert(key, value.clone());
            } else {
                self.constants.insert(key, value.clone());
            }
        } else {
            return Err(ScopeErr::AssignConstant(key));
        }

        Ok(())
    }

    fn resolve_alias<'a>(&'a self, original: &'a str) -> &'a str {
        if let Some(x) = self.alias.get(original) {
            x
        } else {
            original
        }
    }

    pub fn assign_var(&mut self, og_key: String, value: &RuntimeValue) -> Result<(), ScopeErr> {
        let key = self.resolve_alias(&og_key).to_string();

        if og_key == key {
            if let Some(v) = self.variables.get_mut(&key) {
                if discriminant(v) == discriminant(value) || v.is_number() && value.is_number() {
                    *v = value.clone();
                    return Ok(());
                } else {
                    return Err(ScopeErr::TypeMismatch(v.clone(), value.clone()));
                }
            } else if self.constants.contains_key(&key) {
                return Err(ScopeErr::AssignConstant(key));
            }
        }

        if let Some(parent) = &self.parent {
            parent.borrow_mut().assign_var(key.to_string(), value);
        } else {
            return Err(ScopeErr::Variable(key.to_string()));
        }

        Ok(())
    }
}

pub fn get_var(this: Rc<RefCell<Scope>>, key: &str) -> Result<RuntimeValue, ScopeErr> {
    let scope = resolve_var(this, key)?;
    Ok(
        if let Some(value) = scope.0.borrow().variables.get(&scope.1) {
            value.clone()
        } else if let Some(value) = scope.0.borrow().constants.get(&scope.1) {
            value.clone()
        } else {
            return Err(ScopeErr::Variable(key.to_string()));
        },
    )
}

pub fn resolve_var(
    this: Rc<RefCell<Scope>>,
    og_key: &str,
) -> Result<(Rc<RefCell<Scope>>, String), ScopeErr> {
    let key = this.borrow().resolve_alias(og_key).to_string();

    if this.borrow().variables.contains_key(&key) || this.borrow().constants.contains_key(&key) {
        return Ok((this, key));
    } else if let Some(parent) = &this.borrow().parent {
        return resolve_var(parent.clone(), &key);
    } else {
        Err(ScopeErr::Variable(key))
    }
}
