use std::{cell::RefCell, mem::discriminant, rc::Rc};

use crate::runtime::{
    scope::{ScopeErr, VarType},
    values::RuntimeValue,
};

use super::Scope;

impl Scope {
    pub fn push_var(
        &mut self,
        key: String,
        value: RuntimeValue,
        var_type: VarType,
    ) -> Result<(), ScopeErr> {
        if let Some(var) = self.variables.get(&key) {
            if var.1 == VarType::Constant {
                return Err(ScopeErr::AssignConstant(key));
            }
        }

        self.variables.insert(key, (value.clone(), var_type));

        Ok(())
    }

    fn resolve_alias<'a>(&'a self, original: &'a str) -> &'a str {
        if let Some(x) = self.variables.get(original) {
            match &x.1 {
                VarType::Mutable(Some(x)) => x,
                VarType::Immutable(Some(x)) => x,
                _ => original,
            }
        } else {
            original
        }
    }

    pub fn update_var<F>(&mut self, og_key: &str, mut f: F) -> Result<(), ScopeErr>
    where
        F: FnMut(&mut (RuntimeValue, VarType)),
    {
        let key = self.resolve_alias(&og_key).to_string();

        if key == key {
            if let Some(v) = self.variables.get_mut(&key) {
                f(v);
            }
        }

        if let Some(parent) = &self.parent {
            let _ = parent.borrow_mut().update_var(og_key, f)?;
        } else {
            return Err(ScopeErr::Variable(key.to_string()));
        }

        Ok(())
    }

    pub fn assign_var(&mut self, og_key: &str, value: RuntimeValue) -> Result<(), ScopeErr> {
        let key = self.resolve_alias(&og_key).to_string();

        if og_key == key {
            if let Some(v) = self.variables.get_mut(&key) {
                match v.1 {
                    VarType::Mutable(_) => {}
                    _ => return Err(ScopeErr::AssignConstant(key)),
                }
                if discriminant(&v.0) == discriminant(&value)
                    || v.0.is_number() && value.is_number()
                {
                    *v = (value, VarType::Mutable(None));
                    return Ok(());
                } else {
                    return Err(ScopeErr::TypeMismatch(v.0.clone(), value.clone()));
                }
            }
        }

        if let Some(parent) = &self.parent {
            let _ = parent.borrow_mut().assign_var(&key, value)?;
        } else {
            return Err(ScopeErr::Variable(key.to_string()));
        }

        Ok(())
    }
}

pub fn get_var(this: Rc<RefCell<Scope>>, key: &str) -> Result<(RuntimeValue, VarType), ScopeErr> {
    let scope = resolve_var(this, key)?;
    Ok(
        if let Some(value) = scope.0.borrow().variables.get(&scope.1) {
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

    if this.borrow().variables.contains_key(&key) {
        return Ok((this, key));
    } else if let Some(parent) = &this.borrow().parent {
        return resolve_var(parent.clone(), &key);
    } else {
        Err(ScopeErr::Variable(key))
    }
}
