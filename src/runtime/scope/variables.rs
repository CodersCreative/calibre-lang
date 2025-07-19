use std::{cell::RefCell, mem::discriminant, rc::Rc};

use crate::runtime::{
    interpreter::InterpreterErr,
    scope::{ScopeErr, VarType, links::update_link},
    values::{RuntimeType, RuntimeValue, helper::StopValue},
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
}

pub fn assign_var(
    this: &Rc<RefCell<Scope>>,
    og_key: &str,
    value: RuntimeValue,
) -> Result<(), InterpreterErr> {
    let key = this.borrow().resolve_alias(&og_key).to_string();

    if og_key == key {
        if let Some(v) = this.borrow_mut().variables.get_mut(&key) {
            match v.1 {
                VarType::Mutable(_) => {}
                _ => return Err(ScopeErr::AssignConstant(key).into()),
            }

            if let RuntimeValue::Link(_, _) = v.0 {
            } else {
                if discriminant(&v.0) == discriminant(&value)
                    || v.0.is_number() && value.is_number()
                {
                    *v = (value, VarType::Mutable(None));
                    return Ok(());
                } else {
                    return Err(ScopeErr::TypeMismatch(v.0.clone(), value.clone()).into());
                }
            };
        }

        let val = this
            .borrow()
            .variables
            .get(&key)
            .unwrap_or(&(RuntimeValue::Null, VarType::Constant))
            .clone();

        if val.0 != RuntimeValue::Null {
            return update_link(this, &val.0, move |x| {
                if discriminant(x) == discriminant(&value) || x.is_number() && value.is_number() {
                    *x = value.to_owned();
                    Ok(())
                } else {
                    Err(ScopeErr::TypeMismatch(x.clone(), value.clone()).into())
                }
            });
        }
    }

    if let Some(parent) = &this.borrow().parent {
        let _ = assign_var(&parent, &key, value)?;
    } else {
        return Err(ScopeErr::Variable(key.to_string()).into());
    }

    Ok(())
}
pub fn get_global_scope(this: &Rc<RefCell<Scope>>) -> Rc<RefCell<Scope>> {
    if let Some(parent) = &this.borrow().parent {
        get_global_scope(&parent)
    } else {
        this.clone()
    }
}

pub fn get_stop(this: &Rc<RefCell<Scope>>) -> Option<StopValue> {
    return get_global_scope(this).borrow().stop.clone();
}

pub fn get_var(this: &Rc<RefCell<Scope>>, key: &str) -> Result<(RuntimeValue, VarType), ScopeErr> {
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
    this: &Rc<RefCell<Scope>>,
    og_key: &str,
) -> Result<(Rc<RefCell<Scope>>, String), ScopeErr> {
    let key = this.borrow().resolve_alias(og_key).to_string();

    if this.borrow().variables.contains_key(&key) {
        return Ok((this.clone(), key));
    } else if let Some(parent) = &this.borrow().parent {
        return resolve_var(&parent, &key);
    } else {
        Err(ScopeErr::Variable(key))
    }
}
