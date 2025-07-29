use std::{cell::RefCell, mem::discriminant, rc::Rc};

use crate::runtime::{
    interpreter::InterpreterErr,
    scope::{links::update_link, Environment, ScopeErr, VarType, Variable},
    values::{helper::StopValue, RuntimeType, RuntimeValue},
};

use super::Scope;

impl Environment {
    pub fn push_var<'a>(
        &'a mut self,
        scope : &u64,
        key: String,
        value: Variable,
    ) -> Result<RuntimeValue, ScopeErr> {
        if let Some(vars) = self.variables.get_mut(scope) {
            if let Some(var) = vars.get(&key) {
                if var.var_type == VarType::Constant {
                    return Err(ScopeErr::AssignConstant(key));
                }

            } else{
                let typ = (&value.value).into();
                vars.insert(key.clone(), value);
                return Ok(RuntimeValue::Link(*scope, vec![key], typ));
            }

        }
        Ok(self.assign_var(scope, &key, value.value).unwrap())
    }

    pub fn update_var<F>(&mut self, key: &str, mut f: F) -> Result<(), ScopeErr>
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

    pub fn assign_var<'a>(
        &'a mut self,
        scope : &u64,
        key: &str,
        value: RuntimeValue,
    ) -> Result<RuntimeValue, InterpreterErr> {
        let typ = (&value).into();
        if let Some(vars) = self.variables.get_mut(scope) {
            if let Some(var) = vars.get_mut(key) {
                match var.var_type {
                    VarType::Mutable(_) => {}
                    _ => return Err(ScopeErr::AssignConstant(key.to_string()).into()),
                }

                if let RuntimeValue::Link(_, _, _) = &var.value {
                } else {
                    if discriminant(&var.value) == discriminant(&value)
                        || var.value.is_number() && value.is_number()
                    {
                        *var = Variable{value, var_type : VarType::Mutable(None)};
                    } else {
                        return Err(ScopeErr::TypeMismatch(var.value.clone(), value.clone()).into());
                    }
                    return Ok(var.value.clone());
                };
            }
        }

        let val = self 
            .variables
            .get(scope)
            .unwrap()
            .get(key)
            .unwrap_or(&Variable{value : RuntimeValue::Null, var_type : VarType::Constant})
            .clone();

        if val.value != RuntimeValue::Null {
            return update_link(this, &val.0, move |x| {
                if discriminant(x) == discriminant(&value) || x.is_number() && value.is_number() {
                    *x = value.to_owned();
                    Ok(())
                } else {
                    Err(ScopeErr::TypeMismatch(x.clone(), value.clone()).into())
                }
            });
        }

        Ok(RuntimeValue::Link(*scope, vec![key.to_string()], typ))
        // Ok(&self.variables.get(scope).unwrap().get(key).unwrap().value)
    }
    
    pub fn get_global_scope<'a>(&'a self) -> &'a Scope {
        self.scopes.get(&0).unwrap()
    }
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
