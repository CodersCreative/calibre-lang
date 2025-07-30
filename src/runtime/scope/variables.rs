use std::{cell::RefCell, mem::discriminant, rc::Rc};

use crate::runtime::{
    interpreter::InterpreterErr,
    scope::{Environment, ScopeErr, VarType, Variable},
    values::{RuntimeType, RuntimeValue, helper::StopValue},
};

use super::Scope;

impl Environment {
    pub fn push_var(
        &mut self,
        scope: &u64,
        key: String,
        value: Variable,
    ) -> Result<RuntimeValue, ScopeErr> {
        if let Some(vars) = self.variables.get_mut(scope) {
            if let Some(var) = vars.get(&key) {
                if var.var_type == VarType::Constant {
                    return Err(ScopeErr::AssignConstant(key));
                }
            } else {
                let typ = (&value.value).into();
                vars.insert(key.clone(), value);
                return Ok(RuntimeValue::Link(*scope, vec![key], typ));
            }
        }
        Ok(self.assign_var(scope, &key, value.value).unwrap())
    }

    pub fn get_var_ref<'a>(&'a self, scope: &u64, key: &str) -> Result<Variable, ScopeErr> {
        Ok(if let Some(vars) = self.variables.get(scope) {
            if let Some(var) = vars.get(key) {
                Variable{value : RuntimeValue::Link(scope.clone(), vec![key.to_string()], (&var.value).into()), var_type : var.var_type.clone()}
            } else if let Some(scope) = self.scopes.get(scope).unwrap().parent {
                self.get_var_ref(&scope, key)?
            }else {
                return Err(ScopeErr::Variable(key.to_string()));
            }
        } else {
            return Err(ScopeErr::Variable(key.to_string()));
        })
    }

    pub fn get_var<'a>(&'a self, scope: &u64, key: &str) -> Result<&'a Variable, ScopeErr> {
        Ok(if let Some(vars) = self.variables.get(scope) {
            if let Some(var) = vars.get(key) {
                var
            } else if let Some(scope) = self.scopes.get(scope).unwrap().parent {
                self.get_var(&scope, key)?
            }else {
                return Err(ScopeErr::Variable(key.to_string()));
            }
        } else {
            return Err(ScopeErr::Variable(key.to_string()));
        })
    }

    pub fn update_var<F>(&mut self, scope: &u64, key: &str, mut f: F) -> Result<(), ScopeErr>
    where
        F: FnMut(&mut Variable),
    {
        Ok(if let Some(vars) = self.variables.get_mut(scope) {
            if let Some(var) = vars.get_mut(key) {
                f(var);
            } else {
                return Err(ScopeErr::Variable(key.to_string()));
            }
        } else {
            return Err(ScopeErr::Variable(key.to_string()));
        })
    }

    pub fn assign_var(
        &mut self,
        scope: &u64,
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
                        *var = Variable {
                            value,
                            var_type: VarType::Mutable(None),
                        };
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
            .unwrap_or(&Variable {
                value: RuntimeValue::Null,
                var_type: VarType::Constant,
            })
            .clone();

        if val.value != RuntimeValue::Null {
            let _ = self.update_link(&val.value, move |x| {
                if discriminant(x) == discriminant(&value) || x.is_number() && value.is_number() {
                    *x = value.to_owned();
                    Ok(())
                } else {
                    Err(ScopeErr::TypeMismatch(x.clone(), value.clone()).into())
                }
            });
        }

        Ok(RuntimeValue::Link(*scope, vec![key.to_string()], typ))
    }

    pub fn get_global_scope<'a>(&'a self) -> &'a Scope {
        self.scopes.get(&0).unwrap()
    }
}
