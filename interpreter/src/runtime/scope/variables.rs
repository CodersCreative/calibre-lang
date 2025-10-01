use calibre_common::{environment::Variable, errors::ScopeErr};
use calibre_parser::ast::VarType;

use crate::runtime::{
    interpreter::InterpreterErr,
    scope::{InterpreterEnvironment, },
    values::RuntimeValue,
};
use std::mem::discriminant;

impl InterpreterEnvironment {
    pub fn force_var(
        &mut self,
        scope: &u64,
        key: String,
        value: Variable<RuntimeValue>,
    ) -> Result<RuntimeValue, ScopeErr<RuntimeValue>> {
        let typ = (&value.value).into();
        if let Some(vars) = self.variables.get_mut(scope) {
            vars.insert(key.clone(), value);
        }

        Ok(RuntimeValue::Link(*scope, vec![key], typ))
    }
    pub fn push_var(
        &mut self,
        scope: &u64,
        key: String,
        value: Variable<RuntimeValue>,
    ) -> Result<RuntimeValue, ScopeErr<RuntimeValue>> {
        let typ = (&value.value).into();
        if let Some(vars) = self.variables.get_mut(scope) {
            if let Some(var) = vars.get(&key) {
                if var.var_type == VarType::Constant {
                    return Err(ScopeErr::AssignConstant(key));
                }
            }

            vars.insert(key.clone(), value);
        }

        Ok(RuntimeValue::Link(*scope, vec![key], typ))
    }

    pub fn get_var_ref<'a>(&'a self, scope: &u64, key: &str) -> Result<Variable<RuntimeValue>, ScopeErr<RuntimeValue>> {
        Ok(if let Some(vars) = self.variables.get(scope) {
            if let Some(var) = vars.get(key) {
                Variable {
                    value: match var.value {
                        RuntimeValue::Null | RuntimeValue::NativeFunction(_) => var.value.clone(),
                        _ => RuntimeValue::Link(
                            scope.clone(),
                            vec![key.to_string()],
                            (&var.value).into(),
                        ),
                    },
                    var_type: var.var_type.clone(),
                    location: var.location.clone()
                }
            } else if let Some(scope) = self.scopes.get(scope).unwrap().parent {
                self.get_var_ref(&scope, key)?
            } else {
                return Err(ScopeErr::Variable(key.to_string()));
            }
        } else {
            return Err(ScopeErr::Variable(key.to_string()));
        })
    }

    pub fn get_var<'a>(&'a self, scope: &u64, key: &str) -> Result<&'a Variable<RuntimeValue>, ScopeErr<RuntimeValue>> {
        Ok(if let Some(vars) = self.variables.get(scope) {
            if let Some(var) = vars.get(key) {
                var
            } else if let Some(scope) = self.scopes.get(scope).unwrap().parent {
                self.get_var(&scope, key)?
            } else {
                return Err(ScopeErr::Variable(key.to_string()));
            }
        } else {
            return Err(ScopeErr::Variable(key.to_string()));
        })
    }

    pub fn update_var<F>(&mut self, scope: &u64, key: &str, mut f: F) -> Result<(), ScopeErr<RuntimeValue>>
    where
        F: FnMut(&mut Variable<RuntimeValue>),
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
                    VarType::Mutable => {}
                    _ => return Err(ScopeErr::AssignConstant(key.to_string()).into()),
                }

                if let RuntimeValue::Link(_, _, _) = &var.value {
                } else {
                    if discriminant(&var.value) == discriminant(&value)
                        || var.value.is_number() && value.is_number()
                    {
                        *var = Variable {
                            value,
                            var_type: VarType::Mutable,
                            location: var.location.clone()
                        };
                    } else {
                        return Err(ScopeErr::TypeMismatch(var.value.clone(), value.clone()).into());
                    }
                    return Ok(var.value.clone());
                };
            } else if let Some(scope) = self.scopes.get(scope).unwrap().parent {
                return self.assign_var(&scope, key, value);
            } else {
                return Err(ScopeErr::Variable(key.to_string()).into());
            }
        }

        let val = self
            .get_var(scope, key)
            .unwrap_or(&Variable {
                value: RuntimeValue::Null,
                var_type: VarType::Constant,
                location: None,
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
}
