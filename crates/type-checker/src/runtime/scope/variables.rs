use calibre_common::{environment::{Scope, Variable}, errors::ScopeErr};
use calibre_parser::ast::VarType;

use crate::runtime::{
    interpreter::InterpreterErr, scope::CheckerEnvironment, values::RuntimeType
};

impl CheckerEnvironment {
    pub fn force_var(
        &mut self,
        scope: &u64,
        key: String,
        value: Variable<RuntimeType>,
    ) -> Result<(), ScopeErr<RuntimeType>> {
        if let Some(vars) = self.variables.get_mut(scope) {
            vars.insert(key.clone(), value);
        }

        Ok(())
    }
    pub fn push_var(
        &mut self,
        scope: &u64,
        key: String,
        value: Variable<RuntimeType>,
    ) -> Result<(), ScopeErr<RuntimeType>> {
        if let Some(vars) = self.variables.get_mut(scope) {
            if let Some(var) = vars.get_mut(&key) {
                if var.var_type == VarType::Constant {
                    return Err(ScopeErr::AssignConstant(key));
                }
            }
            vars.insert(key.clone(), value);
        }

        Ok(())
    }

    pub fn get_var<'a>(&'a self, scope: &u64, key: &str) -> Result<&'a Variable<RuntimeType>, ScopeErr<RuntimeType>> {
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

    pub fn update_var<F>(&mut self, scope: &u64, key: &str, mut f: F) -> Result<(), ScopeErr<RuntimeType>>
    where
        F: FnMut(&mut Variable<RuntimeType>),
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
        value: RuntimeType,
    ) -> Result<(), InterpreterErr> {
        if let Some(vars) = self.variables.get_mut(scope) {
            if let Some(var) = vars.get_mut(key) {
                match var.var_type {
                    VarType::Mutable => {}
                    _ => return Err(ScopeErr::AssignConstant(key.to_string()).into()),
                }

                if var.value != value {
                    return Err(ScopeErr::TypeMismatch(var.value.clone(), value).into())
                }

            } else if let Some(scope) = self.scopes.get(scope).unwrap().parent {
                return self.assign_var(&scope, key, value);
            } else {
                return Err(ScopeErr::Variable(key.to_string()).into());
            }
        }
        Ok(())
    }

    pub fn get_global_scope<'a>(&'a self) -> &'a Scope {
        self.scopes.get(&0).unwrap()
    }
}
