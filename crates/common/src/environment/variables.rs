use crate::{environment::{Environment, RuntimeType, RuntimeValue, Variable}, errors::ScopeErr};

impl<T : RuntimeValue, U : RuntimeType> Environment<T, U> {
    pub fn get_var<'a>(&'a self, scope: &u64, key: &str) -> Result<&'a Variable<T>, ScopeErr<T>> {
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

    pub fn update_var<F>(&mut self, scope: &u64, key: &str, mut f: F) -> Result<(), ScopeErr<T>>
    where
        F: FnMut(&mut Variable<T>),
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

}
