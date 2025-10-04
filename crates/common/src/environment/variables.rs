use crate::{
    environment::{Environment, RuntimeType, RuntimeValue, Variable},
    errors::ScopeErr,
};

impl<T: RuntimeValue, U: RuntimeType> Environment<T, U> {
    pub fn get_var<'a>(&'a self, scope: &u64, key: &str) -> Result<&'a Variable<T>, ScopeErr<T>> {
        let pointer = if let Some(pointer) = self.scopes.get(scope).unwrap().variables.get(key) {
            pointer.clone()
        } else {
            return Err(ScopeErr::Variable(key.to_string()));
        };

        Ok(if let Some(var) = self.variables.get(&pointer) {
            var
        } else if let Some(scope) = self.scopes.get(scope).unwrap().parent {
            self.get_var(&scope, key)?
        } else {
            return Err(ScopeErr::Variable(key.to_string()));
        })
    }

    pub fn update_var<F>(&mut self, scope: &u64, key: &str, mut f: F) -> Result<(), ScopeErr<T>>
    where
        F: FnMut(&mut Variable<T>),
    {
        let pointer = if let Some(pointer) = self.scopes.get(scope).unwrap().variables.get(key) {
            pointer.clone()
        } else {
            return Err(ScopeErr::Variable(key.to_string()));
        };

        Ok(if let Some(var) = self.variables.get_mut(&pointer) {
            f(var)
        } else if let Some(scope) = self.scopes.get(scope).unwrap().parent {
            self.update_var(&scope, key, f)?
        } else {
            return Err(ScopeErr::Variable(key.to_string()));
        })
    }
}
