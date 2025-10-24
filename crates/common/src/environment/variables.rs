use crate::{
    environment::{Environment, RuntimeType, RuntimeValue, Variable},
    errors::ScopeErr,
};

impl<T: RuntimeValue, U: RuntimeType> Environment<T, U> {
    pub fn get_var<'a>(&'a self, pointer: &u64) -> Result<&'a Variable<T>, ScopeErr> {
        Ok(if let Some(var) = self.variables.get(&pointer) {
            var
        } else {
            return Err(ScopeErr::Variable(pointer.to_string()));
        })
    }

    pub fn get_var_pointer(&self, scope: &u64, key: &str) -> Result<u64, ScopeErr> {
        if let Some(pointer) = self.scopes.get(scope).unwrap().variables.get(key) {
            Ok(pointer.clone())
        } else if let Some(scope) = self.scopes.get(scope).unwrap().parent {
            self.get_var_pointer(&scope, key)
        } else {
            Err(ScopeErr::Variable(key.to_string()))
        }
    }

    pub fn update_var<F>(&mut self, pointer: &u64, mut f: F) -> Result<(), ScopeErr>
    where
        F: FnMut(&mut Variable<T>),
    {
        if let Some(var) = self.variables.get_mut(&pointer) {
            Ok(f(var))
        } else {
            Err(ScopeErr::Variable(pointer.to_string()))
        }
    }
}
