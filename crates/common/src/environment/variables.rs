use crate::{
    environment::{Environment, RuntimeType, RuntimeValue, Variable},
    errors::ScopeErr,
};

impl<T: RuntimeValue, U: RuntimeType> Environment<T, U> {
    pub fn update_var<F>(&mut self, name: &str, mut f: F) -> Result<(), ScopeErr>
    where
        F: FnMut(&mut Variable<T>),
    {
        if let Some(var) = self.variables.get_mut(name) {
            Ok(f(var))
        } else {
            Err(ScopeErr::Variable(name.to_string()))
        }
    }
}
