use crate::{
    environment::{Environment, Location, Object, RuntimeType, RuntimeValue, Type},
    errors::{ScopeErr, ValueErr},
};

impl<T: RuntimeValue, U: RuntimeType> Environment<T, U> {
    pub fn push_object(
        &mut self,
        scope: &u64,
        key: String,
        value: Object<T, U>,
    ) -> Result<(), ScopeErr<T>> {
        if let Some(objects) = self.objects.get_mut(&scope) {
            if !objects.contains_key(&key) {
                objects.insert(key, value);
                return Ok(());
            }
        }
        Err(ScopeErr::Object(key))
    }

    pub fn get_object_type<'a>(
        &'a self,
        scope: &u64,
        key: &str,
    ) -> Result<&'a Type<U>, ScopeErr<T>> {
        if let Some(objects) = self.objects.get(&scope) {
            if let Some(object) = objects.get(key) {
                return Ok(&object.unwrap(self, scope).unwrap().object_type);
            } else if let Some(scope) = self.scopes.get(scope).unwrap().parent {
                return self.get_object_type(&scope, key);
            }
        }
        Err(ScopeErr::Object(key.to_string()))
    }

    pub fn get_object(&self, scope: &u64, key: &str) -> Result<&Object<T, U>, ScopeErr<T>> {
        if let Some(objects) = self.objects.get(&scope) {
            if let Some(object) = objects.get(key) {
                return Ok(object.unwrap(self, scope).unwrap());
            } else if let Some(scope) = self.scopes.get(scope).unwrap().parent {
                return self.get_object(&scope, key);
            }
        }
        Err(ScopeErr::Function(key.to_string()))
    }

    pub fn push_function(
        &mut self,
        scope: &u64,
        key: &str,
        value: (String, T, Option<Location>, bool),
    ) -> Result<(), ScopeErr<T>> {
        if let Some(objects) = self.objects.get_mut(&scope) {
            if let Some(object) = objects.get_mut(key) {
                if !object.functions.contains_key(&value.0) {
                    object
                        .functions
                        .insert(value.0, (value.1, value.2, value.3));
                    return Ok(());
                }
            }
        }
        Err(ScopeErr::Function(key.to_string()))
    }

    pub fn get_function<'a>(
        &'a self,
        scope: &u64,
        key: &str,
        name: &str,
    ) -> Result<&'a (T, Option<Location>, bool), ScopeErr<T>> {
        if let Some(objects) = self.objects.get(&scope) {
            if let Some(object) = objects.get(key) {
                if let Some(func) = object.functions.get(name) {
                    return Ok(func);
                } else if let Some(scope) = self.scopes.get(scope).unwrap().parent {
                    return self.get_function(&scope, key, name);
                }
            }
        }
        Err(ScopeErr::Function(key.to_string()))
    }
}

impl<T: RuntimeValue, U: RuntimeType> Object<T, U> {
    pub fn unwrap<'a>(
        &'a self,
        env: &'a Environment<T, U>,
        _scope: &u64,
    ) -> Result<&'a Object<T, U>, ValueErr<T, U>> {
        match &self.object_type {
            Type::Link(scope, name) => Ok(env.objects.get(&scope).unwrap().get(name).unwrap()),
            _ => Ok(self),
        }
    }
}
