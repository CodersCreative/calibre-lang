use crate::runtime::{
    scope::{Environment, Object, ScopeErr, Type},
    values::{RuntimeValue, ValueErr},
};

impl Environment {
    pub fn push_object(&mut self, scope: &u64, key: String, value: Object) -> Result<(), ScopeErr> {
        if let Some(objects) = self.objects.get_mut(&scope) {
            if !objects.contains_key(&key) {
                objects.insert(key, value);
                return Ok(());
            }
        }
        Err(ScopeErr::Object(key))
    }

    pub fn get_object_type<'a>(&'a self, scope: &u64, key: &str) -> Result<&'a Type, ScopeErr> {
        if let Some(objects) = self.objects.get(&scope) {
            if let Some(object) = objects.get(key) {
                return Ok(&object.unwrap(self, scope).unwrap().object_type);
            } else if let Some(scope) = self.scopes.get(scope).unwrap().parent {
                return self.get_object_type(&scope, key);
            }
        }
        Err(ScopeErr::Object(key.to_string()))
    }

    pub fn get_object(&self, scope: &u64, key: &str) -> Result<&Object, ScopeErr> {
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
        value: (String, RuntimeValue, bool),
    ) -> Result<(), ScopeErr> {
        if let Some(objects) = self.objects.get_mut(&scope) {
            if let Some(object) = objects.get_mut(key) {
                if !object.functions.contains_key(&value.0) {
                    object.functions.insert(value.0, (value.1, value.2));
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
    ) -> Result<&'a (RuntimeValue, bool), ScopeErr> {
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

impl Object {
    pub fn unwrap<'a>(&'a self, env: &'a Environment, scope: &u64) -> Result<&'a Object, ValueErr> {
        match &self.object_type {
            Type::Link(scope, name) => Ok(env.objects.get(&scope).unwrap().get(name).unwrap()),
            _ => Ok(self),
        }
    }
}
