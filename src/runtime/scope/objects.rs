
use crate::runtime::{
    scope::{Environment, Object, ScopeErr, Type},
    values::RuntimeValue,
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

    pub fn get_object<'a>(&'a self, scope: &u64, key: &str) -> Result<&'a Type, ScopeErr> {
        if let Some(objects) = self.objects.get(&scope) {
            if let Some(object) = objects.get(key) {
                return Ok(&object.object_type);
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
                }
            }
        }
        Err(ScopeErr::Function(key.to_string()))
    }
}
