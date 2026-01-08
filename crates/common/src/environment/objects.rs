use crate::{
    environment::{Environment, Location, Object, RuntimeType, RuntimeValue, Type},
    errors::ScopeErr,
};

impl<T: RuntimeValue, U: RuntimeType> Environment<T, U> {
    pub fn push_object(
        &mut self,
        scope: &u64,
        key: String,
        value: Object<T, U>,
    ) -> Result<(), ScopeErr> {
        if !self.scopes.get(scope).unwrap().objects.contains_key(&key) {
            self.objects.insert(self.var_counter, value);
            self.scopes
                .get_mut(scope)
                .unwrap()
                .objects
                .insert(key, self.var_counter);
            self.var_counter += 1;
            return Ok(());
        }

        Err(ScopeErr::Object(key))
    }

    pub fn get_object_type<'a>(&'a self, pointer: &u64) -> Result<&'a Type<U>, ScopeErr> {
        if let Some(x) = self.objects.get(pointer) {
            Ok(&x.object_type)
        } else {
            Err(ScopeErr::Object(pointer.to_string()))
        }
    }

    pub fn get_object(&self, pointer: &u64) -> Result<&Object<T, U>, ScopeErr> {
        if let Some(x) = self.objects.get(pointer) {
            Ok(x)
        } else {
            Err(ScopeErr::Object(pointer.to_string()))
        }
    }

    pub fn get_object_pointer(&self, scope: &u64, key: &str) -> Result<u64, ScopeErr> {
        if let Some(object) = self
            .scopes
            .get(scope)
            .unwrap()
            .objects
            .get(key)
            .map(|x| x.clone())
        {
            Ok(object)
        } else if let Some(scope) = self.scopes.get(scope).unwrap().parent {
            self.get_object_pointer(&scope, key)
        } else {
            Err(ScopeErr::Object(key.to_string()))
        }
    }

    pub fn push_function(
        &mut self,
        pointer: &u64,
        value: (String, T, Option<Location>, bool),
    ) -> Result<(), ScopeErr> {
        if let Some(object) = self.objects.get_mut(pointer) {
            if !object.functions.contains_key(&value.0) {
                object
                    .functions
                    .insert(value.0, (value.1, value.2, value.3));
                return Ok(());
            }
        }

        Err(ScopeErr::Object(pointer.to_string()))
    }

    pub fn get_function<'a>(
        &'a self,
        pointer: &u64,
        name: &str,
    ) -> Result<&'a (T, Option<Location>, bool), ScopeErr> {
        if let Some(f) = &self.objects.get(pointer).unwrap().functions.get(name) {
            Ok(f)
        } else {
            Err(ScopeErr::Function(name.to_string()))
        }
    }
}
