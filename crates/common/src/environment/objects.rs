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
        if !self.scopes.get(scope).unwrap().objects.contains_key(&key) {
            self.objects.insert(self.counter, value);
            self.scopes
                .get_mut(scope)
                .unwrap()
                .objects
                .insert(key, self.counter);
            self.counter += 1;
            return Ok(());
        }

        Err(ScopeErr::Object(key))
    }

    pub fn get_object_type<'a>(
        &'a self,
        scope: &u64,
        key: &str,
    ) -> Result<&'a Type<U>, ScopeErr<T>> {
        if let Some(object) = self
            .scopes
            .get(scope)
            .unwrap()
            .objects
            .get(key)
            .map(|x| x.clone())
        {
            Ok(&self.objects.get(&object).unwrap().object_type)
        } else if let Some(scope) = self.scopes.get(scope).unwrap().parent {
            self.get_object_type(&scope, key)
        } else {
            Err(ScopeErr::Object(key.to_string()))
        }
    }

    pub fn get_object(&self, scope: &u64, key: &str) -> Result<&Object<T, U>, ScopeErr<T>> {
        if let Some(object) = self
            .scopes
            .get(scope)
            .unwrap()
            .objects
            .get(key)
            .map(|x| x.clone())
        {
            Ok(self.objects.get(&object).unwrap())
        } else if let Some(scope) = self.scopes.get(scope).unwrap().parent {
            self.get_object(&scope, key)
        } else {
            Err(ScopeErr::Object(key.to_string()))
        }
    }

    pub fn push_function(
        &mut self,
        scope: &u64,
        key: &str,
        value: (String, T, Option<Location>, bool),
    ) -> Result<(), ScopeErr<T>> {
        if let Some(object) = self
            .scopes
            .get(scope)
            .unwrap()
            .objects
            .get(key)
            .map(|x| x.clone())
        {
            if let Some(object) = self.objects.get_mut(&object) {
                if !object.functions.contains_key(&value.0) {
                    object
                        .functions
                        .insert(value.0, (value.1, value.2, value.3));
                    return Ok(());
                }
            }
        } else if let Some(scope) = self.scopes.get(scope).unwrap().parent {
            return self.push_function(&scope, key, value);
        }

        Err(ScopeErr::Function(key.to_string()))
    }

    pub fn get_function<'a>(
        &'a self,
        scope: &u64,
        key: &str,
        name: &str,
    ) -> Result<&'a (T, Option<Location>, bool), ScopeErr<T>> {
        if let Some(object) = self
            .scopes
            .get(scope)
            .unwrap()
            .objects
            .get(key)
            .map(|x| x.clone())
        {
            if let Some(f) = &self.objects.get(&object).unwrap().functions.get(name) {
                Ok(f)
            } else {
                Err(ScopeErr::Function(key.to_string()))
            }
        } else if let Some(scope) = self.scopes.get(scope).unwrap().parent {
            self.get_function(&scope, key, name)
        } else {
            Err(ScopeErr::Object(key.to_string()))
        }
    }
}
