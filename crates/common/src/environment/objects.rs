use calibre_parser::lexer::Location;

use crate::{
    environment::{Environment, Object, RuntimeType, RuntimeValue, Type},
    errors::ScopeErr,
};

impl<T: RuntimeValue, U: RuntimeType> Environment<T, U> {
    pub fn get_object_type<'a>(&'a self, object_name: &str) -> Result<&'a Type<U>, ScopeErr> {
        if let Some(x) = self.objects.get(object_name) {
            Ok(&x.object_type)
        } else {
            Err(ScopeErr::Object(object_name.to_string()))
        }
    }

    pub fn get_object(&self, object_name: &str) -> Result<&Object<T, U>, ScopeErr> {
        if let Some(x) = self.objects.get(object_name) {
            Ok(x)
        } else {
            Err(ScopeErr::Object(object_name.to_string()))
        }
    }

    pub fn get_function<'a>(
        &'a self,
        object_name: &str,
        name: &str,
    ) -> Result<&'a (T, Option<Location>, bool), ScopeErr> {
        if let Some(f) = &self.objects.get(object_name).unwrap().functions.get(name) {
            Ok(f)
        } else {
            Err(ScopeErr::Function(name.to_string()))
        }
    }
}
