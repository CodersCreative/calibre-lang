use calibre_mir::environment::{MiddleObject, MiddleTypeDefType};
use calibre_mir_ty::MiddleNode;
use calibre_parser::{ast::TypeDefType, lexer::Location};

use crate::{
    environment::{Environment, InterpreterFrom, RuntimeType, RuntimeValue},
    errors::ScopeErr,
};

impl<T: RuntimeValue, U: RuntimeType> Environment<T, U> {
    pub fn get_object_type<'a>(
        &'a self,
        object_name: &str,
    ) -> Result<&'a MiddleTypeDefType, ScopeErr> {
        if let Some(x) = self.objects.get(object_name) {
            Ok(&x.object_type)
        } else {
            Err(ScopeErr::Object(object_name.to_string()))
        }
    }

    pub fn get_object(&self, object_name: &str) -> Result<&MiddleObject, ScopeErr> {
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
    ) -> Result<&'a (MiddleNode, Option<Location>, bool), ScopeErr> {
        if let Some(f) = &self.objects.get(object_name).unwrap().functions.get(name) {
            Ok(f)
        } else {
            Err(ScopeErr::Function(name.to_string()))
        }
    }
}
