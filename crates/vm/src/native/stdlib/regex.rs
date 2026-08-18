use std::sync::{Arc, Mutex};
use crate::{
    VM,
    error::RuntimeError,
    native::{NativeFunction, pop_or_null},
    value::RuntimeValue,
};
use regex::Regex;

pub struct IsMatchFn;

impl NativeFunction for IsMatchFn {
    fn name(&self) -> String {
        String::from("regex.is_match")
    }

    fn run(
        &self,
        _env: &mut VM,
        mut args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let text = pop_or_null(&mut args);
        let pattern = pop_or_null(&mut args);
        let RuntimeValue::Str(text) = text else {
            return Err(RuntimeError::UnexpectedType(text));
        };
        let RuntimeValue::Str(pattern) = pattern else {
            return Err(RuntimeError::UnexpectedType(pattern));
        };
        let re = Regex::new(pattern.lock().unwrap().as_str()).map_err(|e| RuntimeError::Io(e.to_string()))?;
        Ok(RuntimeValue::Bool(re.is_match(text.lock().unwrap().as_str())))
    }
}

pub struct FindFn;

impl NativeFunction for FindFn {
    fn name(&self) -> String {
        String::from("regex.find")
    }

    fn run(
        &self,
        _env: &mut VM,
        mut args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let text = pop_or_null(&mut args);
        let pattern = pop_or_null(&mut args);
        let RuntimeValue::Str(text) = text else {
            return Err(RuntimeError::UnexpectedType(text));
        };
        let RuntimeValue::Str(pattern) = pattern else {
            return Err(RuntimeError::UnexpectedType(pattern));
        };
        let re = Regex::new(pattern.lock().unwrap().as_str()).map_err(|e| RuntimeError::Io(e.to_string()))?;
        let found = re
            .find(text.lock().unwrap().as_str())
            .map(|m| RuntimeValue::Str(Arc::new(Mutex::new(m.as_str().to_string()))));
        Ok(RuntimeValue::Option(found.map(dumpster::sync::Gc::new)))
    }
}

pub struct ReplaceFn;

impl NativeFunction for ReplaceFn {
    fn name(&self) -> String {
        String::from("regex.replace")
    }

    fn run(
        &self,
        _env: &mut VM,
        mut args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let replacement = pop_or_null(&mut args);
        let text = pop_or_null(&mut args);
        let pattern = pop_or_null(&mut args);
        let RuntimeValue::Str(replacement) = replacement else {
            return Err(RuntimeError::UnexpectedType(replacement));
        };
        let RuntimeValue::Str(text) = text else {
            return Err(RuntimeError::UnexpectedType(text));
        };
        let RuntimeValue::Str(pattern) = pattern else {
            return Err(RuntimeError::UnexpectedType(pattern));
        };
        let re = Regex::new(pattern.lock().unwrap().as_str()).map_err(|e| RuntimeError::Io(e.to_string()))?;
        let text = text.lock().unwrap();
        let out = re.replace_all(text.as_str(), replacement.lock().unwrap().as_str());
        Ok(RuntimeValue::Str(Arc::new(Mutex::new(out.to_string()))))
    }
}
