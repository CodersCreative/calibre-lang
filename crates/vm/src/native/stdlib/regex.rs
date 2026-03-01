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
        let re = Regex::new(&pattern).map_err(|e| RuntimeError::Io(e.to_string()))?;
        Ok(RuntimeValue::Bool(re.is_match(&text)))
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
        let re = Regex::new(&pattern).map_err(|e| RuntimeError::Io(e.to_string()))?;
        let found = re
            .find(&text)
            .map(|m| RuntimeValue::Str(std::sync::Arc::new(m.as_str().to_string())));
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
        let re = Regex::new(&pattern).map_err(|e| RuntimeError::Io(e.to_string()))?;
        let out = re.replace_all(&text, replacement.as_str());
        Ok(RuntimeValue::Str(std::sync::Arc::new(out.to_string())))
    }
}
