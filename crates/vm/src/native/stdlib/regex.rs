use crate::{
    VM,
    error::RuntimeError,
    native::{
        NativeFunction,
        utils::{expect_num_args, pop_or_null, resolve_str},
    },
    value::RuntimeValue,
};
use regex::Regex;
use std::sync::Arc;
use wasm_sync::Mutex;

pub struct IsMatchFn;

impl NativeFunction for IsMatchFn {
    fn name(&self) -> String {
        String::from("regex.is_match")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let text = resolve_str(env, &pop_or_null(&mut args))?;
        let pattern = resolve_str(env, &pop_or_null(&mut args))?;

        let re = Regex::new(pattern.lock().unwrap().as_str())
            .map_err(|e| RuntimeError::Io(e.to_string()))?;

        Ok(RuntimeValue::Bool(
            re.is_match(text.lock().unwrap().as_str()),
        ))
    }
}

pub struct FindFn;

impl NativeFunction for FindFn {
    fn name(&self) -> String {
        String::from("regex.find")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let text = resolve_str(env, &pop_or_null(&mut args))?;
        let pattern = resolve_str(env, &pop_or_null(&mut args))?;

        let re = Regex::new(pattern.lock().unwrap().as_str())
            .map_err(|e| RuntimeError::Io(e.to_string()))?;
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

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[3])?;

        let replacement = resolve_str(env, &pop_or_null(&mut args))?;
        let text = resolve_str(env, &pop_or_null(&mut args))?;
        let pattern = resolve_str(env, &pop_or_null(&mut args))?;

        let re = Regex::new(pattern.lock().unwrap().as_str())
            .map_err(|e| RuntimeError::Io(e.to_string()))?;
        let text = text.lock().unwrap();
        let out = re.replace_all(text.as_str(), replacement.lock().unwrap().as_str());
        Ok(RuntimeValue::Str(Arc::new(Mutex::new(out.to_string()))))
    }
}
