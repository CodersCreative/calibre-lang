use crate::{
    VM,
    error::RuntimeError,
    native::{
        NativeFunction,
        utils::{expect_num_args, pop_or_null, resolve_char, resolve_str},
    },
    value::{GcVec, RuntimeValue},
};
use dumpster::sync::Gc;
use std::sync::Arc;
use wasm_lite_std::Mutex;

pub struct CharLowercase;

impl NativeFunction for CharLowercase {
    fn name(&self) -> String {
        String::from("str.char_lowercase")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let c = resolve_char(env, &pop_or_null(&mut args))?;

        Ok(RuntimeValue::Char(c.to_lowercase().next().unwrap_or(c)))
    }
}

pub struct CharUppercase;

impl NativeFunction for CharUppercase {
    fn name(&self) -> String {
        String::from("str.char_uppercase")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let c = resolve_char(env, &pop_or_null(&mut args))?;

        Ok(RuntimeValue::Char(c.to_uppercase().next().unwrap_or(c)))
    }
}

pub struct StrSplit;

impl NativeFunction for StrSplit {
    fn name(&self) -> String {
        String::from("str.split")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let delim = resolve_str(env, &pop_or_null(&mut args))?;
        let text = resolve_str(env, &pop_or_null(&mut args))?;

        let parts = if delim.lock_sync().is_empty() {
            text.lock_sync()
                .chars()
                .map(|c| RuntimeValue::Str(Arc::new(Mutex::new(c.to_string()))))
                .collect::<Vec<_>>()
        } else {
            text.lock_sync()
                .split(delim.lock_sync().as_str())
                .map(|s| RuntimeValue::Str(Arc::new(Mutex::new(s.to_string()))))
                .collect::<Vec<_>>()
        };

        Ok(RuntimeValue::List(Gc::new(GcVec(parts))))
    }
}

pub struct StrContains;

impl NativeFunction for StrContains {
    fn name(&self) -> String {
        String::from("str.contains")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let needle = resolve_str(env, &pop_or_null(&mut args))?;
        let text = resolve_str(env, &pop_or_null(&mut args))?;

        Ok(RuntimeValue::Bool(
            text.lock_sync()
                .as_str()
                .contains(needle.lock_sync().as_str()),
        ))
    }
}

pub struct StrStartsWith;

impl NativeFunction for StrStartsWith {
    fn name(&self) -> String {
        String::from("str.starts_with")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let prefix = resolve_str(env, &pop_or_null(&mut args))?;
        let text = resolve_str(env, &pop_or_null(&mut args))?;

        Ok(RuntimeValue::Bool(
            text.lock_sync()
                .as_str()
                .starts_with(prefix.lock_sync().as_str()),
        ))
    }
}

pub struct StrEndsWith;

impl NativeFunction for StrEndsWith {
    fn name(&self) -> String {
        String::from("str.ends_with")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let suffix = resolve_str(env, &pop_or_null(&mut args))?;
        let text = resolve_str(env, &pop_or_null(&mut args))?;

        Ok(RuntimeValue::Bool(
            text.lock_sync()
                .as_str()
                .ends_with(suffix.lock_sync().as_str()),
        ))
    }
}
