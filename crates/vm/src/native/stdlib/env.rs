use std::sync::Arc;

use crate::{VM, error::RuntimeError, native::NativeFunction, value::RuntimeValue};
use dumpster::sync::Gc;

pub struct EnvGet;

impl NativeFunction for EnvGet {
    fn name(&self) -> String {
        String::from("env.get")
    }

    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let Some(idx) = args.first() else {
            return Err(RuntimeError::InvalidFunctionCall);
        };

        let idx = match idx {
            RuntimeValue::Int(value) if *value >= 0 => *value as usize,
            RuntimeValue::UInt(value) => *value as usize,
            RuntimeValue::Byte(value) => *value as usize,
            other => return Err(RuntimeError::UnexpectedType(other.clone())),
        };

        let Some(value) = env.program_args().get(idx) else {
            return Ok(RuntimeValue::Option(None));
        };

        Ok(RuntimeValue::Option(Some(Gc::new(RuntimeValue::Str(
            Arc::new(value.clone()),
        )))))
    }
}

pub struct EnvVar;

impl NativeFunction for EnvVar {
    fn name(&self) -> String {
        String::from("env.var")
    }

    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let Some(name) = args.first() else {
            return Err(RuntimeError::InvalidFunctionCall);
        };

        let RuntimeValue::Str(name) = name else {
            return Err(RuntimeError::UnexpectedType(name.clone()));
        };

        match std::env::var(name.as_str()) {
            Ok(value) => Ok(RuntimeValue::Option(Some(Gc::new(RuntimeValue::Str(
                Arc::new(value),
            ))))),
            Err(std::env::VarError::NotPresent) => Ok(RuntimeValue::Option(None)),
            Err(err) => Err(RuntimeError::Io(err.to_string())),
        }
    }
}

pub struct EnvSetVar;

impl NativeFunction for EnvSetVar {
    fn name(&self) -> String {
        String::from("env.set_var")
    }

    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        if args.len() != 2 {
            return Err(RuntimeError::InvalidFunctionCall);
        }

        let RuntimeValue::Str(name) = &args[0] else {
            return Err(RuntimeError::UnexpectedType(args[0].clone()));
        };
        let RuntimeValue::Str(value) = &args[1] else {
            return Err(RuntimeError::UnexpectedType(args[1].clone()));
        };

        unsafe { std::env::set_var(name.as_str(), value.as_str()) };

        Ok(RuntimeValue::Null)
    }
}

pub struct EnvRemoveVar;

impl NativeFunction for EnvRemoveVar {
    fn name(&self) -> String {
        String::from("env.remove_var")
    }

    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let Some(name) = args.first() else {
            return Err(RuntimeError::InvalidFunctionCall);
        };

        let RuntimeValue::Str(name) = name else {
            return Err(RuntimeError::UnexpectedType(name.clone()));
        };

        unsafe { std::env::remove_var(name.as_str()) };

        Ok(RuntimeValue::Null)
    }
}

pub struct EnvVars;

impl NativeFunction for EnvVars {
    fn name(&self) -> String {
        String::from("env.vars")
    }

    fn run(&self, _env: &mut VM, _args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let vars = std::env::vars()
            .map(|(k, v)| RuntimeValue::Str(Arc::new(format!("{k}={v}"))))
            .collect();

        Ok(RuntimeValue::List(Gc::new(crate::value::GcVec(vars))))
    }
}
