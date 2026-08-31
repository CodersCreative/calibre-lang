use crate::{
    VM,
    error::RuntimeError,
    native::{
        NativeFunction,
        utils::{expect_num_args, pop_or_null, resolve_int, resolve_str},
    },
    value::{GcVec, RuntimeValue},
};
use dumpster::sync::Gc;
use ustr::Ustr;

pub struct EnvGet;

impl NativeFunction for EnvGet {
    fn name(&self) -> String {
        String::from("env.get")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let idx = resolve_int(env, &pop_or_null(&mut args))? as usize;

        let Some(value) = env.program_args().get(idx) else {
            return Ok(RuntimeValue::Option(None));
        };

        Ok(RuntimeValue::Option(Some(Gc::new(RuntimeValue::Str(
            value.clone(),
        )))))
    }
}

pub struct EnvVar;

impl NativeFunction for EnvVar {
    fn name(&self) -> String {
        String::from("env.var")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let name = resolve_str(env, &pop_or_null(&mut args))?;

        match std::env::var(name.as_str()) {
            Ok(value) => Ok(RuntimeValue::Option(Some(Gc::new(RuntimeValue::Str(
                Ustr::from(&value),
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

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let name = resolve_str(env, &pop_or_null(&mut args))?;
        let value = resolve_str(env, &pop_or_null(&mut args))?;

        unsafe { std::env::set_var(name.as_str(), value.as_str()) };

        Ok(RuntimeValue::Null)
    }
}

pub struct EnvRemoveVar;

impl NativeFunction for EnvRemoveVar {
    fn name(&self) -> String {
        String::from("env.remove_var")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let name = resolve_str(env, &pop_or_null(&mut args))?;

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
            .map(|(k, v)| RuntimeValue::Str(Ustr::from(&format!("{k}={v}"))))
            .collect();

        Ok(RuntimeValue::List(Gc::new(GcVec(vars))))
    }
}
