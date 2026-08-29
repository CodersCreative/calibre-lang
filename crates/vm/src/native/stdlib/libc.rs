use crate::{
    VM,
    error::RuntimeError,
    native::{
        NativeFunction,
        utils::{expect_num_args, pop_or_null, resolve_int},
    },
    value::RuntimeValue,
};
use std::sync::{Arc, Mutex};
extern crate errno;
use errno::{Errno, errno, set_errno};

pub struct GetCErrNo;

impl NativeFunction for GetCErrNo {
    fn name(&self) -> String {
        String::from("libc.get_c_errno")
    }

    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[0])?;

        let e = errno();
        Ok(RuntimeValue::Int(e.0 as i64))
    }
}

pub struct GetCErrNoDescription;

impl NativeFunction for GetCErrNoDescription {
    fn name(&self) -> String {
        String::from("libc.get_c_errno_description")
    }

    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[0])?;

        let e = errno();
        Ok(RuntimeValue::Str(Arc::new(Mutex::new(e.to_string()))))
    }
}

pub struct SetCErrNo;

impl NativeFunction for SetCErrNo {
    fn name(&self) -> String {
        String::from("libc.set_c_errno")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let code = resolve_int(env, &pop_or_null(&mut args))?;
        set_errno(Errno(code as i32));
        Ok(RuntimeValue::Null)
    }
}
