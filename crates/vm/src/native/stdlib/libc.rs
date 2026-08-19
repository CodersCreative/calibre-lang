extern crate errno;
use crate::{
    VM,
    error::RuntimeError,
    native::{NativeFunction, expect_int, pop_or_null},
    value::RuntimeValue,
};
use errno::{Errno, errno, set_errno};
use std::sync::{Arc, Mutex};

pub struct GetCErrNo;

impl NativeFunction for GetCErrNo {
    fn name(&self) -> String {
        String::from("libc.get_c_errno")
    }

    fn run(&self, _env: &mut VM, _args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let e = errno();
        Ok(RuntimeValue::Int(e.0 as i64))
    }
}

pub struct GetCErrNoDescription;

impl NativeFunction for GetCErrNoDescription {
    fn name(&self) -> String {
        String::from("libc.get_c_errno_description")
    }

    fn run(&self, _env: &mut VM, _args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let e = errno();
        Ok(RuntimeValue::Str(Arc::new(Mutex::new(e.to_string()))))
    }
}

pub struct SetCErrNo;

impl NativeFunction for SetCErrNo {
    fn name(&self) -> String {
        String::from("libc.set_c_errno")
    }

    fn run(
        &self,
        _env: &mut VM,
        mut args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let code = expect_int(pop_or_null(&mut args))?;
        set_errno(Errno(code as i32));
        Ok(RuntimeValue::Null)
    }
}
