use std::time::{SystemTime, UNIX_EPOCH};

use crate::{
    VM,
    error::RuntimeError,
    native::{NativeFunction, utils::expect_num_args},
    value::RuntimeValue,
};

pub struct UnixNow;

impl NativeFunction for UnixNow {
    fn name(&self) -> String {
        String::from("time.unix_now")
    }

    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[0])?;

        let time = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();

        Ok(RuntimeValue::UInt(time.as_millis() as usize))
    }
}
