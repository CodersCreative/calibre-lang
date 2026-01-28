use crate::{VM, error::RuntimeError, native::NativeFunction, value::RuntimeValue};
use std::{
    thread::{self},
    time::Duration,
};

pub struct Wait();

impl NativeFunction for Wait {
    fn name(&self) -> String {
        String::from("wait")
    }
    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        if let Some(RuntimeValue::Int(x)) = args.get(0) {
            thread::sleep(Duration::from_secs(*x as u64));
        } else if let Some(RuntimeValue::Float(x)) = args.get(0) {
            thread::sleep(Duration::from_secs_f64(*x));
        }
        Ok(RuntimeValue::Null)
    }
}
