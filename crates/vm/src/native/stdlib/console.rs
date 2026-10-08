use crate::{
    VM,
    error::RuntimeError,
    native::NativeFunction,
    value::{RuntimeValue, StringReader},
};
use std::{
    io::{self, BufReader},
    sync::Arc,
};
use wasm_sync::Mutex;
pub struct InputStream;

impl NativeFunction for InputStream {
    fn name(&self) -> String {
        String::from("console.input_stream")
    }

    fn run(&self, _env: &mut VM, _args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let reader = BufReader::new(io::stdin());
        Ok(RuntimeValue::Reader(StringReader(Arc::new(Mutex::new(
            reader,
        )))))
    }
}
