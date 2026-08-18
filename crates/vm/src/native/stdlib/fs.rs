use std::sync::{Arc, Mutex};
use crate::{
    VM,
    error::RuntimeError,
    native::{NativeFunction, expect_str_ref, first_arg},
    value::{GcVec, RuntimeValue},
};
use dumpster::sync::Gc;

pub struct FsReadDir;

impl NativeFunction for FsReadDir {
    fn name(&self) -> String {
        String::from("fs.read_dir")
    }

    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let path = expect_str_ref(first_arg(&args)?)?;

        match std::fs::read_dir(path.lock().unwrap().as_str()) {
            Ok(entries) => {
                let mut out = Vec::new();
                for entry in entries {
                    match entry {
                        Ok(entry) => {
                            out.push(RuntimeValue::Str(Arc::new(Mutex::new(
                                entry.path().display().to_string(),)
                            )));
                        }
                        Err(err) => {
                            return Ok(RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(
                                Arc::new(Mutex::new(err.to_string())),
                            )))));
                        }
                    }
                }

                Ok(RuntimeValue::Result(Ok(Gc::new(RuntimeValue::List(
                    Gc::new(GcVec(out)),
                )))))
            }
            Err(err) => Ok(RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(
                Arc::new(Mutex::new(err.to_string())),
            ))))),
        }
    }
}
