use std::sync::Arc;

use crate::{VM, error::RuntimeError, native::NativeFunction, value::RuntimeValue};
use dumpster::sync::Gc;

pub struct FsReadDir;

impl NativeFunction for FsReadDir {
    fn name(&self) -> String {
        String::from("fs.read_dir")
    }

    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let Some(path) = args.first() else {
            return Err(RuntimeError::InvalidFunctionCall);
        };
        let RuntimeValue::Str(path) = path else {
            return Err(RuntimeError::UnexpectedType(path.clone()));
        };

        match std::fs::read_dir(path.as_str()) {
            Ok(entries) => {
                let mut out = Vec::new();
                for entry in entries {
                    match entry {
                        Ok(entry) => {
                            out.push(RuntimeValue::Str(Arc::new(
                                entry.path().display().to_string(),
                            )));
                        }
                        Err(err) => {
                            return Ok(RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(
                                Arc::new(err.to_string()),
                            )))));
                        }
                    }
                }

                Ok(RuntimeValue::Result(Ok(Gc::new(RuntimeValue::List(
                    Gc::new(crate::value::GcVec(out)),
                )))))
            }
            Err(err) => Ok(RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(
                Arc::new(err.to_string()),
            ))))),
        }
    }
}
