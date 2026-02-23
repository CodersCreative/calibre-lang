use crate::{VM, error::RuntimeError, native::NativeFunction, value::RuntimeValue};
use dumpster::sync::Gc;

pub struct ArgsLen;

impl NativeFunction for ArgsLen {
    fn name(&self) -> String {
        String::from("args.len")
    }

    fn run(&self, env: &mut VM, _args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        Ok(RuntimeValue::Int(env.program_args().len() as i64))
    }
}

pub struct ArgsGet;

impl NativeFunction for ArgsGet {
    fn name(&self) -> String {
        String::from("args.get")
    }

    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let Some(idx) = args.first() else {
            return Err(RuntimeError::InvalidFunctionCall);
        };
        let idx = match idx {
            RuntimeValue::Int(value) if *value >= 0 => *value as usize,
            RuntimeValue::UInt(value) => *value as usize,
            other => return Err(RuntimeError::UnexpectedType(other.clone())),
        };
        let Some(value) = env.program_args().get(idx) else {
            return Ok(RuntimeValue::Option(None));
        };
        Ok(RuntimeValue::Option(Some(Gc::new(RuntimeValue::Str(
            std::sync::Arc::new(value.clone()),
        )))))
    }
}

pub struct ArgsAll;

impl NativeFunction for ArgsAll {
    fn name(&self) -> String {
        String::from("args.all")
    }

    fn run(&self, env: &mut VM, _args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let values = env
            .program_args()
            .iter()
            .cloned()
            .map(|value| RuntimeValue::Str(std::sync::Arc::new(value)))
            .collect();
        Ok(RuntimeValue::List(Gc::new(crate::value::GcVec(values))))
    }
}
