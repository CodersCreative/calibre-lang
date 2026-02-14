use dumpster::sync::Gc;

use crate::{VM, error::RuntimeError, native::NativeFunction, value::RuntimeValue};

pub struct StrSplit();

impl NativeFunction for StrSplit {
    fn name(&self) -> String {
        String::from("str.split")
    }

    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let Some(RuntimeValue::Str(text)) = args.get(0) else {
            return Err(RuntimeError::UnexpectedType(
                args.get(0).cloned().unwrap_or(RuntimeValue::Null),
            ));
        };
        let Some(RuntimeValue::Str(delim)) = args.get(1) else {
            return Err(RuntimeError::UnexpectedType(
                args.get(1).cloned().unwrap_or(RuntimeValue::Null),
            ));
        };

        let parts = if delim.is_empty() {
            text.chars()
                .map(|c| RuntimeValue::Str(c.to_string()))
                .collect::<Vec<_>>()
        } else {
            text.split(delim)
                .map(|s| RuntimeValue::Str(s.to_string()))
                .collect::<Vec<_>>()
        };

        Ok(RuntimeValue::List(Gc::new(crate::value::GcVec(parts))))
    }
}

pub struct StrContains();

impl NativeFunction for StrContains {
    fn name(&self) -> String {
        String::from("str.contains")
    }

    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let Some(RuntimeValue::Str(text)) = args.get(0) else {
            return Err(RuntimeError::UnexpectedType(
                args.get(0).cloned().unwrap_or(RuntimeValue::Null),
            ));
        };
        let Some(RuntimeValue::Str(needle)) = args.get(1) else {
            return Err(RuntimeError::UnexpectedType(
                args.get(1).cloned().unwrap_or(RuntimeValue::Null),
            ));
        };

        Ok(RuntimeValue::Bool(text.contains(needle)))
    }
}

pub struct StrStartsWith();

impl NativeFunction for StrStartsWith {
    fn name(&self) -> String {
        String::from("str.starts_with")
    }

    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let Some(RuntimeValue::Str(text)) = args.get(0) else {
            return Err(RuntimeError::UnexpectedType(
                args.get(0).cloned().unwrap_or(RuntimeValue::Null),
            ));
        };
        let Some(RuntimeValue::Str(prefix)) = args.get(1) else {
            return Err(RuntimeError::UnexpectedType(
                args.get(1).cloned().unwrap_or(RuntimeValue::Null),
            ));
        };

        Ok(RuntimeValue::Bool(text.starts_with(prefix)))
    }
}

pub struct StrEndsWith();

impl NativeFunction for StrEndsWith {
    fn name(&self) -> String {
        String::from("str.ends_with")
    }

    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let Some(RuntimeValue::Str(text)) = args.get(0) else {
            return Err(RuntimeError::UnexpectedType(
                args.get(0).cloned().unwrap_or(RuntimeValue::Null),
            ));
        };
        let Some(RuntimeValue::Str(suffix)) = args.get(1) else {
            return Err(RuntimeError::UnexpectedType(
                args.get(1).cloned().unwrap_or(RuntimeValue::Null),
            ));
        };

        Ok(RuntimeValue::Bool(text.ends_with(suffix)))
    }
}
