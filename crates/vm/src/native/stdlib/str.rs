use dumpster::sync::Gc;

use crate::{
    VM,
    error::RuntimeError,
    native::{NativeFunction, expect_str_ref},
    value::RuntimeValue,
};

#[inline]
fn expect_str_arg(
    args: &[RuntimeValue],
    index: usize,
) -> Result<&std::sync::Arc<String>, RuntimeError> {
    if let Some(value) = args.get(index) {
        expect_str_ref(value)
    } else {
        Err(RuntimeError::UnexpectedType(RuntimeValue::Null))
    }
}

pub struct StrSplit();

impl NativeFunction for StrSplit {
    fn name(&self) -> String {
        String::from("str.split")
    }

    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let text = expect_str_arg(&args, 0)?;
        let delim = expect_str_arg(&args, 1)?;

        let parts = if delim.is_empty() {
            text.chars()
                .map(|c| RuntimeValue::Str(std::sync::Arc::new(c.to_string())))
                .collect::<Vec<_>>()
        } else {
            text.split(delim.as_str())
                .map(|s| RuntimeValue::Str(std::sync::Arc::new(s.to_string())))
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
        let text = expect_str_arg(&args, 0)?;
        let needle = expect_str_arg(&args, 1)?;

        Ok(RuntimeValue::Bool(text.as_str().contains(needle.as_str())))
    }
}

pub struct StrStartsWith();

impl NativeFunction for StrStartsWith {
    fn name(&self) -> String {
        String::from("str.starts_with")
    }

    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let text = expect_str_arg(&args, 0)?;
        let prefix = expect_str_arg(&args, 1)?;

        Ok(RuntimeValue::Bool(
            text.as_str().starts_with(prefix.as_str()),
        ))
    }
}

pub struct StrEndsWith();

impl NativeFunction for StrEndsWith {
    fn name(&self) -> String {
        String::from("str.ends_with")
    }

    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let text = expect_str_arg(&args, 0)?;
        let suffix = expect_str_arg(&args, 1)?;

        Ok(RuntimeValue::Bool(text.as_str().ends_with(suffix.as_str())))
    }
}
