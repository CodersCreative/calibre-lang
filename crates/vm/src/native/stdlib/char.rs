use crate::{
    VM,
    error::RuntimeError,
    native::{NativeFunction, char_lower, char_upper, expect_char},
    value::RuntimeValue,
};

#[inline]
fn expect_char_arg(args: &[RuntimeValue], index: usize) -> Result<char, RuntimeError> {
    match args.get(index) {
        Some(value) => expect_char(value.clone()),
        None => Ok('\0'),
    }
}

pub struct CharLowercase;

impl NativeFunction for CharLowercase {
    fn name(&self) -> String {
        String::from("char.lowercase")
    }

    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let c = expect_char_arg(&args, 0)?;
        Ok(RuntimeValue::Char(char_lower(c)))
    }
}

pub struct CharUppercase;

impl NativeFunction for CharUppercase {
    fn name(&self) -> String {
        String::from("char.uppercase")
    }

    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let c = expect_char_arg(&args, 0)?;
        Ok(RuntimeValue::Char(char_upper(c)))
    }
}
