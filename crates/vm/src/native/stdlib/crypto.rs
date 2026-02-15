use crate::{VM, error::RuntimeError, native::NativeFunction, value::RuntimeValue};
use blake3::Hasher as Blake3;
use sha2::{Digest, Sha256, Sha512};

pub struct Sha256Fn;

impl NativeFunction for Sha256Fn {
    fn name(&self) -> String {
        String::from("crypto.sha256")
    }

    fn run(
        &self,
        _env: &mut VM,
        mut args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let input = args.pop().unwrap_or(RuntimeValue::Null);
        let RuntimeValue::Str(s) = input else {
            return Err(RuntimeError::UnexpectedType(input));
        };
        let mut hasher = Sha256::new();
        hasher.update(s.as_bytes());
        let out = hasher.finalize();
        Ok(RuntimeValue::Str(std::sync::Arc::new(hex::encode(out))))
    }
}

pub struct Sha512Fn;

impl NativeFunction for Sha512Fn {
    fn name(&self) -> String {
        String::from("crypto.sha512")
    }

    fn run(
        &self,
        _env: &mut VM,
        mut args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let input = args.pop().unwrap_or(RuntimeValue::Null);
        let RuntimeValue::Str(s) = input else {
            return Err(RuntimeError::UnexpectedType(input));
        };
        let mut hasher = Sha512::new();
        hasher.update(s.as_bytes());
        let out = hasher.finalize();
        Ok(RuntimeValue::Str(std::sync::Arc::new(hex::encode(out))))
    }
}

pub struct Blake3Fn;

impl NativeFunction for Blake3Fn {
    fn name(&self) -> String {
        String::from("crypto.blake3")
    }

    fn run(
        &self,
        _env: &mut VM,
        mut args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let input = args.pop().unwrap_or(RuntimeValue::Null);
        let RuntimeValue::Str(s) = input else {
            return Err(RuntimeError::UnexpectedType(input));
        };
        let mut hasher = Blake3::new();
        hasher.update(s.as_bytes());
        let out = hasher.finalize();
        Ok(RuntimeValue::Str(std::sync::Arc::new(
            out.to_hex().to_string(),
        )))
    }
}
