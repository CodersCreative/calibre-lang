use crate::{
    VM,
    error::RuntimeError,
    native::{
        NativeFunction,
        utils::{expect_num_args, pop_or_null, resolve_str},
    },
    value::RuntimeValue,
};
use blake3::Hasher as Blake3;
use sha2::{Digest, Sha256, Sha512};
use ustr::Ustr;

pub struct Sha256Fn;

impl NativeFunction for Sha256Fn {
    fn name(&self) -> String {
        String::from("crypto.sha256")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let s = resolve_str(env, &pop_or_null(&mut args))?;

        let mut hasher = Sha256::new();
        hasher.update(s.as_bytes());
        let out = hasher.finalize();
        Ok(RuntimeValue::Str(Ustr::from(&hex::encode(out))))
    }
}

pub struct Sha512Fn;

impl NativeFunction for Sha512Fn {
    fn name(&self) -> String {
        String::from("crypto.sha512")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let s = resolve_str(env, &pop_or_null(&mut args))?;

        let mut hasher = Sha512::new();
        hasher.update(s.as_bytes());
        let out = hasher.finalize();
        Ok(RuntimeValue::Str(Ustr::from(&hex::encode(out))))
    }
}

pub struct Blake3Fn;

impl NativeFunction for Blake3Fn {
    fn name(&self) -> String {
        String::from("crypto.blake3")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let s = resolve_str(env, &pop_or_null(&mut args))?;

        let mut hasher = Blake3::new();
        hasher.update(s.as_bytes());
        let out = hasher.finalize();
        Ok(RuntimeValue::Str(Ustr::from(
            &out.to_hex().to_string(),
        )))
    }
}
