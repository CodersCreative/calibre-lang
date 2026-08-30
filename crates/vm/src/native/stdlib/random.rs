use crate::{
    VM,
    error::RuntimeError,
    native::{
        NativeFunction,
        utils::{expect_num_args, resolve_int, resolve_range},
    },
    value::RuntimeValue,
};

pub struct Rand;

impl NativeFunction for Rand {
    fn name(&self) -> String {
        String::from("random.rand")
    }

    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;
        let val = resolve_range(env, &args[0])?;

        Ok(RuntimeValue::Float(
            val.start as f64 + fastrand::f64() * (val.end - val.start) as f64,
        ))
    }
}

pub struct Seed;

impl NativeFunction for Seed {
    fn name(&self) -> String {
        String::from("random.seed")
    }

    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;
        let val = resolve_int(env, &args[0])?;
        fastrand::seed(val as u64);
        Ok(RuntimeValue::Null)
    }
}
