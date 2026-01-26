use crate::{
    native::NativeFunction,
    runtime::{interpreter::InterpreterErr, scope::InterpreterEnvironment, values::RuntimeValue},
};
use rand::{self, random_bool, random_range, random_ratio};

pub struct Generate();

impl NativeFunction for Generate {
    fn name(&self) -> String {
        String::from("generate")
    }
    fn run(
        &self,
        _env: &mut InterpreterEnvironment,
        _scope: &u64,
        args: &[RuntimeValue],
    ) -> Result<RuntimeValue, InterpreterErr> {
        if args.is_empty() {
            Ok(RuntimeValue::Float(random_range(0.0..=1.0)))
        } else if args.len() == 1 {
            if let RuntimeValue::Range(start, stop) = args[0] {
                Ok(RuntimeValue::Float(random_range(start as f64..stop as f64)))
            } else {
                let RuntimeValue::Float(amt) = args[0] else {
                    panic!()
                };

                Ok(RuntimeValue::Float(random_range(0.0..=amt)))
            }
        } else if args.len() == 2 {
            let RuntimeValue::Float(start) = args[0] else {
                panic!()
            };

            let RuntimeValue::Float(stop) = args[1] else {
                panic!()
            };

            Ok(RuntimeValue::Float(random_range(start..=stop)))
        } else {
            Ok(RuntimeValue::Null)
        }
    }
}

pub struct Bool();

impl NativeFunction for Bool {
    fn name(&self) -> String {
        String::from("bool")
    }
    fn run(
        &self,
        _env: &mut InterpreterEnvironment,
        _scope: &u64,
        args: &[RuntimeValue],
    ) -> Result<RuntimeValue, InterpreterErr> {
        if args.is_empty() {
            Ok(RuntimeValue::Bool(random_bool(0.5)))
        } else if args.len() == 1 {
            let RuntimeValue::Float(amt) = args[0] else {
                panic!()
            };

            Ok(RuntimeValue::Bool(random_bool(amt)))
        } else {
            Ok(RuntimeValue::Null)
        }
    }
}

pub struct Ratio();

impl NativeFunction for Ratio {
    fn name(&self) -> String {
        String::from("ratio")
    }
    fn run(
        &self,
        _env: &mut InterpreterEnvironment,
        _scope: &u64,
        args: &[RuntimeValue],
    ) -> Result<RuntimeValue, InterpreterErr> {
        if args.is_empty() {
            Ok(RuntimeValue::Bool(random_ratio(1, 2)))
        } else if args.len() == 1 {
            let RuntimeValue::Int(amt) = args[0] else {
                panic!()
            };

            Ok(RuntimeValue::Bool(random_ratio(1, amt as u32)))
        } else if args.len() == 2 {
            let RuntimeValue::Int(start) = args[0] else {
                panic!()
            };

            let RuntimeValue::Int(stop) = args[1] else {
                panic!()
            };

            if start > stop {
                Ok(RuntimeValue::Bool(random_ratio(stop as u32, start as u32)))
            } else {
                Ok(RuntimeValue::Bool(random_ratio(start as u32, stop as u32)))
            }
        } else {
            Ok(RuntimeValue::Null)
        }
    }
}
