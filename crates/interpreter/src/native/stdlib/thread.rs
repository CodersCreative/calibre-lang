use calibre_parser::ast::VarType;

use crate::{
    native::NativeFunction,
    runtime::{
        scope::InterpreterEnvironment,
        values::{RuntimeType, RuntimeValue},
    },
};
use std::{
    rc::Rc,
    thread::{self},
    time::Duration,
};

pub struct Wait();

impl NativeFunction for Wait {
    fn run(
        &self,
        _env: &mut InterpreterEnvironment,
        _scope: &u64,
        args: &[(
            crate::runtime::values::RuntimeValue,
            Option<crate::runtime::values::RuntimeValue>,
        )],
    ) -> Result<crate::runtime::values::RuntimeValue, crate::runtime::interpreter::InterpreterErr>
    {
        if let Some((RuntimeValue::Int(x), _)) = args.get(0) {
            thread::sleep(Duration::from_secs(*x as u64));
        } else if let Some((RuntimeValue::Float(x), _)) = args.get(0) {
            thread::sleep(Duration::from_secs_f64(*x));
        }
        Ok(RuntimeValue::Null)
    }
}
