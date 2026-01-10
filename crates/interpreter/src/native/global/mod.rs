use crate::{
    native::NativeFunction,
    runtime::{
        interpreter::InterpreterErr,
        scope::InterpreterEnvironment,
        values::{RuntimeType, RuntimeValue},
    },
};

pub struct ErrFn();

impl NativeFunction for ErrFn {
    fn run(
        &self,
        _env: &mut InterpreterEnvironment,
        _scope: &u64,
        args: &[(RuntimeValue, Option<RuntimeValue>)],
    ) -> Result<RuntimeValue, InterpreterErr> {
        Ok(if let Some(x) = args.get(0) {
            RuntimeValue::Result(
                Err(Box::new(x.0.clone())),
                RuntimeType::Result(Box::new((&x.0).into()), Box::new(RuntimeType::Dynamic)),
            )
        } else {
            RuntimeValue::Result(
                Err(Box::new(RuntimeValue::Str(String::from("Add parameter")))),
                RuntimeType::Result(Box::new(RuntimeType::Str), Box::new(RuntimeType::Dynamic)),
            )
        })
    }
}

pub struct OkFn();

impl NativeFunction for OkFn {
    fn run(
        &self,
        _env: &mut InterpreterEnvironment,
        _scope: &u64,
        args: &[(RuntimeValue, Option<RuntimeValue>)],
    ) -> Result<RuntimeValue, InterpreterErr> {
        Ok(if let Some(x) = args.get(0) {
            RuntimeValue::Result(
                Ok(Box::new(x.0.clone())),
                RuntimeType::Result(Box::new(RuntimeType::Dynamic), Box::new((&x.0).into())),
            )
        } else {
            RuntimeValue::Result(
                Err(Box::new(RuntimeValue::Str(String::from("Add parameter")))),
                RuntimeType::Result(Box::new(RuntimeType::Str), Box::new(RuntimeType::Dynamic)),
            )
        })
    }
}

pub struct TupleFn();

impl NativeFunction for TupleFn {
    fn run(
        &self,
        _env: &mut InterpreterEnvironment,
        _scope: &u64,
        args: &[(RuntimeValue, Option<RuntimeValue>)],
    ) -> Result<RuntimeValue, InterpreterErr> {
        let mut tple = Vec::new();
        for arg in args {
            tple.push(arg.0.clone());
        }
        Ok(RuntimeValue::Aggregate(None, tple.into()))
    }
}

pub struct SomeFn();

impl NativeFunction for SomeFn {
    fn run(
        &self,
        _env: &mut InterpreterEnvironment,
        _scope: &u64,
        args: &[(RuntimeValue, Option<RuntimeValue>)],
    ) -> Result<RuntimeValue, InterpreterErr> {
        Ok(if let Some(x) = args.get(0) {
            RuntimeValue::Option(
                Some(Box::new(x.0.clone())),
                RuntimeType::Option(Box::new((&x.0).into())),
            )
        } else {
            RuntimeValue::Option(None, RuntimeType::Option(Box::new(RuntimeType::Dynamic)))
        })
    }
}

pub struct PanicFn();

impl NativeFunction for PanicFn {
    fn run(
        &self,
        _env: &mut InterpreterEnvironment,
        _scope: &u64,
        _args: &[(RuntimeValue, Option<RuntimeValue>)],
    ) -> Result<RuntimeValue, InterpreterErr> {
        panic!();
    }
}

pub struct Len();

impl NativeFunction for Len {
    fn run(
        &self,
        _env: &mut InterpreterEnvironment,
        _scope: &u64,
        args: &[(RuntimeValue, Option<RuntimeValue>)],
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let Some((x, _)) = args.get(0) {
            Ok(RuntimeValue::Int(match x {
                RuntimeValue::List { data, data_type: _ } => data.len() as i64,
                RuntimeValue::Aggregate(_, data) => data.len() as i64,
                RuntimeValue::Range(_, x) => *x,
                RuntimeValue::Str(x) => x.len() as i64,
                RuntimeValue::Int(x) => *x,
                RuntimeValue::Float(x) => *x as i64,
                _ => return Err(InterpreterErr::UnexpectedType(x.clone())),
            }))
        } else {
            Ok(RuntimeValue::Null)
        }
    }
}

pub struct Trim();

impl NativeFunction for Trim {
    fn run(
        &self,
        _env: &mut InterpreterEnvironment,
        _scope: &u64,
        args: &[(RuntimeValue, Option<RuntimeValue>)],
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let Some((x, _)) = args.get(0) {
            let RuntimeValue::Str(x) = x else { panic!() };
            Ok(RuntimeValue::Str(x.trim().to_string()))
        } else {
            Ok(RuntimeValue::Null)
        }
    }
}
