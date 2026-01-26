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
    fn name(&self) -> String {
        String::from("err")
    }

    fn run(
        &self,
        _env: &mut InterpreterEnvironment,
        _scope: &u64,
        args: &[RuntimeValue],
    ) -> Result<RuntimeValue, InterpreterErr> {
        Ok(if let Some(x) = args.get(0) {
            RuntimeValue::Result(
                Err(Box::new(x.clone())),
                RuntimeType::Result {
                    err: Box::new(x.into()),
                    ok: Box::new(RuntimeType::Dynamic),
                },
            )
        } else {
            RuntimeValue::Result(
                Err(Box::new(RuntimeValue::Str(String::from("Add parameter")))),
                RuntimeType::Result {
                    err: Box::new(RuntimeType::Str),
                    ok: Box::new(RuntimeType::Dynamic),
                },
            )
        })
    }
}

pub struct OkFn();

impl NativeFunction for OkFn {
    fn name(&self) -> String {
        String::from("ok")
    }
    fn run(
        &self,
        _env: &mut InterpreterEnvironment,
        _scope: &u64,
        args: &[RuntimeValue],
    ) -> Result<RuntimeValue, InterpreterErr> {
        Ok(if let Some(x) = args.get(0) {
            RuntimeValue::Result(
                Ok(Box::new(x.clone())),
                RuntimeType::Result {
                    err: Box::new(RuntimeType::Dynamic),
                    ok: Box::new(x.into()),
                },
            )
        } else {
            RuntimeValue::Result(
                Err(Box::new(RuntimeValue::Str(String::from("Add parameter")))),
                RuntimeType::Result {
                    err: Box::new(RuntimeType::Str),
                    ok: Box::new(RuntimeType::Dynamic),
                },
            )
        })
    }
}

pub struct TupleFn();

impl NativeFunction for TupleFn {
    fn name(&self) -> String {
        String::from("tuple")
    }
    fn run(
        &self,
        _env: &mut InterpreterEnvironment,
        _scope: &u64,
        args: &[RuntimeValue],
    ) -> Result<RuntimeValue, InterpreterErr> {
        let mut tple = Vec::new();
        for arg in args {
            tple.push(arg.clone());
        }
        Ok(RuntimeValue::Aggregate(None, tple.into()))
    }
}

pub struct SomeFn();

impl NativeFunction for SomeFn {
    fn name(&self) -> String {
        String::from("some")
    }
    fn run(
        &self,
        _env: &mut InterpreterEnvironment,
        _scope: &u64,
        args: &[RuntimeValue],
    ) -> Result<RuntimeValue, InterpreterErr> {
        Ok(if let Some(x) = args.get(0) {
            RuntimeValue::Option(
                Some(Box::new(x.clone())),
                RuntimeType::Option(Box::new(x.into())),
            )
        } else {
            RuntimeValue::Option(None, RuntimeType::Option(Box::new(RuntimeType::Dynamic)))
        })
    }
}

pub struct PanicFn();

impl NativeFunction for PanicFn {
    fn name(&self) -> String {
        String::from("panic")
    }
    fn run(
        &self,
        _env: &mut InterpreterEnvironment,
        _scope: &u64,
        _args: &[RuntimeValue],
    ) -> Result<RuntimeValue, InterpreterErr> {
        panic!();
    }
}

pub struct Len();

impl NativeFunction for Len {
    fn name(&self) -> String {
        String::from("len")
    }
    fn run(
        &self,
        _env: &mut InterpreterEnvironment,
        _scope: &u64,
        args: &[RuntimeValue],
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let Some(x) = args.get(0) {
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
    fn name(&self) -> String {
        String::from("trim")
    }
    fn run(
        &self,
        _env: &mut InterpreterEnvironment,
        _scope: &u64,
        args: &[RuntimeValue],
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let Some(x) = args.get(0) {
            let RuntimeValue::Str(x) = x else { panic!() };
            Ok(RuntimeValue::Str(x.trim().to_string()))
        } else {
            Ok(RuntimeValue::Null)
        }
    }
}

pub struct DiscriminantFn();

impl NativeFunction for DiscriminantFn {
    fn name(&self) -> String {
        String::from("discriminant")
    }
    fn run(
        &self,
        _env: &mut InterpreterEnvironment,
        _scope: &u64,
        args: &[RuntimeValue],
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let Some(x) = args.get(0) {
            Ok(RuntimeValue::Int(match x {
                RuntimeValue::Enum(_, index, _) => *index as i64,
                RuntimeValue::Option(Some(_), _) | RuntimeValue::Result(Ok(_), _) => 0 as i64,
                RuntimeValue::Option(None, _) | RuntimeValue::Result(Err(_), _) => 1,
                _ => 0,
            }))
        } else {
            Ok(RuntimeValue::Null)
        }
    }
}
