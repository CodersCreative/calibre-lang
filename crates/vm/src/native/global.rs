use crate::{VM, error::RuntimeError, native::NativeFunction, value::RuntimeValue};

pub struct ErrFn();

impl NativeFunction for ErrFn {
    fn name(&self) -> String {
        String::from("err")
    }

    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        Ok(if let Some(x) = args.get(0) {
            RuntimeValue::Result(Err(Box::new(x.clone())))
        } else {
            RuntimeValue::Result(Err(Box::new(RuntimeValue::Str(String::from(
                "Add parameter",
            )))))
        })
    }
}

pub struct OkFn();

impl NativeFunction for OkFn {
    fn name(&self) -> String {
        String::from("ok")
    }
    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        Ok(if let Some(x) = args.get(0) {
            RuntimeValue::Result(Ok(Box::new(x.clone())))
        } else {
            RuntimeValue::Result(Err(Box::new(RuntimeValue::Str(String::from(
                "Add parameter",
            )))))
        })
    }
}

pub struct TupleFn();

impl NativeFunction for TupleFn {
    fn name(&self) -> String {
        String::from("tuple")
    }
    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
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
    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        Ok(if let Some(x) = args.get(0) {
            RuntimeValue::Option(Some(Box::new(x.clone())))
        } else {
            RuntimeValue::Option(None)
        })
    }
}

pub struct PanicFn();

impl NativeFunction for PanicFn {
    fn name(&self) -> String {
        String::from("panic")
    }
    fn run(&self, _env: &mut VM, _args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        Err(RuntimeError::Panic)
    }
}

pub struct Len();

impl NativeFunction for Len {
    fn name(&self) -> String {
        String::from("len")
    }

    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        if let Some(mut x) = args.get(0) {
            while let RuntimeValue::Ref(r) = x {
                x = env.variables.get(r).ok_or(RuntimeError::DanglingRef(r.clone()))?;
            }
            Ok(RuntimeValue::Int(match x {
                RuntimeValue::List(data) => data.len() as i64,
                RuntimeValue::Aggregate(_, data) => data.len() as i64,
                RuntimeValue::Range(_, x) => *x,
                RuntimeValue::Str(x) => x.len() as i64,
                RuntimeValue::Int(x) => *x,
                RuntimeValue::Float(x) => *x as i64,
                _ => return Err(RuntimeError::UnexpectedType(x.clone())),
            }))
        } else {
            Ok(RuntimeValue::Null)
        }
    }
}

pub struct MinOrZero();

impl NativeFunction for MinOrZero {
    fn name(&self) -> String {
        String::from("min_or_zero")
    }

    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        if let Some(mut x) = args.get(0) {
            while let RuntimeValue::Ref(r) = x {
                x = env.variables.get(r).ok_or(RuntimeError::DanglingRef(r.clone()))?;
            }
            Ok(RuntimeValue::Int(match x {
                RuntimeValue::Range(from, _) => *from,
                _ => 0,
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
    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        if let Some(mut x) = args.get(0) {
            while let RuntimeValue::Ref(r) = x {
                x = env.variables.get(r).ok_or(RuntimeError::DanglingRef(r.clone()))?;
            }
            match x {
                RuntimeValue::Str(s) => Ok(RuntimeValue::Str(s.trim().to_string())),
                other => Err(RuntimeError::UnexpectedType(other.clone())),
            }
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
    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        if let Some(mut x) = args.get(0) {
            while let RuntimeValue::Ref(r) = x {
                x = env.variables.get(r).ok_or(RuntimeError::DanglingRef(r.clone()))?;
            }

            Ok(RuntimeValue::Int(match x {
                RuntimeValue::Enum(_, index, _) => *index as i64,
                RuntimeValue::Option(Some(_)) | RuntimeValue::Result(Ok(_)) => 0 as i64,
                RuntimeValue::Option(None) | RuntimeValue::Result(Err(_)) => 1,
                _ => 0,
            }))
        } else {
            Ok(RuntimeValue::Null)
        }
    }
}
