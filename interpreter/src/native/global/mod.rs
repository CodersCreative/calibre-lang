use crate::{native::NativeFunction, runtime::{
        interpreter::InterpreterErr, scope::InterpreterEnvironment, values::{RuntimeType, RuntimeValue}
    }};

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

pub struct Range();

impl NativeFunction for Range {
    fn run(
        &self,
        env: &mut InterpreterEnvironment,
        scope: &u64,
        args: &[(RuntimeValue, Option<RuntimeValue>)],
    ) -> Result<RuntimeValue, InterpreterErr> {
        if args.len() <= 1 {
            let RuntimeValue::Int(amt) = args[0].0.into_type(env, scope, &RuntimeType::Int)? else {
                panic!()
            };

            Ok(RuntimeValue::Range(0, amt as i64))
        } else if args.len() == 2 {
            let RuntimeValue::Int(start) = args[0].0.into_type(env, scope, &RuntimeType::Int)?
            else {
                panic!()
            };

            let RuntimeValue::Int(stop) = args[1].0.into_type(env, scope, &RuntimeType::Int)?
            else {
                panic!()
            };

            Ok(RuntimeValue::Range(start as i64, stop as i64))
        } else if args.len() == 3 {
            let RuntimeValue::Int(start) = args[0].0.into_type(env, scope, &RuntimeType::Int)?
            else {
                panic!()
            };

            let RuntimeValue::Int(stop) = args[1].0.into_type(env, scope, &RuntimeType::Int)?
            else {
                panic!()
            };

            let RuntimeValue::Int(step) = args[2].0.into_type(env, scope, &RuntimeType::Int)?
            else {
                panic!()
            };

            Ok(RuntimeValue::List {
                data: (start..stop)
                    .step_by(step as usize)
                    .map(|x| RuntimeValue::Int(x))
                    .collect(),
                data_type: Box::new(Some(RuntimeType::Int)),
            })
        } else {
            Ok(RuntimeValue::Null)
        }
    }
}

pub struct Len();

impl NativeFunction for Len {
    fn run(
        &self,
        env: &mut InterpreterEnvironment,
        scope: &u64,
        args: &[(RuntimeValue, Option<RuntimeValue>)],
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let Some((x, _)) = args.get(0) {
            Ok(RuntimeValue::Int(match x.unwrap(env, scope)? {
                RuntimeValue::List { data, data_type: _ } => data.len() as i64,
                RuntimeValue::Tuple(data) => data.len() as i64,
                RuntimeValue::Str(x) => x.len() as i64,
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
        env: &mut InterpreterEnvironment,
        scope: &u64,
        args: &[(RuntimeValue, Option<RuntimeValue>)],
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let Some((x, _)) = args.get(0) {
            let RuntimeValue::Str(x) =
                x.unwrap(env, scope)?
                    .into_type(env, scope, &RuntimeType::Str)?
            else {
                panic!()
            };
            Ok(RuntimeValue::Str(x.trim().to_string()))
        } else {
            Ok(RuntimeValue::Null)
        }
    }
}
