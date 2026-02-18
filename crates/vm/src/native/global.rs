use crate::{VM, error::RuntimeError, native::NativeFunction, value::RuntimeValue};
use dumpster::sync::Gc;
use std::io::{self, Write};

pub struct ConsoleOutput();

fn unescape_string(input: &str) -> String {
    let mut out = String::with_capacity(input.len());
    let mut chars = input.chars();
    while let Some(ch) = chars.next() {
        if ch != '\\' {
            out.push(ch);
            continue;
        }
        match chars.next() {
            Some('n') => out.push('\n'),
            Some('t') => out.push('\t'),
            Some('r') => out.push('\r'),
            Some('\\') => out.push('\\'),
            Some('"') => out.push('"'),
            Some('\'') => out.push('\''),
            Some(other) => {
                out.push('\\');
                out.push(other);
            }
            None => out.push('\\'),
        }
    }
    out
}

impl NativeFunction for ConsoleOutput {
    fn name(&self) -> String {
        String::from("console_output")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let handle_type = match args.get(0) {
            Some(RuntimeValue::Int(val)) => *val,
            Some(other) => {
                return Err(RuntimeError::UnexpectedType(other.clone()));
            }
            None => {
                return Err(RuntimeError::InvalidFunctionCall);
            }
        };
        args.remove(0);

        let rendered = args
            .into_iter()
            .map(|arg| match arg {
                RuntimeValue::Str(value) => unescape_string(value.as_str()),
                other => other.display(env),
            })
            .collect::<String>();

        if env.suppress_output {
            env.captured_output.push_str(&rendered);
        } else if handle_type != 2 {
            let stdout = io::stdout();
            let mut handle = stdout.lock();
            handle
                .write_all(rendered.as_bytes())
                .map_err(|e| RuntimeError::Io(e.to_string()))?;
            handle
                .flush()
                .map_err(|e| RuntimeError::Io(e.to_string()))?;
        } else {
            let stderr = io::stderr();
            let mut handle = stderr.lock();
            handle
                .write_all(rendered.as_bytes())
                .map_err(|e| RuntimeError::Io(e.to_string()))?;
            handle
                .flush()
                .map_err(|e| RuntimeError::Io(e.to_string()))?;
        }

        Ok(RuntimeValue::Null)
    }
}

pub struct ErrFn();

impl NativeFunction for ErrFn {
    fn name(&self) -> String {
        String::from("err")
    }

    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        Ok(if let Some(x) = args.get(0) {
            RuntimeValue::Result(Err(Gc::new(x.clone())))
        } else {
            RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(std::sync::Arc::new(
                String::from("Add parameter"),
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
            RuntimeValue::Result(Ok(Gc::new(x.clone())))
        } else {
            RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(std::sync::Arc::new(
                String::from("Add parameter"),
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
        let mut tple = Vec::with_capacity(args.len());
        for arg in args {
            tple.push(arg);
        }
        Ok(RuntimeValue::Aggregate(
            None,
            Gc::new(crate::value::GcMap(tple.into())),
        ))
    }
}

pub struct SomeFn();

impl NativeFunction for SomeFn {
    fn name(&self) -> String {
        String::from("some")
    }
    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        Ok(if let Some(x) = args.get(0) {
            RuntimeValue::Option(Some(Gc::new(x.clone())))
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
    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let msg = args.first().map(|value| match value {
            RuntimeValue::Str(s) => s.to_string(),
            other => format!("{other:?}"),
        });
        Err(RuntimeError::Panic(msg))
    }
}

pub struct AssertFn();

impl NativeFunction for AssertFn {
    fn name(&self) -> String {
        String::from("assert")
    }

    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let condition = args.first().ok_or(RuntimeError::InvalidFunctionCall)?;
        match condition {
            RuntimeValue::Bool(true) => Ok(RuntimeValue::Null),
            RuntimeValue::Bool(false) => {
                let msg = args.get(1).map(|value| match value {
                    RuntimeValue::Str(s) => s.to_string(),
                    other => format!("{other:?}"),
                });
                Err(RuntimeError::Panic(msg))
            }
            other => Err(RuntimeError::UnexpectedType(other.clone())),
        }
    }
}

pub struct Len();

impl NativeFunction for Len {
    fn name(&self) -> String {
        String::from("len")
    }

    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        if let Some(x) = args.get(0) {
            let mut current = x.clone();
            for _ in 0..64 {
                match current {
                    RuntimeValue::Ref(ref r) => {
                        current = env
                            .variables
                            .get(r)
                            .cloned()
                            .ok_or(RuntimeError::DanglingRef(r.clone()))?;
                    }
                    RuntimeValue::VarRef(id) => {
                        current = env
                            .variables
                            .get_by_id(id)
                            .ok_or(RuntimeError::DanglingRef(format!("#{}", id)))?;
                    }
                    RuntimeValue::RegRef { frame, reg } => {
                        current = env.get_reg_value_in_frame(frame, reg);
                    }
                    _ => break,
                }
            }
            Ok(RuntimeValue::Int(match current {
                RuntimeValue::List(data) => data.as_ref().0.len() as i64,
                RuntimeValue::Aggregate(_, data) => data.as_ref().0.0.len() as i64,
                RuntimeValue::Range(_, x) => x,
                RuntimeValue::Str(x) => x.len() as i64,
                RuntimeValue::HashMap(map) => map.lock().map(|m| m.len() as i64).unwrap_or(0),
                RuntimeValue::HashSet(set) => set.lock().map(|s| s.len() as i64).unwrap_or(0),
                RuntimeValue::Int(x) => x,
                RuntimeValue::Float(x) => x as i64,
                other => return Err(RuntimeError::UnexpectedType(other)),
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
        if let Some(x) = args.get(0) {
            let mut current = x.clone();
            for _ in 0..64 {
                match current {
                    RuntimeValue::Ref(ref r) => {
                        current = env
                            .variables
                            .get(r)
                            .cloned()
                            .ok_or(RuntimeError::DanglingRef(r.clone()))?;
                    }
                    RuntimeValue::VarRef(id) => {
                        current = env
                            .variables
                            .get_by_id(id)
                            .ok_or(RuntimeError::DanglingRef(format!("#{}", id)))?;
                    }
                    _ => break,
                }
            }
            Ok(RuntimeValue::Int(match current {
                RuntimeValue::Range(from, _) => from,
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
        if let Some(x) = args.get(0) {
            let mut current = x.clone();
            for _ in 0..64 {
                match current {
                    RuntimeValue::Ref(ref r) => {
                        current = env
                            .variables
                            .get(r)
                            .cloned()
                            .ok_or(RuntimeError::DanglingRef(r.clone()))?;
                    }
                    RuntimeValue::VarRef(id) => {
                        current = env
                            .variables
                            .get_by_id(id)
                            .ok_or(RuntimeError::DanglingRef(format!("#{}", id)))?;
                    }
                    _ => break,
                }
            }
            match current {
                RuntimeValue::Str(s) => {
                    Ok(RuntimeValue::Str(std::sync::Arc::new(s.trim().to_string())))
                }
                other => Err(RuntimeError::UnexpectedType(other)),
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
        if let Some(x) = args.get(0) {
            let mut current = x.clone();
            for _ in 0..64 {
                match current {
                    RuntimeValue::Ref(ref r) => {
                        current = env
                            .variables
                            .get(r)
                            .cloned()
                            .ok_or(RuntimeError::DanglingRef(r.clone()))?;
                    }
                    RuntimeValue::VarRef(id) => {
                        current = env
                            .variables
                            .get_by_id(id)
                            .ok_or(RuntimeError::DanglingRef(format!("#{}", id)))?;
                    }
                    RuntimeValue::RegRef { frame, reg } => {
                        current = env.get_reg_value_in_frame(frame, reg);
                    }
                    _ => break,
                }
            }

            Ok(RuntimeValue::Int(match current {
                RuntimeValue::Enum(_, index, _) => index as i64,
                RuntimeValue::Option(Some(_)) | RuntimeValue::Result(Ok(_)) => 0 as i64,
                RuntimeValue::Option(None) | RuntimeValue::Result(Err(_)) => 1,
                _ => 0,
            }))
        } else {
            Ok(RuntimeValue::Null)
        }
    }
}
