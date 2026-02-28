use crate::{VM, error::RuntimeError, native::NativeFunction, value::RuntimeValue};
use dumpster::sync::Gc;
use std::{
    io::{self, Write},
    sync::Arc,
};

pub struct ConsoleOutput();

fn resolve_native_input(
    env: &mut VM,
    mut current: RuntimeValue,
    include_reg_ref: bool,
) -> Result<RuntimeValue, RuntimeError> {
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
                    .cloned()
                    .ok_or(RuntimeError::DanglingRef(format!("#{}", id)))?;
            }
            RuntimeValue::RegRef { frame, reg } if include_reg_ref => {
                current = env.get_reg_value_in_frame(frame, reg).clone();
            }
            _ => break,
        }
    }
    Ok(current)
}

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

fn panic_message_arg(value: &RuntimeValue) -> String {
    match value {
        RuntimeValue::Str(s) => s.to_string(),
        other => format!("{other:?}"),
    }
}

#[inline]
fn resolve_first_arg(
    env: &mut VM,
    args: Vec<RuntimeValue>,
    include_reg_ref: bool,
) -> Result<Option<RuntimeValue>, RuntimeError> {
    args.into_iter()
        .next()
        .map(|value| resolve_native_input(env, value, include_reg_ref).map(Some))
        .unwrap_or(Ok(None))
}

#[inline]
fn missing_parameter_result() -> RuntimeValue {
    RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(Arc::new(String::from(
        "Add parameter",
    ))))))
}

impl NativeFunction for ConsoleOutput {
    fn name(&self) -> String {
        String::from("console_output")
    }

    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let handle_type = match args.first() {
            Some(RuntimeValue::Int(val)) => *val,
            Some(other) => {
                return Err(RuntimeError::UnexpectedType(other.clone()));
            }
            None => {
                return Err(RuntimeError::InvalidFunctionCall);
            }
        };

        let rendered = args
            .into_iter()
            .skip(1)
            .map(|arg| match arg {
                RuntimeValue::Str(value) => unescape_string(value.as_str()),
                other => env.display_value(&other),
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

    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        Ok(match resolve_first_arg(env, args, true)? {
            Some(value) => RuntimeValue::Result(Err(Gc::new(value))),
            None => missing_parameter_result(),
        })
    }
}

pub struct Repr();

impl NativeFunction for Repr {
    fn name(&self) -> String {
        String::from("repr")
    }

    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        Ok(RuntimeValue::Str(Arc::new(args[0].display(env))))
    }
}

pub struct OkFn();

impl NativeFunction for OkFn {
    fn name(&self) -> String {
        String::from("ok")
    }
    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        Ok(match resolve_first_arg(env, args, true)? {
            Some(value) => RuntimeValue::Result(Ok(Gc::new(value))),
            None => missing_parameter_result(),
        })
    }
}

pub struct TupleFn();

impl NativeFunction for TupleFn {
    fn name(&self) -> String {
        String::from("tuple")
    }
    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        Ok(RuntimeValue::Aggregate(
            None,
            Gc::new(crate::value::GcMap(args.into())),
        ))
    }
}

pub struct SomeFn();

impl NativeFunction for SomeFn {
    fn name(&self) -> String {
        String::from("some")
    }
    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        Ok(match resolve_first_arg(env, args, true)? {
            Some(value) => RuntimeValue::Option(Some(Gc::new(value))),
            None => RuntimeValue::Option(None),
        })
    }
}

pub struct PanicFn();

impl NativeFunction for PanicFn {
    fn name(&self) -> String {
        String::from("panic")
    }
    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let msg = args.first().map(panic_message_arg);
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
                let msg = args.get(1).map(panic_message_arg);
                Err(RuntimeError::Panic(msg))
            }
            other => Err(RuntimeError::UnexpectedType(other.clone())),
        }
    }
}

pub struct GenSuspendFn();

impl NativeFunction for GenSuspendFn {
    fn name(&self) -> String {
        String::from("gen_suspend")
    }

    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let value = args.into_iter().next().unwrap_or(RuntimeValue::Null);
        Ok(RuntimeValue::GeneratorSuspend(Box::new(value)))
    }
}

pub struct Len();

impl NativeFunction for Len {
    fn name(&self) -> String {
        String::from("len")
    }

    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        if let Some(current) = resolve_first_arg(env, args, true)? {
            Ok(RuntimeValue::Int(match current {
                RuntimeValue::List(data) => data.as_ref().0.len() as i64,
                RuntimeValue::Aggregate(_, data) => data.as_ref().0.0.len() as i64,
                RuntimeValue::Range(from, to) => (to - from).max(0),
                RuntimeValue::Str(x) => x.len() as i64,
                RuntimeValue::Null => 0,
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
        if let Some(current) = resolve_first_arg(env, args, true)? {
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
        if let Some(current) = resolve_first_arg(env, args, false)? {
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
        if let Some(current) = resolve_first_arg(env, args, true)? {
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
