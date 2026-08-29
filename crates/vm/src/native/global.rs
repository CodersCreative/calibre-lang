use crate::{
    VM,
    error::RuntimeError,
    native::{
        NativeFunction,
        utils::{expect_num_args, first_or_null, panic_message_arg, pop_or_null, resolve_int},
    },
    value::{GcMap, RuntimeValue},
};
use dumpster::sync::Gc;
use std::{
    io::{self, BufRead, Write},
    sync::Arc,
};
use wasm_lite_std::Mutex;

pub struct ConsoleOutput;

impl NativeFunction for ConsoleOutput {
    fn name(&self) -> String {
        String::from("console_output")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let handle_type = resolve_int(env, &first_or_null(&mut args))?;

        let rendered = args
            .into_iter()
            .map(|arg| match arg {
                RuntimeValue::Str(value) => {
                    calibre_parser::parse::util::unescape_string(&value.lock_sync())
                }
                other => other.display(env),
            })
            .collect::<String>();

        if env.suppress_output {
            env.captured_output.push_str(&rendered);
        } else if handle_type == 2 {
            let stderr = io::stderr();
            let mut handle = stderr.lock();

            handle
                .write_all(rendered.as_bytes())
                .map_err(|e| RuntimeError::Io(e.to_string()))?;
            handle
                .flush()
                .map_err(|e| RuntimeError::Io(e.to_string()))?;
        } else {
            let stdout = io::stdout();
            let mut handle = stdout.lock();

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

pub struct ConsoleInput;

impl NativeFunction for ConsoleInput {
    fn name(&self) -> String {
        String::from("console_input")
    }

    fn run(&self, env: &mut VM, _args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        if !env.input_buffer.is_empty() {
            let input = env.input_buffer.remove(0);
            return Ok(RuntimeValue::Str(Arc::new(Mutex::new(input))));
        }

        let stdin = io::stdin();
        let mut handle = stdin.lock();
        let mut line = String::new();

        match handle.read_line(&mut line) {
            Ok(0) => Ok(RuntimeValue::Str(Arc::new(Mutex::new(String::new())))),
            Ok(_) => Ok(RuntimeValue::Str(Arc::new(Mutex::new(
                line.trim().to_string(),
            )))),
            Err(e) => Err(RuntimeError::Io(e.to_string())),
        }
    }
}

pub struct ErrFn;

impl NativeFunction for ErrFn {
    fn name(&self) -> String {
        String::from("err")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        Ok(RuntimeValue::Result(Err(Gc::new(
            env.resolve_value_for_op_ref(&pop_or_null(&mut args))?,
        ))))
    }
}

pub struct Repr;

impl NativeFunction for Repr {
    fn name(&self) -> String {
        String::from("repr")
    }

    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        Ok(RuntimeValue::Str(Arc::new(Mutex::new(args[0].repr(env)))))
    }
}

pub struct Display;

impl NativeFunction for Display {
    fn name(&self) -> String {
        String::from("display")
    }

    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        Ok(RuntimeValue::Str(Arc::new(Mutex::new(
            args[0].display(env),
        ))))
    }
}

pub struct OkFn;

impl NativeFunction for OkFn {
    fn name(&self) -> String {
        String::from("ok")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        Ok(RuntimeValue::Result(Ok(Gc::new(
            env.resolve_value_for_op_ref(&pop_or_null(&mut args))?,
        ))))
    }
}

pub struct TupleFn;

impl NativeFunction for TupleFn {
    fn name(&self) -> String {
        String::from("tuple")
    }

    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        Ok(RuntimeValue::Aggregate(None, Gc::new(GcMap(args.into()))))
    }
}

pub struct SomeFn;

impl NativeFunction for SomeFn {
    fn name(&self) -> String {
        String::from("some")
    }
    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        Ok(RuntimeValue::Option(Some(Gc::new(
            env.resolve_value_for_op_ref(&pop_or_null(&mut args))?,
        ))))
    }
}

pub struct PanicFn;

impl NativeFunction for PanicFn {
    fn name(&self) -> String {
        String::from("panic")
    }
    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let msg = args.first().map(panic_message_arg);
        Err(RuntimeError::Panic(msg))
    }
}

pub struct AssertFn;

impl NativeFunction for AssertFn {
    fn name(&self) -> String {
        String::from("assert")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1, 2])?;

        match env.resolve_value_for_op_ref(&first_or_null(&mut args))? {
            RuntimeValue::Option(Some(_))
            | RuntimeValue::Result(Ok(_))
            | RuntimeValue::Bool(true) => Ok(RuntimeValue::Null),
            RuntimeValue::Option(None)
            | RuntimeValue::Result(Err(_))
            | RuntimeValue::Bool(false) => {
                let msg = args.first().map(panic_message_arg);
                Err(RuntimeError::Panic(msg))
            }
            other => Err(RuntimeError::UnexpectedType(Box::new(other.clone()))),
        }
    }
}

pub struct Len;

impl NativeFunction for Len {
    fn name(&self) -> String {
        String::from("len")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        Ok(RuntimeValue::UInt(
            match env.resolve_value_for_op_ref(&pop_or_null(&mut args))? {
                RuntimeValue::List(data) => data.as_ref().0.len() as u64,
                RuntimeValue::Aggregate(_, data) => data.as_ref().0.0.len() as u64,
                RuntimeValue::Range(from, to) => (to - from).max(0).unsigned_abs(),
                RuntimeValue::Str(x) => x.lock_sync().len() as u64,
                RuntimeValue::Null => 0,
                RuntimeValue::HashMap(map) => map.lock_sync().len() as u64,
                RuntimeValue::HashSet(set) => set.lock_sync().len() as u64,
                RuntimeValue::Int(x) => x as u64,
                RuntimeValue::UInt(x) => x,
                RuntimeValue::Float(x) => x as u64,
                other => return Err(RuntimeError::UnexpectedType(Box::new(other))),
            },
        ))
    }
}

pub struct MinOrZero;

impl NativeFunction for MinOrZero {
    fn name(&self) -> String {
        String::from("min_or_zero")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        Ok(RuntimeValue::Int(
            match env.resolve_value_for_op_ref(&pop_or_null(&mut args))? {
                RuntimeValue::Range(from, _) => from,
                _ => 0,
            },
        ))
    }
}

pub struct Trim;

impl NativeFunction for Trim {
    fn name(&self) -> String {
        String::from("trim")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        match env.resolve_value_for_op_ref(&pop_or_null(&mut args))? {
            RuntimeValue::Str(s) => Ok(RuntimeValue::Str(Arc::new(Mutex::new(
                s.lock_sync().trim().to_string(),
            )))),
            other => Err(RuntimeError::UnexpectedType(Box::new(other))),
        }
    }
}

pub struct TrimStart;

impl NativeFunction for TrimStart {
    fn name(&self) -> String {
        String::from("trim_start")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        match env.resolve_value_for_op_ref(&pop_or_null(&mut args))? {
            RuntimeValue::Str(s) => Ok(RuntimeValue::Str(Arc::new(Mutex::new(
                s.lock_sync().trim_start().to_string(),
            )))),
            other => Err(RuntimeError::UnexpectedType(Box::new(other))),
        }
    }
}

pub struct TrimEnd;

impl NativeFunction for TrimEnd {
    fn name(&self) -> String {
        String::from("trim_end")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        match env.resolve_value_for_op_ref(&pop_or_null(&mut args))? {
            RuntimeValue::Str(s) => Ok(RuntimeValue::Str(Arc::new(Mutex::new(
                s.lock_sync().trim_end().to_string(),
            )))),
            other => Err(RuntimeError::UnexpectedType(Box::new(other))),
        }
    }
}

pub struct IsWhitespace;

impl NativeFunction for IsWhitespace {
    fn name(&self) -> String {
        String::from("is_whitespace")
    }
    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        match env.resolve_value_for_op_ref(&pop_or_null(&mut args))? {
            RuntimeValue::Str(s) => Ok(RuntimeValue::Bool(
                s.lock_sync().chars().all(|c| c.is_whitespace()),
            )),
            RuntimeValue::Char(c) => Ok(RuntimeValue::Bool(c.is_whitespace())),
            other => Err(RuntimeError::UnexpectedType(Box::new(other))),
        }
    }
}

pub struct DiscriminantFn;

impl NativeFunction for DiscriminantFn {
    fn name(&self) -> String {
        String::from("discriminant")
    }
    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        Ok(RuntimeValue::Int(
            match env.resolve_value_for_op_ref(&pop_or_null(&mut args))? {
                RuntimeValue::Enum(_, index, _) => index as i64,
                RuntimeValue::Option(Some(_)) | RuntimeValue::Result(Ok(_)) => 0,
                RuntimeValue::Option(None) | RuntimeValue::Result(Err(_)) => 1,
                _ => 0,
            },
        ))
    }
}
