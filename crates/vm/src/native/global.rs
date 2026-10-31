use crate::{
    VM,
    error::RuntimeError,
    native::{
        NativeFunction,
        utils::{
            expect_num_args, first_or_null, panic_message_arg, pop_or_null, resolve_int,
            resolve_str,
        },
    },
    value::{RuntimeValue, ValueSlot},
};
use calibre_parser::ast::types::ParserInnerType;
use std::{
    io::{self, BufRead, Write},
    time::Duration,
};
use ustr::Ustr;
use wasm_thread as thread;

pub struct ConsoleOutput;

impl NativeFunction for ConsoleOutput {
    fn name(&self) -> String {
        String::from("console_output")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let text = resolve_str(env, pop_or_null(&mut args))?;
        let handle_type = resolve_int(env, pop_or_null(&mut args))?;

        if env.suppress_output {
            env.captured_output.push(text);
        } else if handle_type == 2 {
            let stderr = io::stderr();
            let mut handle = stderr.lock();

            handle
                .write_all(text.as_bytes())
                .map_err(|e| RuntimeError::Io(e.to_string()))?;
            handle
                .flush()
                .map_err(|e| RuntimeError::Io(e.to_string()))?;
        } else {
            let stdout = io::stdout();
            let mut handle = stdout.lock();

            handle
                .write_all(text.as_bytes())
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

    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[0])?;

        if !env.input_buffer.is_empty() {
            let input = env.input_buffer.remove(0);
            return Ok(RuntimeValue::Str(Ustr::from(&input)));
        }

        let stdin = io::stdin();
        let mut handle = stdin.lock();
        let mut line = String::new();

        match handle.read_line(&mut line) {
            Ok(0) => Ok(RuntimeValue::Str(Ustr::default())),
            Ok(_) => Ok(RuntimeValue::Str(Ustr::from(line.trim()))),
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

        Ok(RuntimeValue::Result(Err(ValueSlot::new(
            env.resolve_value(pop_or_null(&mut args))?,
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

        Ok(RuntimeValue::Result(Ok(ValueSlot::new(
            env.resolve_value(pop_or_null(&mut args))?,
        ))))
    }
}

pub struct Wait;

impl NativeFunction for Wait {
    fn name(&self) -> String {
        String::from("wait")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;
        let val = resolve_int(env, pop_or_null(&mut args))? as u64;
        thread::sleep(Duration::from_millis(val));

        Ok(RuntimeValue::Null)
    }
}

pub struct SomeFn;

impl NativeFunction for SomeFn {
    fn name(&self) -> String {
        String::from("some")
    }
    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        Ok(RuntimeValue::Option(Some(ValueSlot::new(
            env.resolve_value(pop_or_null(&mut args))?,
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

        match env.resolve_value(first_or_null(&mut args))? {
            RuntimeValue::Option(Some(_))
            | RuntimeValue::Result(Ok(_))
            | RuntimeValue::Bool(true) => Ok(RuntimeValue::Null),
            RuntimeValue::Option(None)
            | RuntimeValue::Result(Err(_))
            | RuntimeValue::Bool(false) => {
                let msg = args.first().map(panic_message_arg);
                Err(RuntimeError::Panic(msg))
            }
            other => Err(RuntimeError::UnexpectedTypeInConversion {
                value: Box::new(other.clone()),
                target_type: ParserInnerType::Bool,
            }),
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
            match env.resolve_value(pop_or_null(&mut args))? {
                RuntimeValue::List(data) => data.0.len(),
                RuntimeValue::Aggregate(_, data) => data.as_ref().0.0.len(),
                RuntimeValue::Range(from, to) => (to - from).max(0).unsigned_abs(),
                RuntimeValue::Str(x) => x.len(),
                RuntimeValue::Null => 0,
                RuntimeValue::HashMap(map) => map.map.len(),
                RuntimeValue::HashSet(set) => set.set.len(),
                RuntimeValue::Int(x) => x as usize,
                RuntimeValue::UInt(x) => x,
                RuntimeValue::Float(x) => x as usize,
                other => {
                    return Err(RuntimeError::UnexpectedTypeInConversion {
                        value: Box::new(other),
                        target_type: ParserInnerType::Str,
                    });
                }
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
            match env.resolve_value(pop_or_null(&mut args))? {
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

        match env.resolve_value(pop_or_null(&mut args))? {
            RuntimeValue::Str(s) => Ok(RuntimeValue::Str(Ustr::from(s.trim()))),
            other => Err(RuntimeError::UnexpectedTypeInConversion {
                value: Box::new(other),
                target_type: ParserInnerType::Str,
            }),
        }
    }
}
