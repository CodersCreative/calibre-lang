use crate::{VM, error::RuntimeError, native::NativeFunction, value::RuntimeValue};
use rustyline::DefaultEditor;

pub struct Out();

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

impl NativeFunction for Out {
    fn name(&self) -> String {
        String::from("out")
    }

    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        use std::io::{self, Write};
        let stdout = io::stdout();
        let mut handle = stdout.lock();

        for arg in args {
            let s = match arg {
                RuntimeValue::Str(value) => unescape_string(&value),
                other => other.to_string(),
            };

            handle
                .write_all(s.as_bytes())
                .map_err(|e| RuntimeError::Io(e.to_string()))?;
        }
        handle
            .write_all(b"\n")
            .map_err(|e| RuntimeError::Io(e.to_string()))?;
        handle
            .flush()
            .map_err(|e| RuntimeError::Io(e.to_string()))?;

        Ok(RuntimeValue::Null)
    }
}

pub struct ErrFn();

impl NativeFunction for ErrFn {
    fn name(&self) -> String {
        String::from("err")
    }

    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        use std::io::{self, Write};
        let stderr = io::stderr();
        let mut handle = stderr.lock();

        for arg in args {
            let s = match arg {
                RuntimeValue::Str(value) => unescape_string(&value),
                other => other.to_string(),
            };

            handle
                .write_all(s.as_bytes())
                .map_err(|e| RuntimeError::Io(e.to_string()))?;
        }
        handle
            .write_all(b"\n")
            .map_err(|e| RuntimeError::Io(e.to_string()))?;
        handle
            .flush()
            .map_err(|e| RuntimeError::Io(e.to_string()))?;

        Ok(RuntimeValue::Null)
    }
}

pub struct Input();

impl NativeFunction for Input {
    fn name(&self) -> String {
        String::from("input")
    }

    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let mut editor = DefaultEditor::new().map_err(|e| RuntimeError::Io(e.to_string()))?;
        let txt = match args.get(0) {
            Some(x) => x.clone(),
            None => RuntimeValue::Str("".to_string()),
        };

        let readline = editor.readline(&txt.to_string());
        match readline {
            Ok(line) => Ok(RuntimeValue::Option(Some(dumpster::sync::Gc::new(
                RuntimeValue::Str(line),
            )))),
            _ => Ok(RuntimeValue::Option(None)),
        }
    }
}

pub struct Clear();

impl NativeFunction for Clear {
    fn name(&self) -> String {
        String::from("clear")
    }

    fn run(&self, _env: &mut VM, _args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        print!("{esc}[2J{esc}[1;1H", esc = 27 as char);
        Ok(RuntimeValue::Null)
    }
}
