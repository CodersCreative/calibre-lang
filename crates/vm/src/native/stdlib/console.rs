use crate::{VM, error::RuntimeError, native::NativeFunction, value::RuntimeValue};
use rustyline::DefaultEditor;

pub struct Out();

impl NativeFunction for Out {
    fn name(&self) -> String {
        String::from("out")
    }

    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        use std::io::{self, Write};
        let stdout = io::stdout();
        let mut handle = stdout.lock();

        for arg in args {
            let s = arg.to_string();

            handle.write_all(s.as_bytes()).unwrap();
        }
        handle.write_all(b"\n").unwrap();
        handle.flush().unwrap();

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
            let s = arg.to_string();

            handle.write_all(s.as_bytes()).unwrap();
        }
        handle.write_all(b"\n").unwrap();
        handle.flush().unwrap();

        Ok(RuntimeValue::Null)
    }
}

pub struct Input();

impl NativeFunction for Input {
    fn name(&self) -> String {
        String::from("input")
    }

    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let mut editor = DefaultEditor::new().unwrap();
        let txt = match args.get(0) {
            Some(x) => x.clone(),
            None => RuntimeValue::Str("".to_string()),
        };

        let readline = editor.readline(&txt.to_string());
        match readline {
            Ok(line) => Ok(RuntimeValue::Option(Some(Box::new(RuntimeValue::Str(
                line,
            ))))),
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
