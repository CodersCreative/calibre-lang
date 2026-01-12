use crate::{
    native::NativeFunction,
    runtime::{
        interpreter::InterpreterErr,
        scope::InterpreterEnvironment,
        values::{RuntimeType, RuntimeValue},
    },
};
use rustyline::DefaultEditor;

pub struct Out();

impl NativeFunction for Out {
    fn name(&self) -> String {
        String::from("out")
    }
    fn run(
        &self,
        _env: &mut InterpreterEnvironment,
        _scope: &u64,
        args: &[(RuntimeValue, Option<RuntimeValue>)],
    ) -> Result<RuntimeValue, InterpreterErr> {
        use std::io::{self, Write};
        let stdout = io::stdout();
        let mut handle = stdout.lock();

        for arg in args {
            let s = arg.0.to_string();

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
    fn run(
        &self,
        _env: &mut InterpreterEnvironment,
        _scope: &u64,
        args: &[(RuntimeValue, Option<RuntimeValue>)],
    ) -> Result<RuntimeValue, InterpreterErr> {
        use std::io::{self, Write};
        let stderr = io::stderr();
        let mut handle = stderr.lock();

        for arg in args {
            let s = arg.0.to_string();

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
    fn run(
        &self,
        _env: &mut InterpreterEnvironment,
        _scope: &u64,
        args: &[(RuntimeValue, Option<RuntimeValue>)],
    ) -> Result<RuntimeValue, InterpreterErr> {
        let mut editor = DefaultEditor::new().unwrap();
        let txt = match args.get(0) {
            Some(x) => x.0.clone(),
            None => RuntimeValue::Str("".to_string()),
        };

        let readline = editor.readline(&txt.to_string());
        match readline {
            Ok(line) => Ok(RuntimeValue::Option(
                Some(Box::new(RuntimeValue::Str(line))),
                RuntimeType::Str,
            )),
            _ => Ok(RuntimeValue::Option(None, RuntimeType::Str)),
        }
    }
}

pub struct Clear();

impl NativeFunction for Clear {
    fn name(&self) -> String {
        String::from("clear")
    }
    fn run(
        &self,
        _env: &mut InterpreterEnvironment,
        _scope: &u64,
        _args: &[(RuntimeValue, Option<RuntimeValue>)],
    ) -> Result<RuntimeValue, InterpreterErr> {
        print!("{esc}[2J{esc}[1;1H", esc = 27 as char);
        Ok(RuntimeValue::Null)
    }
}
