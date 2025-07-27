use std::{cell::RefCell, rc::Rc};

use rustyline::DefaultEditor;

use crate::{
    native::NativeFunction,
    runtime::{
        interpreter::InterpreterErr,
        scope::{Scope, links::get_link_path},
        values::{RuntimeType, RuntimeValue},
    },
};

pub fn setup(parent: Rc<RefCell<Scope>>) {
    let scope = Scope::new_from_parent(parent, String::from("console"));

    let funcs: Vec<(String, Rc<dyn NativeFunction>)> = vec![
        (String::from("out"), Rc::new(Out())),
        (String::from("err"), Rc::new(ErrFn())),
        (String::from("input"), Rc::new(Input())),
        (String::from("clear"), Rc::new(Clear())),
    ];

    for func in funcs {
        let _ = scope
            .borrow_mut()
            .push_var(
                func.0,
                RuntimeValue::NativeFunction(func.1),
                crate::runtime::values::helper::VarType::Constant,
            )
            .unwrap();
    }
}

pub struct Out();

impl NativeFunction for Out {
    fn run(
        &self,
        args: &[(RuntimeValue, Option<RuntimeValue>)],
        scope: &Rc<RefCell<Scope>>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        use std::io::{self, Write};
        let stdout = io::stdout();
        let mut handle = stdout.lock();

        for arg in args {
            let mut s = arg.0.to_string();

            if let RuntimeValue::Link(path, _) = &arg.0 {
                s = get_link_path(scope, &path)?.to_string();
            }

            handle.write_all(s.as_bytes()).unwrap();
        }
        handle.write_all(b"\n").unwrap();
        handle.flush().unwrap();

        Ok(RuntimeValue::Null)
    }
}

pub struct ErrFn();

impl NativeFunction for ErrFn {
    fn run(
        &self,
        args: &[(RuntimeValue, Option<RuntimeValue>)],
        scope: &Rc<RefCell<Scope>>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        use std::io::{self, Write};
        let stderr = io::stderr();
        let mut handle = stderr.lock();

        for arg in args {
            let mut s = arg.0.to_string();

            if let RuntimeValue::Link(path, _) = &arg.0 {
                s = get_link_path(scope, &path)?.to_string();
            }

            handle.write_all(s.as_bytes()).unwrap();
        }
        handle.write_all(b"\n").unwrap();
        handle.flush().unwrap();

        Ok(RuntimeValue::Null)
    }
}

pub struct Input();

impl NativeFunction for Input {
    fn run(
        &self,
        args: &[(RuntimeValue, Option<RuntimeValue>)],
        scope: &Rc<RefCell<Scope>>,
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
    fn run(
        &self,
        args: &[(RuntimeValue, Option<RuntimeValue>)],
        scope: &Rc<RefCell<Scope>>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        print!("{esc}[2J{esc}[1;1H", esc = 27 as char);
        Ok(RuntimeValue::Null)
    }
}
