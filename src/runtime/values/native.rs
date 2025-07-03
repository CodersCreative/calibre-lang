use std::{cell::RefCell, rc::Rc, thread, time::Duration};

use rustyline::DefaultEditor;

use crate::runtime::{
    scope::Scope,
    values::{RuntimeType, RuntimeValue},
};

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub enum NativeFunctions {
    Print,
    Input,
    Trim,
    Range,
    Wait,
}

impl NativeFunctions {}

impl RuntimeValue {
    pub fn call_native(
        &self,
        args: Vec<(RuntimeValue, Option<RuntimeValue>)>,
        scope: Rc<RefCell<Scope>>,
    ) -> RuntimeValue {
        if let Self::NativeFunction(func) = self {
            match func {
                NativeFunctions::Print => {
                    use std::io::{self, Write};
                    let stdout = io::stdout();
                    let mut handle = stdout.lock();

                    for arg in args {
                        let s = arg.0.to_string();
                        handle.write_all(s.as_bytes()).unwrap();
                    }
                    handle.write_all(b"\n").unwrap();
                    handle.flush().unwrap();

                    RuntimeValue::Null
                }
                NativeFunctions::Range => {
                    if args.len() <= 1 {
                        let RuntimeValue::Integer(amt) = args[0].0 else {
                            panic!()
                        };
                        RuntimeValue::Range(0, amt as i32)
                    } else if args.len() == 2 {
                        let RuntimeValue::Integer(start) = args[0].0 else {
                            panic!()
                        };
                        let RuntimeValue::Integer(stop) = args[1].0 else {
                            panic!()
                        };
                        RuntimeValue::Range(start as i32, stop as i32)
                    } else if args.len() == 3 {
                        let RuntimeValue::Integer(start) = args[0].0 else {
                            panic!()
                        };
                        let RuntimeValue::Integer(stop) = args[1].0 else {
                            panic!()
                        };
                        let RuntimeValue::Integer(step) = args[2].0 else {
                            panic!()
                        };
                        RuntimeValue::List {
                            data: (start..stop)
                                .step_by(step as usize)
                                .map(|x| RuntimeValue::Integer(x))
                                .collect(),
                            data_type: Box::new(Some(RuntimeType::Integer)),
                        }
                    } else {
                        RuntimeValue::Null
                    }
                }
                NativeFunctions::Wait => {
                    if let Some((RuntimeValue::Integer(x), _)) = args.get(0) {
                        thread::sleep(Duration::from_secs(*x as u64));
                    } else if let Some((RuntimeValue::Float(x), _)) = args.get(0) {
                        thread::sleep(Duration::from_secs_f64(*x));
                    }
                    RuntimeValue::Null
                }
                NativeFunctions::Trim => {
                    if let Some((RuntimeValue::Str(x), _)) = args.get(0) {
                        RuntimeValue::Str(x.trim().to_string())
                    } else {
                        RuntimeValue::Null
                    }
                }
                NativeFunctions::Input => {
                    let mut editor = DefaultEditor::new().unwrap();
                    let txt = match args.get(0) {
                        Some(x) => x.0.clone(),
                        None => RuntimeValue::Str("".to_string()),
                    };

                    let readline = editor.readline(&txt.to_string());
                    match readline {
                        Ok(line) => RuntimeValue::Str(line),
                        Err(_) => RuntimeValue::Null,
                    }
                }
            }
        } else {
            panic!();
        }
    }
}
