use std::{cell::RefCell, rc::Rc, thread, time::Duration};

use rustyline::DefaultEditor;

use crate::runtime::{
    scope::Scope,
    values::{RuntimeType, RuntimeValue},
};

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub enum NativeFunctions {
    Print,
    Some,
    Err,
    Ok,
    Input,
    Trim,
    Range,
    Wait,
    Clear,
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
                NativeFunctions::Err => {
                    if let Some(x) = args.get(0) {
                        RuntimeValue::Result(
                            Err(Box::new(x.0.clone())),
                            RuntimeType::Result(
                                Box::new((&x.0).into()),
                                Box::new(RuntimeType::Str),
                            ),
                        )
                    } else {
                        RuntimeValue::Result(
                            Err(Box::new(RuntimeValue::Str(String::from("Add parameter")))),
                            RuntimeType::Result(
                                Box::new(RuntimeType::Str),
                                Box::new(RuntimeType::Str),
                            ),
                        )
                    }
                }
                NativeFunctions::Ok => {
                    if let Some(x) = args.get(0) {
                        RuntimeValue::Result(
                            Ok(Box::new(x.0.clone())),
                            RuntimeType::Result(
                                Box::new(RuntimeType::Str),
                                Box::new((&x.0).into()),
                            ),
                        )
                    } else {
                        RuntimeValue::Result(
                            Err(Box::new(RuntimeValue::Str(String::from("Add parameter")))),
                            RuntimeType::Result(
                                Box::new(RuntimeType::Str),
                                Box::new(RuntimeType::Str),
                            ),
                        )
                    }
                }
                NativeFunctions::Some => {
                    if let Some(x) = args.get(0) {
                        RuntimeValue::Option(
                            Some(Box::new(x.0.clone())),
                            RuntimeType::Option(Box::new((&x.0).into())),
                        )
                    } else {
                        RuntimeValue::Option(None, RuntimeType::Option(Box::new(RuntimeType::Str)))
                    }
                }
                NativeFunctions::Clear => {
                    print!("{esc}[2J{esc}[1;1H", esc = 27 as char);
                    RuntimeValue::Null
                }
                NativeFunctions::Range => {
                    if args.len() <= 1 {
                        let RuntimeValue::Int(amt) = args[0].0 else {
                            panic!()
                        };
                        RuntimeValue::Range(0, amt as i32)
                    } else if args.len() == 2 {
                        let RuntimeValue::Int(start) = args[0].0 else {
                            panic!()
                        };
                        let RuntimeValue::Int(stop) = args[1].0 else {
                            panic!()
                        };
                        RuntimeValue::Range(start as i32, stop as i32)
                    } else if args.len() == 3 {
                        let RuntimeValue::Int(start) = args[0].0 else {
                            panic!()
                        };
                        let RuntimeValue::Int(stop) = args[1].0 else {
                            panic!()
                        };
                        let RuntimeValue::Int(step) = args[2].0 else {
                            panic!()
                        };
                        RuntimeValue::List {
                            data: (start..stop)
                                .step_by(step as usize)
                                .map(|x| RuntimeValue::Int(x))
                                .collect(),
                            data_type: Box::new(Some(RuntimeType::Int)),
                        }
                    } else {
                        RuntimeValue::Null
                    }
                }
                NativeFunctions::Wait => {
                    if let Some((RuntimeValue::Int(x), _)) = args.get(0) {
                        thread::sleep(Duration::from_secs(*x as u64));
                    } else if let Some((RuntimeValue::Float(x), _)) = args.get(0) {
                        thread::sleep(Duration::from_secs_f32(*x));
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
                        Ok(line) => RuntimeValue::Option(
                            Some(Box::new(RuntimeValue::Str(line))),
                            RuntimeType::Str,
                        ),
                        Err(_) => RuntimeValue::Option(None, RuntimeType::Str),
                    }
                }
            }
        } else {
            panic!();
        }
    }
}
