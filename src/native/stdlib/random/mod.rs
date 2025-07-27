use rand::{self, random, random_bool, random_range, random_ratio, seq::IndexedRandom};
use rustyline::DefaultEditor;
use std::{cell::RefCell, rc::Rc};

use crate::{
    native::NativeFunction,
    runtime::{
        interpreter::InterpreterErr,
        scope::{Scope, links::get_link_path},
        values::{RuntimeType, RuntimeValue},
    },
};

pub fn setup(parent: Rc<RefCell<Scope>>) {
    let scope = Scope::new_from_parent(parent, String::from("random"));

    let funcs: Vec<(String, Rc<dyn NativeFunction>)> = vec![
        (String::from("generate"), Rc::new(Generate())),
        (String::from("bool"), Rc::new(Bool())),
        (String::from("ratio"), Rc::new(Ratio())),
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

pub struct Generate();

impl NativeFunction for Generate {
    fn run(
        &self,
        args: &[(RuntimeValue, Option<RuntimeValue>)],
        scope: &Rc<RefCell<Scope>>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if args.is_empty() {
            Ok(RuntimeValue::Float(random_range(0.0..=1.0)))
        } else if args.len() == 1 {
            if let RuntimeValue::Range(start, stop) = args[0].0 {
                Ok(RuntimeValue::Float(random_range(start as f32..stop as f32)))
            } else {
                let RuntimeValue::Float(amt) = args[0].0.into_type(&scope, &RuntimeType::Float)?
                else {
                    panic!()
                };

                Ok(RuntimeValue::Float(random_range(0.0..=amt)))
            }
        } else if args.len() == 2 {
            let RuntimeValue::Float(start) = args[0].0.into_type(&scope, &RuntimeType::Float)?
            else {
                panic!()
            };

            let RuntimeValue::Float(stop) = args[1].0.into_type(&scope, &RuntimeType::Float)?
            else {
                panic!()
            };

            Ok(RuntimeValue::Float(random_range(start..=stop)))
        } else {
            Ok(RuntimeValue::Null)
        }
    }
}

pub struct Bool();

impl NativeFunction for Bool {
    fn run(
        &self,
        args: &[(RuntimeValue, Option<RuntimeValue>)],
        scope: &Rc<RefCell<Scope>>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if args.is_empty() {
            Ok(RuntimeValue::Bool(random_bool(0.5)))
        } else if args.len() == 1 {
            let RuntimeValue::Double(amt) = args[0].0.into_type(&scope, &RuntimeType::Double)?
            else {
                panic!()
            };

            Ok(RuntimeValue::Bool(random_bool(amt)))
        } else {
            Ok(RuntimeValue::Null)
        }
    }
}

pub struct Ratio();

impl NativeFunction for Ratio {
    fn run(
        &self,
        args: &[(RuntimeValue, Option<RuntimeValue>)],
        scope: &Rc<RefCell<Scope>>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if args.is_empty() {
            Ok(RuntimeValue::Bool(random_ratio(1, 2)))
        } else if args.len() == 1 {
            let RuntimeValue::UInt(amt) = args[0].0.into_type(&scope, &RuntimeType::UInt)? else {
                panic!()
            };

            Ok(RuntimeValue::Bool(random_ratio(1, amt as u32)))
        } else if args.len() == 2 {
            let RuntimeValue::UInt(start) = args[0].0.into_type(&scope, &RuntimeType::UInt)? else {
                panic!()
            };

            let RuntimeValue::UInt(stop) = args[1].0.into_type(&scope, &RuntimeType::UInt)? else {
                panic!()
            };

            if start > stop {
                Ok(RuntimeValue::Bool(random_ratio(stop as u32, start as u32)))
            } else {
                Ok(RuntimeValue::Bool(random_ratio(start as u32, stop as u32)))
            }
        } else {
            Ok(RuntimeValue::Null)
        }
    }
}
