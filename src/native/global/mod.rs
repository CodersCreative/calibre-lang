use std::{cell::RefCell, f32::consts::PI, rc::Rc};

use crate::{
    native::{NativeFunction, stdlib::console::Out},
    runtime::{
        interpreter::InterpreterErr,
        scope::Scope,
        values::{RuntimeType, RuntimeValue},
    },
};

pub fn setup(scope: Rc<RefCell<Scope>>) {
    let funcs: Vec<(String, Rc<dyn NativeFunction>)> = vec![
        (String::from("ok"), Rc::new(OkFn())),
        (String::from("err"), Rc::new(ErrFn())),
        (String::from("some"), Rc::new(SomeFn())),
        (String::from("range"), Rc::new(Range())),
        (String::from("trim"), Rc::new(Trim())),
        (String::from("print"), Rc::new(Out())),
    ];

    let mut borrowed_scope = scope.borrow_mut();

    for func in funcs {
        let _ = borrowed_scope
            .push_var(
                func.0,
                RuntimeValue::NativeFunction(func.1),
                crate::runtime::values::helper::VarType::Constant,
            )
            .unwrap();
    }

    let vars = vec![
        (String::from("PI"), RuntimeValue::Float(PI)),
        (String::from("FLOAT_MAX"), RuntimeValue::Float(f32::MAX)),
        (String::from("DOUBLE_MAX"), RuntimeValue::Double(f64::MAX)),
        (String::from("INT_MAX"), RuntimeValue::Int(i64::MAX)),
        (String::from("LONG_MAX"), RuntimeValue::Long(i128::MAX)),
        (String::from("FLOAT_MIN"), RuntimeValue::Float(f32::MIN)),
        (String::from("DOUBLE_MIN"), RuntimeValue::Double(f64::MIN)),
        (String::from("INT_MIN"), RuntimeValue::Int(i64::MIN)),
        (String::from("LONG_MIN"), RuntimeValue::Long(i128::MIN)),
        (String::from("true"), RuntimeValue::Bool(true)),
        (String::from("false"), RuntimeValue::Bool(false)),
    ];

    for var in vars {
        let _ = borrowed_scope
            .push_var(
                var.0,
                var.1,
                crate::runtime::values::helper::VarType::Constant,
            )
            .unwrap();
    }
}

pub struct ErrFn();

impl NativeFunction for ErrFn {
    fn run(
        &self,
        args: &[(RuntimeValue, Option<RuntimeValue>)],
        scope: &Rc<RefCell<Scope>>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        Ok(if let Some(x) = args.get(0) {
            RuntimeValue::Result(
                Err(Box::new(x.0.clone())),
                RuntimeType::Result(Box::new((&x.0).into()), Box::new(RuntimeType::Dynamic)),
            )
        } else {
            RuntimeValue::Result(
                Err(Box::new(RuntimeValue::Str(String::from("Add parameter")))),
                RuntimeType::Result(Box::new(RuntimeType::Str), Box::new(RuntimeType::Dynamic)),
            )
        })
    }
}

pub struct OkFn();

impl NativeFunction for OkFn {
    fn run(
        &self,
        args: &[(RuntimeValue, Option<RuntimeValue>)],
        scope: &Rc<RefCell<Scope>>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        Ok(if let Some(x) = args.get(0) {
            RuntimeValue::Result(
                Ok(Box::new(x.0.clone())),
                RuntimeType::Result(Box::new(RuntimeType::Dynamic), Box::new((&x.0).into())),
            )
        } else {
            RuntimeValue::Result(
                Err(Box::new(RuntimeValue::Str(String::from("Add parameter")))),
                RuntimeType::Result(Box::new(RuntimeType::Str), Box::new(RuntimeType::Dynamic)),
            )
        })
    }
}

pub struct SomeFn();

impl NativeFunction for SomeFn {
    fn run(
        &self,
        args: &[(RuntimeValue, Option<RuntimeValue>)],
        scope: &Rc<RefCell<Scope>>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        Ok(if let Some(x) = args.get(0) {
            RuntimeValue::Option(
                Some(Box::new(x.0.clone())),
                RuntimeType::Option(Box::new((&x.0).into())),
            )
        } else {
            RuntimeValue::Option(None, RuntimeType::Option(Box::new(RuntimeType::Dynamic)))
        })
    }
}

pub struct Range();

impl NativeFunction for Range {
    fn run(
        &self,
        args: &[(RuntimeValue, Option<RuntimeValue>)],
        scope: &Rc<RefCell<Scope>>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if args.len() <= 1 {
            let RuntimeValue::Int(amt) = args[0].0.into_type(&scope, &RuntimeType::Int)? else {
                panic!()
            };

            Ok(RuntimeValue::Range(0, amt as i32))
        } else if args.len() == 2 {
            let RuntimeValue::Int(start) = args[0].0.into_type(&scope, &RuntimeType::Int)? else {
                panic!()
            };

            let RuntimeValue::Int(stop) = args[1].0.into_type(&scope, &RuntimeType::Int)? else {
                panic!()
            };

            Ok(RuntimeValue::Range(start as i32, stop as i32))
        } else if args.len() == 3 {
            let RuntimeValue::Int(start) = args[0].0.into_type(&scope, &RuntimeType::Int)? else {
                panic!()
            };

            let RuntimeValue::Int(stop) = args[1].0.into_type(&scope, &RuntimeType::Int)? else {
                panic!()
            };

            let RuntimeValue::Int(step) = args[2].0.into_type(&scope, &RuntimeType::Int)? else {
                panic!()
            };

            Ok(RuntimeValue::List {
                data: (start..stop)
                    .step_by(step as usize)
                    .map(|x| RuntimeValue::Int(x))
                    .collect(),
                data_type: Box::new(Some(RuntimeType::Int)),
            })
        } else {
            Ok(RuntimeValue::Null)
        }
    }
}

pub struct Trim();

impl NativeFunction for Trim {
    fn run(
        &self,
        args: &[(RuntimeValue, Option<RuntimeValue>)],
        scope: &Rc<RefCell<Scope>>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let Some((RuntimeValue::Str(x), _)) = args.get(0) {
            Ok(RuntimeValue::Str(x.trim().to_string()))
        } else {
            Ok(RuntimeValue::Null)
        }
    }
}
