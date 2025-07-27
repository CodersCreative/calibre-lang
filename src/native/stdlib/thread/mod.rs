use std::{
    cell::RefCell,
    path::PathBuf,
    rc::Rc,
    thread::{self},
    time::Duration,
};

use crate::{
    native::NativeFunction,
    runtime::{scope::Scope, values::RuntimeValue},
};
pub fn setup(parent: Rc<RefCell<Scope>>) {
    let scope = Scope::new_from_parent(parent, String::from("thread"));

    let funcs: Vec<(String, Rc<dyn NativeFunction>)> =
        vec![(String::from("wait"), Rc::new(Wait()))];

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
pub struct Wait();

impl NativeFunction for Wait {
    fn run(
        &self,
        args: &[(
            crate::runtime::values::RuntimeValue,
            Option<crate::runtime::values::RuntimeValue>,
        )],
        scope: &std::rc::Rc<std::cell::RefCell<crate::runtime::scope::Scope>>,
    ) -> Result<crate::runtime::values::RuntimeValue, crate::runtime::interpreter::InterpreterErr>
    {
        if let Some((RuntimeValue::Int(x), _)) = args.get(0) {
            thread::sleep(Duration::from_secs(*x as u64));
        } else if let Some((RuntimeValue::Float(x), _)) = args.get(0) {
            thread::sleep(Duration::from_secs_f32(*x));
        }
        Ok(RuntimeValue::Null)
    }
}
