use std::{
    cell::RefCell,
    path::PathBuf,
    rc::Rc,
    thread::{self},
    time::Duration,
};

use crate::{
    native::NativeFunction,
    runtime::{
        scope::{Environment, Scope, Variable},
        values::RuntimeValue,
    },
};
pub fn setup(env: &mut Environment, parent: &u64) {
    let scope = env.new_scope_from_parent(*parent, "thread");

    let funcs: Vec<(String, Rc<dyn NativeFunction>)> =
        vec![(String::from("wait"), Rc::new(Wait()))];

    if let Some(map) = env.variables.get_mut(&scope) {
        for func in funcs {
            map.insert(
                func.0,
                Variable {
                    value: RuntimeValue::NativeFunction(func.1),
                    var_type: crate::runtime::values::helper::VarType::Constant,
                },
            );
        }
    }
}
pub struct Wait();

impl NativeFunction for Wait {
    fn run(
        &self,
        env: &mut Environment,
        scope: &u64,
        args: &[(
            crate::runtime::values::RuntimeValue,
            Option<crate::runtime::values::RuntimeValue>,
        )],
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
