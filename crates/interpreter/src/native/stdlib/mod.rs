
use crate::runtime::scope::InterpreterEnvironment;
use crate::runtime::values::RuntimeValue;
use calibre_common::environment::{RuntimeValue as Value, Variable};
use calibre_parser::ast::VarType;

pub mod console;
pub mod random;
pub mod thread;

pub fn setup(env: &mut InterpreterEnvironment, scope: &u64) {
    setup_scope(env, scope, "thread", &["wait"]);
    setup_scope(env, scope, "console", &["out", "input", "err", "clear"]);
    setup_scope(env, scope, "random", &["generate", "bool", "ratio"]);
}

pub fn setup_scope(
    env: &mut InterpreterEnvironment,
    parent: &u64,
    name: &str,
    funcs: &[&'static str],
) {
    let scope = env.new_scope(Some(*parent));
    let map: std::collections::HashMap<String, RuntimeValue> = RuntimeValue::natives();
    let funcs = funcs.into_iter().map(|x| {
        (
            String::from(*x),
            map.get(&format!("{}.{}", name, x)).clone().unwrap(),
        )
    });

    for var in funcs {
        let name = env
            .mappings
            .iter()
            .find(|x| x.split_once(":").unwrap().1 == var.0)
            .unwrap()
            .clone();

        let _ = env.variables.insert(
            name.clone(),
            Variable {
                value: var.1.clone(),
                var_type: VarType::Constant,
            },
        );

        env.scopes
            .get_mut(&scope)
            .unwrap()
            .variables
            .push(name.clone());
    }
}
