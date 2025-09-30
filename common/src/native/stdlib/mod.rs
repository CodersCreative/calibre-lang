use calibre_parser::{ast::VarType, Parser};
use std::{collections::HashMap, fs, rc::Rc, u64};

use crate::{environment::{Environment, RuntimeType, RuntimeValue, Variable}, native::NativeFunction};

pub mod console;
pub mod random;
pub mod thread;

pub fn setup<T : RuntimeValue, U : RuntimeType>(env: &mut Environment<T, U>, scope: &u64) {
    let mut parser = Parser::default();
    let program = parser
        .produce_ast(fs::read_to_string(env.scopes.get(scope).unwrap().path.clone()).unwrap())
        .unwrap();

    let _ = (env.evaluate)(env, scope, program).unwrap();

    setup_scope(env, scope, "thread", &["wait"]);
    setup_scope(env, scope, "console", &["out", "input", "err", "clear"]);
    setup_scope(env, scope, "random", &["generate", "bool", "ratio"]);
}

pub fn setup_scope<T : RuntimeValue, U : RuntimeType>(env: &mut Environment<T, U>, parent: &u64, name : &str, funcs : &[&'static str]) {
    let scope = env.new_scope_from_parent(*parent, name);
    let map = T::natives();
    let funcs = funcs.into_iter().map(|x| (String::from(*x), map.get(*x).clone().unwrap()));

    if let Some(map) = env.variables.get_mut(&scope) {
        for func in funcs {
            map.insert(
                func.0,
                Variable {
                    value: func.1.clone(),
                    var_type: VarType::Constant,
                },
            );
        }
    }
}
