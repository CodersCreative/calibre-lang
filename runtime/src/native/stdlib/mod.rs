use crate::{parser::Parser, runtime::scope::Environment};
use std::{fs, u64};

pub mod console;
pub mod random;
pub mod thread;

pub fn setup(env: &mut Environment, scope: &u64) {
    let mut parser = Parser::default();
    let program = parser
        .produce_ast(fs::read_to_string(env.scopes.get(scope).unwrap().path.clone()).unwrap())
        .unwrap();

    let _ = env.evaluate(scope, program).unwrap();

    let scopes: Vec<Box<dyn Fn(&mut Environment, &u64)>> = vec![
        Box::new(thread::setup),
        Box::new(console::setup),
        Box::new(random::setup),
    ];

    for func in scopes {
        func(env, scope);
    }
}
