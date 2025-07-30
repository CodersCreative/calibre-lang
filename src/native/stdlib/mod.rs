use std::{fs, u64};

use crate::{
    parser::Parser,
    runtime::scope::Environment,
};

pub mod console;
pub mod random;
pub mod thread;

pub fn setup(env: &mut Environment, scope: &u64) {
    let mut parser = Parser::default();
    let program = parser
        .produce_ast(fs::read_to_string(env.scopes.get(scope).unwrap().path.clone()).unwrap())
        .unwrap();

    let _ = env.evaluate(scope, program).unwrap();

    let scopes: Vec<(String, Box<dyn Fn(&mut Environment, &u64)>)> = vec![
        (String::from("thread"), Box::new(thread::setup)),
        (String::from("console"), Box::new(console::setup)),
        (String::from("random"), Box::new(random::setup)),
    ];

    for (name, func) in scopes {
        func(env, scope);
    }
}
