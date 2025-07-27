use std::{cell::RefCell, fs, path::PathBuf, rc::Rc, str::FromStr};

use crate::{
    parser::Parser,
    runtime::{
        interpreter::evaluate,
        scope::{Scope, ScopeErr},
    },
    utils::get_path,
};

pub mod console;
pub mod thread;

pub fn setup(scope: Rc<RefCell<Scope>>) {
    let mut parser = Parser::default();
    let program = parser
        .produce_ast(fs::read_to_string(scope.borrow().path.clone()).unwrap())
        .unwrap();

    let _ = evaluate(program, &scope).unwrap();

    let scopes: Vec<(String, Box<dyn Fn(Rc<RefCell<Scope>>)>)> = vec![
        (String::from("thread"), Box::new(thread::setup)),
        (String::from("console"), Box::new(console::setup)),
    ];

    for (name, func) in scopes {
        func(scope.clone());
    }
}
