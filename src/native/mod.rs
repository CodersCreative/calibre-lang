pub mod global;
pub mod stdlib;

use std::{
    cell::RefCell, cmp::Ordering, collections::HashMap, fmt::Debug, fs, io::stderr,
    marker::PhantomData, path::PathBuf, rc::Rc, str::FromStr, thread, time::Duration,
};

use rustyline::DefaultEditor;

use crate::{
    parser,
    runtime::{
        interpreter::{InterpreterErr, evaluate},
        scope::{Environment, Scope},
        values::{RuntimeType, RuntimeValue},
    },
    utils::get_path,
};

pub trait NativeFunction {
    fn run(
        &self,
        args: &[(RuntimeValue, Option<RuntimeValue>)],
        scope: &Rc<RefCell<Scope>>,
    ) -> Result<RuntimeValue, InterpreterErr>;
}

impl Debug for dyn NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Native Function")
    }
}

impl PartialEq for dyn NativeFunction {
    fn ne(&self, other: &Self) -> bool {
        false
    }

    fn eq(&self, other: &Self) -> bool {
        true
    }
}
impl PartialOrd for dyn NativeFunction {
    fn gt(&self, other: &Self) -> bool {
        false
    }

    fn lt(&self, other: &Self) -> bool {
        false
    }

    fn ge(&self, other: &Self) -> bool {
        true
    }

    fn le(&self, other: &Self) -> bool {
        true
    }

    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(Ordering::Equal)
    }
}

impl Environment {
    pub fn new_scope_with_stdlib<'a>(
        &'a mut self,
        parent: Option<u64>,
        path: PathBuf,
        namespace: Option<&str>,
    ) -> u64 {
        let mut parser = parser::Parser::default();
        let scope = self.counter.clone();

        self.add_scope(Scope {
            id: 0,
            namespace: namespace.unwrap_or(&self.counter.to_string()).to_string(),
            parent,
            children: HashMap::new(),
            path,
        });

        // global::setup(&scope);

        let program = parser
            .produce_ast(fs::read_to_string(get_path("native/global/main.cl".to_string())).unwrap())
            .unwrap();

        let _ = evaluate(program, &scope).unwrap();

        let std = self.new_scope(
            Some(scope),
            PathBuf::from_str(&get_path("native/stdlib/main.cl".to_string())).unwrap(),
            Some("std"),
        );

        // stdlib::setup(std.clone());

        let root = self.new_scope(Some(scope), path, Some("root"));

        self.counter - 1
    }
}
