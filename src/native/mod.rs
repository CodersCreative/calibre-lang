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
        interpreter::{evaluate, InterpreterErr},
        scope::{links::get_link_path, Environment, Scope},
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
        parent: Option<&'a Scope>,
        path: PathBuf,
        namespace: Option<&str>,
    ) -> &'a Scope {
        let mut parser = parser::Parser::default();
        let scope = Scope {
                id : self.counter.clone(),
                namespace : namespace.unwrap_or(&self.counter.to_string()).to_string(),
                parent: if let Some(parent) = parent {Some(parent.id.clone())} else {None},
                children: HashMap::new(),
                path,
            };

        // global::setup(&scope);

        let program = parser
            .produce_ast(fs::read_to_string(get_path("native/global/main.cl".to_string())).unwrap())
            .unwrap();

        let _ = evaluate(program, &scope).unwrap();

        let std = self.new_scope(
            Some(&scope.clone()),
            PathBuf::from_str(&get_path("native/stdlib/main.cl".to_string())).unwrap(),
            Some("std"),
        );

        // stdlib::setup(std.clone());

        let root = self.new_scope(Some(&scope), path, Some("root"));

        self.scopes.get(&(self.counter - 1)).unwrap()
    }
}
