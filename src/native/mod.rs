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
        scope::{Scope, links::get_link_path},
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

impl Scope {
    pub fn new_with_stdlib(
        parent: Option<Rc<RefCell<Self>>>,
        path: PathBuf,
        namespace: Option<String>,
    ) -> (Rc<RefCell<Self>>, parser::Parser) {
        let mut parser = parser::Parser::default();
        let scope = Self {
            variables: HashMap::new(),
            children: HashMap::new(),
            objects: HashMap::new(),
            stop: None,
            functions: HashMap::new(),
            parent: parent.clone(),
            path: path.clone(),
        };

        let scope = Rc::new(RefCell::new(scope));

        global::setup(scope.clone());

        let program = parser
            .produce_ast(fs::read_to_string(get_path("native/global/main.cl".to_string())).unwrap())
            .unwrap();

        let _ = evaluate(program, &scope).unwrap();

        let std = Scope::new(
            Some(scope.clone()),
            PathBuf::from_str(&get_path("native/stdlib/main.cl".to_string())).unwrap(),
            Some("std".to_string()),
        );

        stdlib::setup(std.clone());

        let root = Scope::new(Some(scope.clone()), path, Some("root".to_string()));

        (root, parser)
    }
}
