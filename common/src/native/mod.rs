use crate::{
    environment::{Environment, RuntimeType, RuntimeValue, Scope}, errors::InterpreterErr, utils::get_path
};
use calibre_parser::Parser;
use std::{cmp::Ordering, collections::HashMap, fmt::Debug, fs, path::PathBuf, str::FromStr};

pub mod global;
pub mod stdlib;

pub trait NativeFunction<T : RuntimeValue, U : RuntimeType> {
    fn run(
        &self,
        env: &mut Environment<T, U>,
        scope: &u64,
        args: &[(T, Option<T>)],
    ) -> Result<T, InterpreterErr<T, U>>;
}

impl<T : RuntimeValue, U : RuntimeType> Debug for dyn NativeFunction<T, U> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Native Function")
    }
}

impl<T : RuntimeValue, U : RuntimeType> PartialEq for dyn NativeFunction<T, U> {
    fn ne(&self, _other: &Self) -> bool {
        false
    }

    fn eq(&self, _other: &Self) -> bool {
        true
    }
}
impl<T : RuntimeValue, U : RuntimeType> PartialOrd for dyn NativeFunction<T, U> {
    fn gt(&self, _other: &Self) -> bool {
        false
    }

    fn lt(&self, _other: &Self) -> bool {
        false
    }

    fn ge(&self, _other: &Self) -> bool {
        true
    }

    fn le(&self, _other: &Self) -> bool {
        true
    }

    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        Some(Ordering::Equal)
    }
}

impl<T : RuntimeValue, U : RuntimeType> Environment<T, U> {
    pub fn new_scope_with_stdlib<'a>(
        &'a mut self,
        parent: Option<u64>,
        path: PathBuf,
        namespace: Option<&str>,
    ) -> u64 {
        let mut parser = Parser::default();
        let scope = 0;

        self.add_scope(Scope {
            id: 0,
            namespace: namespace.unwrap_or(&self.counter.to_string()).to_string(),
            parent,
            children: HashMap::new(),
            counter: 0,
            path: path.clone(),
        });

        global::setup(self, &scope);

        let program = parser
            .produce_ast(fs::read_to_string(get_path("native/global/main.cl".to_string())).unwrap())
            .unwrap();

        let _ = (self.evaluate)(self, &scope, program).unwrap();

        let std = self.new_scope(
            Some(scope),
            PathBuf::from_str(&get_path("native/stdlib/main.cl".to_string())).unwrap(),
            Some("std"),
        );

        stdlib::setup(self, &std);

        let root = self.new_scope(Some(scope), path, Some("root"));

        root
    }
}
