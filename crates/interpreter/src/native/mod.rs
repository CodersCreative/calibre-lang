use std::{cmp::Ordering, collections::HashMap, fmt::Debug, fs, path::PathBuf};

use calibre_common::{
    environment::Scope,
    utils::{get_globals_path, get_stdlib_path},
};
use calibre_parser::{Parser, lexer::Tokenizer};

use crate::runtime::{
    interpreter::InterpreterErr, scope::InterpreterEnvironment, values::RuntimeValue,
};

pub mod global;
pub mod stdlib;

pub trait NativeFunction {
    fn run(
        &self,
        env: &mut InterpreterEnvironment,
        scope: &u64,
        args: &[(RuntimeValue, Option<RuntimeValue>)],
    ) -> Result<RuntimeValue, InterpreterErr>;
}

impl Debug for dyn NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Native Function")
    }
}

impl PartialEq for dyn NativeFunction {
    fn ne(&self, _other: &Self) -> bool {
        false
    }

    fn eq(&self, _other: &Self) -> bool {
        true
    }
}
impl PartialOrd for dyn NativeFunction {
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

impl InterpreterEnvironment {
    pub fn new_scope_with_stdlib<'a>(
        &'a mut self,
        parent: Option<u64>,
        path: PathBuf,
        namespace: Option<&str>,
    ) -> u64 {
        let mut parser = Parser::default();
        let scope = 0;
        let counter = self.counter;

        self.add_scope(Scope {
            id: 0,
            namespace: namespace.unwrap_or(&counter.to_string()).to_string(),
            parent,
            children: HashMap::new(),
            counter: 0,
            path: path.clone(),
            variables: HashMap::new(),
            objects: HashMap::new(),
        });

        calibre_common::native::global::setup(self, &scope);

        let mut tokenizer = Tokenizer::default();
        let program = parser
            .produce_ast(
                tokenizer
                    .tokenize(fs::read_to_string(&get_globals_path()).unwrap())
                    .unwrap(),
            )
            .unwrap();

        let _ = self.evaluate(&scope, program).unwrap();

        let std = self.new_scope(Some(scope), get_stdlib_path(), Some("std"));

        stdlib::setup(self, &std);

        let root = self.new_scope(Some(scope), path, Some("root"));

        root
    }
}
