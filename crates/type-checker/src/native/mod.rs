use std::{cmp::Ordering, collections::HashMap, fmt::Debug, fs, path::PathBuf};

use calibre_common::{
    environment::Scope,
    utils::{get_globals_path, get_stdlib_path},
};
use calibre_parser::{Parser, lexer::Tokenizer};

use crate::runtime::scope::CheckerEnvironment;

pub mod stdlib;

impl CheckerEnvironment {
    pub fn new_scope_with_stdlib<'a>(
        &'a mut self,
        parent: Option<u64>,
        path: PathBuf,
        namespace: Option<&str>,
    ) -> u64 {
        let mut parser = Parser::default();
        let scope = 0;
        let counter = self.scope_counter;

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
