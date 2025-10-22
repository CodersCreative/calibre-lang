pub mod variables;

use std::{
    fs,
    ops::{Deref, DerefMut},
};

use crate::runtime::{
    interpreter::InterpreterErr,
    values::{RuntimeType, helper::StopValue},
};
use calibre_common::environment::Environment;
use calibre_parser::{Parser, ParserError, lexer::Tokenizer};

#[derive(Debug, Clone)]
pub struct CheckerEnvironment {
    pub env: Environment<RuntimeType, RuntimeType>,
    pub stop: Option<StopValue>,
    pub errors: Vec<InterpreterErr>,
}

impl PartialEq for CheckerEnvironment {
    fn eq(&self, other: &Self) -> bool {
        self.env == other.env && self.stop == other.stop
    }

    fn ne(&self, other: &Self) -> bool {
        self.env != other.env || self.stop != other.stop
    }
}

impl CheckerEnvironment {
    pub fn new() -> Self {
        Self {
            env: Environment::new(false),
            stop: None,
            errors: Vec::new(),
        }
    }

    pub fn add_err(&mut self, err: InterpreterErr) {
        self.errors.push(err);
    }

    pub fn add_parser_errors(&mut self, errors: Vec<ParserError>) {
        if !errors.is_empty() {
            self.add_err(errors.into());
        }
    }

    pub fn unwrap_type<E: Into<InterpreterErr>>(
        &mut self,
        val: Result<RuntimeType, E>,
    ) -> RuntimeType {
        match val {
            Ok(x) => x,
            Err(e) => {
                self.add_err(e.into());
                RuntimeType::Dynamic
            }
        }
    }

    pub fn import_scope_list(&mut self, scope: u64, mut list: Vec<String>) -> u64 {
        if list.len() <= 0 {
            return scope;
        }
        let first = list.remove(0);
        let scope = self.import_next_scope(scope, first.as_str());
        self.import_scope_list(scope, list)
    }

    pub fn import_next_scope(&mut self, scope: u64, key: &str) -> u64 {
        match key {
            "super" => self.scopes.get(&scope).unwrap().parent.clone().unwrap(),
            _ => {
                let current = self.scopes.get(&scope).unwrap().clone();
                if let Some(x) = current.children.get(key) {
                    x.clone()
                } else {
                    if let Some(s) = self.get_global_scope().children.get(key) {
                        s.clone()
                    } else {
                        let scope = self.new_scope_from_parent(current.id, key);
                        let mut parser = Parser::default();

                        let mut tokenizer = Tokenizer::default();
                        let program = parser.produce_ast(
                            tokenizer
                                .tokenize(
                                    fs::read_to_string(
                                        self.scopes.get(&scope).unwrap().path.clone(),
                                    )
                                    .unwrap(),
                                )
                                .unwrap(),
                        );

                        self.add_parser_errors(parser.errors);

                        let _ = self.evaluate(&scope, program);
                        scope
                    }
                }
            }
        }
    }
}
impl Deref for CheckerEnvironment {
    type Target = Environment<RuntimeType, RuntimeType>;
    fn deref(&self) -> &Self::Target {
        &self.env
    }
}

impl DerefMut for CheckerEnvironment {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.env
    }
}
