pub mod variables;

use std::{
    fs,
    ops::{Deref, DerefMut},
};

use crate::runtime::{
    interpreter::InterpreterErr,
    values::{RuntimeType, RuntimeValue, helper::StopValue},
};
use calibre_common::environment::Environment;
use calibre_parser::{Parser, lexer::Tokenizer};

#[derive(Debug, Clone, PartialEq)]
pub struct InterpreterEnvironment {
    pub env: Environment<RuntimeValue, RuntimeType>,
    pub stop: Option<StopValue>,
}

impl InterpreterEnvironment {
    pub fn new() -> Self {
        Self {
            env: Environment::new(true),
            stop: None,
        }
    }

    pub fn import_scope_list(
        &mut self,
        scope: u64,
        mut list: Vec<String>,
    ) -> Result<u64, InterpreterErr> {
        if list.len() <= 0 {
            return Ok(scope);
        }
        let first = list.remove(0);
        let scope = self.import_next_scope(scope, first.as_str())?;
        self.import_scope_list(scope, list)
    }

    pub fn import_next_scope(&mut self, scope: u64, key: &str) -> Result<u64, InterpreterErr> {
        Ok(match key {
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
                        let program = parser
                            .produce_ast(
                                tokenizer
                                    .tokenize(
                                        fs::read_to_string(
                                            self.scopes.get(&scope).unwrap().path.clone(),
                                        )
                                        .unwrap(),
                                    )
                                    .unwrap(),
                            )
                            .unwrap();

                        let _ = self.evaluate(&scope, program)?;
                        scope
                    }
                }
            }
        })
    }
}
impl Deref for InterpreterEnvironment {
    type Target = Environment<RuntimeValue, RuntimeType>;
    fn deref(&self) -> &Self::Target {
        &self.env
    }
}

impl DerefMut for InterpreterEnvironment {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.env
    }
}
