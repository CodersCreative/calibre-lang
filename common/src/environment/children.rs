use calibre_parser::Parser;
use std::fs;

use crate::{environment::{Environment, RuntimeType, RuntimeValue, Scope}, errors::{InterpreterErr, ScopeErr}};

impl<T : RuntimeValue, U : RuntimeType> Environment<T, U> {
    pub fn import_scope_list(
        &mut self,
        scope: u64,
        mut list: Vec<String>,
    ) -> Result<u64, InterpreterErr<T, U>> {
        if list.len() <= 0 {
            return Ok(scope);
        }
        let first = list.remove(0);
        let scope = self.import_next_scope(scope, first.as_str())?;
        self.import_scope_list(scope, list)
    }

    pub fn import_next_scope(&mut self, scope: u64, key: &str) -> Result<u64, InterpreterErr<T, U>> {
        Ok(match key {
            "super" => self.scopes.get(&scope).unwrap().parent.clone().unwrap(),
            _ => {
                let current = self.scopes.get(&scope).unwrap();
                if let Some(x) = current.children.get(key) {
                    x.clone()
                } else {
                    if let Some(s) = self.get_global_scope().children.get(key) {
                        s.clone()
                    } else {
                        let scope = self.new_scope_from_parent(current.id, key);
                        let mut parser = Parser::default();
                        let program = parser
                            .produce_ast(
                                fs::read_to_string(self.scopes.get(&scope).unwrap().path.clone())
                                    .unwrap(),
                            )
                            .unwrap();
                        let _ = (self.evaluate)(self, &scope, program)?;
                        scope
                    }
                }
            }
        })
    }

    pub fn get_scope_list(&mut self, scope: u64, mut list: Vec<String>) -> Result<u64, ScopeErr<T>> {
        if list.len() <= 0 {
            return Ok(scope);
        }
        let first = list.remove(0);
        let scope = self.get_next_scope(scope, first.as_str())?;
        self.get_scope_list(scope, list)
    }

    pub fn get_next_scope(&mut self, scope: u64, key: &str) -> Result<u64, ScopeErr<T>> {
        Ok(match key {
            "super" => self.scopes.get(&scope).unwrap().parent.clone().unwrap(),
            _ => {
                let current = self.scopes.get(&scope).unwrap();
                if let Some(x) = current.children.get(key) {
                    x.clone()
                } else {
                    if let Some(s) = self.get_global_scope().children.get(key) {
                        s.clone()
                    } else {
                        return Err(ScopeErr::Scope(key.to_string()));
                    }
                }
            }
        })
    }

    pub fn get_global_scope<'a>(&'a self) -> &'a Scope {
        self.scopes.get(&0).unwrap()
    }
}
