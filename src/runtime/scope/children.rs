use crate::{
    parser::{self},
    runtime::{
        interpreter::InterpreterErr,
        scope::{Environment, ScopeErr},
    },
};
use std::fs;

impl Environment {
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
                let current = self.scopes.get(&scope).unwrap();
                if let Some(x) = current.children.get(key) {
                    x.clone()
                } else {
                    if let Some(s) = self.get_global_scope().children.get(key) {
                        s.clone()
                    } else {
                        let scope = self.new_scope_from_parent(current.id, key);
                        let mut parser = parser::Parser::default();
                        let program = parser
                            .produce_ast(
                                fs::read_to_string(self.scopes.get(&scope).unwrap().path.clone())
                                    .unwrap(),
                            )
                            .unwrap();
                        // let _ = evaluate(program, &scope)?;
                        scope
                    }
                }
            }
        })
    }

    // pub fn get_scope_path(this: &Rc<RefCell<Scope>>, path: &mut Vec<String>) -> Vec<String> {
    //
    //     if let Some(parent) = &this.borrow().parent {
    //         for (k, v) in parent.borrow().children.iter() {
    //             if v.borrow().path == this.borrow().path {
    //                 path.push(k.clone());
    //                 break;
    //             }
    //         }
    //         get_scope_path(&parent, path)
    //     } else {
    //         path.to_vec()
    //     }
    // }

    pub fn get_scope_list(&mut self, scope: u64, mut list: Vec<String>) -> Result<u64, ScopeErr> {
        if list.len() <= 0 {
            return Ok(scope);
        }
        let first = list.remove(0);
        let scope = self.get_next_scope(scope, first.as_str())?;
        self.get_scope_list(scope, list)
    }

    pub fn get_next_scope(&mut self, scope: u64, key: &str) -> Result<u64, ScopeErr> {
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
                        return Err(ScopeErr::Scope(key.to_string())); // _ => Scope::new_from_parent(this.clone(), key.to_string()),
                    }
                }
            }
        })
    }
}
