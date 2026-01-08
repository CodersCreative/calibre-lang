use crate::{
    environment::{Environment, RuntimeType, RuntimeValue, Scope},
    errors::ScopeErr,
};

impl<T: RuntimeValue, U: RuntimeType> Environment<T, U> {
    pub fn get_scope_list(&self, scope: u64, mut list: Vec<String>) -> Result<u64, ScopeErr> {
        if list.len() <= 0 {
            return Ok(scope);
        }
        let first = list.remove(0);
        let scope = self.get_next_scope(scope, first.as_str())?;
        self.get_scope_list(scope, list)
    }

    pub fn get_next_scope(&self, scope: u64, key: &str) -> Result<u64, ScopeErr> {
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

    pub fn get_root_scope<'a>(&'a self) -> &'a Scope {
        for i in 1..self.scope_counter {
            if let Some(scope) = self.scopes.get(&i) {
                if scope.namespace == "root" {
                    return scope;
                }
            }
        }

        todo!()
    }
}
