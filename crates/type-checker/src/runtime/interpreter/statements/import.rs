use calibre_common::environment::{Object, Variable};

use crate::runtime::{interpreter::InterpreterErr, scope::CheckerEnvironment, values::RuntimeType};
use std::collections::HashMap;

impl CheckerEnvironment {
    pub fn evaluate_import_statement(
        &mut self,
        scope: &u64,
        module: Vec<String>,
        alias: Option<String>,
        values: Vec<String>,
    ) -> Result<RuntimeType, InterpreterErr> {
        let new_scope = if let Some(alias) = alias {
            if ["super", "root"].contains(&alias.as_str()) {
                return Ok(RuntimeType::Null);
            }
            let new_scope_id = self.get_scope_list(*scope, module)?;
            self.scopes
                .get_mut(scope)
                .unwrap()
                .children
                .insert(alias, new_scope_id);

            return Ok(RuntimeType::Null);
        } else if !values.is_empty() {
            self.get_scope_list(*scope, module.clone())?
        } else {
            let _ = self.import_scope_list(*scope, module.clone());
            return Ok(RuntimeType::Null);
        };

        if &values[0] == "*" {
            let vars: Vec<(String, u64)> = self
                .scopes
                .get(&new_scope)
                .unwrap()
                .variables
                .iter()
                .map(|x| (x.0.clone(), x.1.clone()))
                .collect();

            for (key, value) in vars {
                if !key.starts_with("__") {
                    self.scopes
                        .get_mut(scope)
                        .unwrap()
                        .variables
                        .insert(key, value);
                }
            }

            let objs: Vec<(String, u64)> = self
                .scopes
                .get(&new_scope)
                .unwrap()
                .objects
                .iter()
                .map(|x| (x.0.clone(), x.1.clone()))
                .collect();

            for (key, value) in objs {
                if !key.starts_with("__") {
                    self.scopes
                        .get_mut(scope)
                        .unwrap()
                        .objects
                        .insert(key, value);
                }
            }
        } else {
            for key in values {
                if let Some(value) = self
                    .scopes
                    .get(&new_scope)
                    .unwrap()
                    .variables
                    .get(&key)
                    .map(|x| x.clone())
                {
                    self.scopes
                        .get_mut(scope)
                        .unwrap()
                        .variables
                        .insert(key.clone(), value);
                }
                if let Some(value) = self
                    .scopes
                    .get(&new_scope)
                    .unwrap()
                    .objects
                    .get(&key)
                    .map(|x| x.clone())
                {
                    self.scopes
                        .get_mut(scope)
                        .unwrap()
                        .objects
                        .insert(key, value);
                } else {
                    panic!()
                }
            }
        }

        Ok(RuntimeType::Null)
    }
}
