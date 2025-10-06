use calibre_common::environment::{Object, Variable};

use crate::runtime::{
    interpreter::InterpreterErr,
    scope::InterpreterEnvironment,
    values::{RuntimeType, RuntimeValue},
};
use std::collections::HashMap;

impl InterpreterEnvironment {
    pub fn evaluate_import_statement(
        &mut self,
        scope: &u64,
        module: Vec<String>,
        alias: Option<String>,
        values: Vec<String>,
    ) -> Result<RuntimeValue, InterpreterErr> {
        let new_scope = if let Some(alias) = alias {
            if ["super", "root"].contains(&alias.as_str()) {
                return Ok(RuntimeValue::Null);
            }
            let new_scope_id = self.get_scope_list(*scope, module)?;
            self.scopes
                .get_mut(scope)
                .unwrap()
                .children
                .insert(alias, new_scope_id);

            return Ok(RuntimeValue::Null);
        } else if !values.is_empty() {
            self.get_scope_list(*scope, module.clone())?
        } else {
            let _ = self.import_scope_list(*scope, module.clone());
            return Ok(RuntimeValue::Null);
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

        Ok(RuntimeValue::Null)
    }
}
