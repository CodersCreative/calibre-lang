use calibre_common::environment::{Object, Variable};

use crate::runtime::{
    interpreter::InterpreterErr, scope::CheckerEnvironment, values::RuntimeType,
};
use std::collections::HashMap;

impl CheckerEnvironment {
    pub fn evaluate_import_statement(
        &mut self,
        scope: &u64,
        module : Vec<String>,
        alias : Option<String>,
        values : Vec<String>,
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
                let vars: Vec<(String, Variable<RuntimeType>)> = self
                    .variables
                    .get(&new_scope)
                    .unwrap()
                    .iter()
                    .map(|x| {
                        (
                            x.0.clone(),
                            x.1.clone()
                        )
                    })
                    .collect();

                for (key, value) in vars {
                    if !key.starts_with("__") {
                        let _ = self.push_var(scope, key, value.clone());
                    }
                }

                let obj: Vec<(String, Object<RuntimeType, RuntimeType>)> = self
                    .objects
                    .get(&new_scope)
                    .unwrap()
                    .iter()
                    .map(|x| {
                        (
                            x.0.clone(),
                            Object {
                                object_type: calibre_common::environment::Type::Link(
                                    new_scope,
                                    x.0.clone(),
                                ),
                                functions: HashMap::new(),
                                traits: Vec::new(),
                                location: None,
                            },
                        )
                    })
                    .collect();

                for (value, obj) in obj {
                    if !value.starts_with("__") {
                        let _ = self.push_object(scope, value.clone(), obj.clone());
                    }
                }
            } else {
                for value in values {
                    if let Some(var) = self.variables.get(&new_scope).unwrap().get(&value) {
                        self.push_var(
                            scope,
                            value.clone(),
                            var.clone()
                        )?;
                    } else if self.objects.get(&new_scope).unwrap().contains_key(&value) {
                        let val = self.objects.get(&new_scope).unwrap().get(&value).unwrap().clone();
                        self.push_object(scope, value.clone(), val)?; //Object::Link(path.clone()))?;
                    } else {
                        panic!()
                    }
                }
            }

            Ok(RuntimeType::Null)
    }
}
