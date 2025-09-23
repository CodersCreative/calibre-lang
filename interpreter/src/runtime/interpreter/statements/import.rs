use calibre_parser::ast::NodeType;

use crate::runtime::{
    interpreter::InterpreterErr,
    scope::{Environment, Object, Variable},
    values::RuntimeValue,
};
use std::collections::HashMap;

impl Environment {
    pub fn evaluate_import_statement(
        &mut self,
        scope: &u64,
        module : Vec<String>,
        alias : Option<String>,
        values : Vec<String>,
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
                let vars: Vec<(String, Variable)> = self
                    .variables
                    .get(&new_scope)
                    .unwrap()
                    .iter()
                    .map(|x| {
                        (
                            x.0.clone(),
                            Variable {
                                value: RuntimeValue::Link(
                                    new_scope,
                                    vec![x.0.to_string()],
                                    (&x.1.value).into(),
                                ),
                                var_type: x.1.var_type.clone(),
                            },
                        )
                    })
                    .collect();

                for (key, value) in vars {
                    if !key.starts_with("__") {
                        let _ = self.push_var(scope, key, value.clone());
                    }
                }

                let obj: Vec<(String, Object)> = self
                    .objects
                    .get(&new_scope)
                    .unwrap()
                    .iter()
                    .map(|x| {
                        (
                            x.0.clone(),
                            Object {
                                object_type: crate::runtime::scope::Type::Link(
                                    new_scope,
                                    x.0.clone(),
                                ),
                                functions: HashMap::new(),
                                traits: Vec::new(),
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
                            Variable {
                                value: RuntimeValue::Link(
                                    new_scope,
                                    vec![value.clone()],
                                    (&var.value).into(),
                                ),
                                var_type: var.var_type.clone(),
                            },
                        )?;
                    } else if let Some(obj) = self.objects.get(&new_scope).unwrap().get(&value) {
                        self.push_object(scope, value.clone(), obj.clone())?; //Object::Link(path.clone()))?;
                    } else {
                        panic!()
                    }
                }
            }

            Ok(RuntimeValue::Null)
    }
}
