use calibre_common::{environment::Variable, errors::ScopeErr};
use calibre_parser::ast::{ObjectMap, VarType};

use crate::runtime::{
    interpreter::{InterpreterErr, expressions::member::MemberPathType},
    scope::InterpreterEnvironment,
    values::{RuntimeType, RuntimeValue},
};
use std::collections::HashMap;

impl InterpreterEnvironment {
    pub fn get_var<'a>(&'a self, name: &str) -> Result<Variable<RuntimeValue>, ScopeErr> {
        Ok(if let Some(var) = self.variables.get(name) {
            self.convert_saveable_into_runtime_var(var.clone())
        } else {
            return Err(ScopeErr::Variable(name.to_string()));
        })
    }

    pub fn force_var(
        &mut self,
        scope: &u64,
        key: String,
        value: Variable<RuntimeValue>,
    ) -> Result<RuntimeValue, ScopeErr> {
        let typ = (&value.value).into();
        let value = self.convert_runtime_var_into_saveable(value);

        self.scopes
            .get_mut(scope)
            .unwrap()
            .variables
            .push(key.clone());

        self.variables.insert(key.clone(), value);
        Ok(RuntimeValue::Ref(key, typ))
    }

    fn convert_saveable_into_runtime_var(
        &self,
        value: Variable<RuntimeValue>,
    ) -> Variable<RuntimeValue> {
        let var_type = value.var_type.clone();

        fn unwrap_runtime_value(env: &InterpreterEnvironment, value: RuntimeValue) -> RuntimeValue {
            match value {
                RuntimeValue::Ref(pointer, _) => {
                    let inner = env.variables.get(&pointer).unwrap().clone();
                    unwrap_runtime_value(env, inner.value)
                }
                RuntimeValue::Aggregate(x, ObjectMap(map)) => {
                    let new_map = map
                        .into_iter()
                        .map(|(k, v)| (k, unwrap_runtime_value(env, v)))
                        .collect();
                    RuntimeValue::Aggregate(x, ObjectMap(new_map))
                }
                RuntimeValue::List { data, data_type } => {
                    let new_data = data
                        .into_iter()
                        .map(|v| unwrap_runtime_value(env, v))
                        .collect();
                    RuntimeValue::List {
                        data: new_data,
                        data_type,
                    }
                }
                RuntimeValue::Enum(x, y, Some(data)) => {
                    RuntimeValue::Enum(x, y, Some(Box::new(unwrap_runtime_value(env, *data))))
                }
                RuntimeValue::Option(Some(data), typ) => {
                    RuntimeValue::Option(Some(Box::new(unwrap_runtime_value(env, *data))), typ)
                }
                RuntimeValue::Result(Ok(data), typ) => {
                    RuntimeValue::Result(Ok(Box::new(unwrap_runtime_value(env, *data))), typ)
                }
                RuntimeValue::Result(Err(data), typ) => {
                    RuntimeValue::Result(Err(Box::new(unwrap_runtime_value(env, *data))), typ)
                }
                other => other,
            }
        }

        Variable {
            value: unwrap_runtime_value(self, value.value),
            var_type,
        }
    }

    fn convert_runtime_var_into_saveable(
        &mut self,
        value: Variable<RuntimeValue>,
    ) -> Variable<RuntimeValue> {
        let var_type = value.var_type.clone();

        fn transform(
            env: &mut InterpreterEnvironment,
            val: RuntimeValue,
            v_type: &VarType,
        ) -> RuntimeValue {
            match val {
                RuntimeValue::Aggregate(x, ObjectMap(map)) => {
                    let mut new_map = Vec::new();
                    for (k, v) in map {
                        let typ = (&v).into();
                        let inner_val = transform(env, v, v_type);
                        let name = rand::random_range(0..10000000).to_string();
                        env.variables.insert(
                            name.clone(),
                            Variable {
                                value: inner_val,
                                var_type: v_type.clone(),
                            },
                        );
                        new_map.push((k, RuntimeValue::Ref(name, typ)));
                    }
                    RuntimeValue::Aggregate(x, ObjectMap(new_map))
                }
                RuntimeValue::List { data, data_type } => {
                    let mut new_vec = Vec::new();
                    for v in data {
                        let typ = (&v).into();
                        let inner_val = transform(env, v, v_type);
                        let name = rand::random_range(0..10000000).to_string();
                        env.variables.insert(
                            name.clone(),
                            Variable {
                                value: inner_val,
                                var_type: v_type.clone(),
                            },
                        );
                        new_vec.push(RuntimeValue::Ref(name, typ));
                    }
                    RuntimeValue::List {
                        data: new_vec,
                        data_type,
                    }
                }
                RuntimeValue::Enum(x, y, Some(data)) => {
                    let typ = (&*data).into();
                    let inner_val = transform(env, *data, v_type);
                    let name = rand::random_range(0..10000000).to_string();
                    env.variables.insert(
                        name.clone(),
                        Variable {
                            value: inner_val,
                            var_type: v_type.clone(),
                        },
                    );
                    RuntimeValue::Enum(x, y, Some(Box::new(RuntimeValue::Ref(name, typ))))
                }
                RuntimeValue::Option(Some(data), opt_typ) => {
                    let typ = (&*data).into();
                    let inner_val = transform(env, *data, v_type);
                    let name = rand::random_range(0..10000000).to_string();
                    env.variables.insert(
                        name.clone(),
                        Variable {
                            value: inner_val,
                            var_type: v_type.clone(),
                        },
                    );
                    RuntimeValue::Option(Some(Box::new(RuntimeValue::Ref(name, typ))), opt_typ)
                }
                other => other,
            }
        }

        let runtime_value = transform(self, value.value, &var_type);

        Variable {
            value: runtime_value,
            var_type,
        }
    }

    pub fn push_var(
        &mut self,
        scope: &u64,
        name: String,
        value: Variable<RuntimeValue>,
    ) -> Result<RuntimeValue, ScopeErr> {
        if let Some(var) = self.variables.get(&name) {
            if var.var_type == VarType::Constant {
                return Err(ScopeErr::AssignConstant(name));
            }
        };

        self.force_var(scope, name, value)
    }

    pub fn assign_var_from_ref_pointer(
        &mut self,
        pointer: &str,
        value: RuntimeValue,
    ) -> Result<(), ScopeErr> {
        let (target_pointer, var_type) = {
            let existing_var = self
                .variables
                .get(pointer)
                .ok_or_else(|| ScopeErr::Variable(pointer.to_string()))?;

            if let RuntimeValue::Ref(inner_ptr, _) = &existing_var.value {
                (Some(inner_ptr.to_string()), existing_var.var_type.clone())
            } else {
                (None, existing_var.var_type.clone())
            }
        };

        if let Some(inner_ptr) = target_pointer {
            return self.assign_var_from_ref_pointer(&inner_ptr, value);
        }

        if var_type != VarType::Mutable {
            return Err(ScopeErr::AssignConstant(pointer.to_string()));
        }

        let saved_value = self.convert_runtime_var_into_saveable(Variable {
            value,
            var_type: var_type.clone(),
        });

        self.variables.insert(pointer.to_string(), saved_value);
        Ok(())
    }

    pub fn get_var_ref(&self, name: &str) -> Result<RuntimeValue, ScopeErr> {
        if let Some(typ) = self.variables.get(name).map(|x| (&x.value).into()) {
            Ok(RuntimeValue::Ref(name.to_string(), typ))
        } else {
            Err(ScopeErr::Variable(name.to_string()))
        }
    }

    pub fn get_member_ref(&self, keys: &[MemberPathType]) -> Result<RuntimeValue, InterpreterErr> {
        let first = {
            let Some(MemberPathType::Dot(first)) = keys.get(0) else {
                panic!("Invalid member path start")
            };
            first.clone()
        };

        if keys.len() <= 1 {
            let var = self.get_var(&first)?;
            let typ = (&var.value).into();
            return Ok(RuntimeValue::Ref(first, typ));
        }

        let mut pointer = first;

        // Resolve initial pointer chain
        while let Some(var) = self.variables.get(&pointer) {
            if let RuntimeValue::Ref(x, _) = &var.value {
                pointer = x.to_string();
            } else {
                break;
            }
        }

        for key in keys.iter().skip(1) {
            let current_val = if let Some(x) = self.variables.get(&pointer) {
                &x.value
            } else {
                return Err(InterpreterErr::Value(
                    calibre_common::errors::ValueErr::ProgressErr,
                ));
            };
            match (current_val, key) {
                (RuntimeValue::Aggregate(_, map), _) => match map.get(&key.to_string()) {
                    Some(RuntimeValue::Ref(p, _)) => pointer = p.clone(),
                    _ => break,
                },
                (RuntimeValue::List { data, .. }, MemberPathType::Computed(k)) => {
                    let idx = k.parse::<usize>().unwrap();
                    match data.get(idx) {
                        Some(RuntimeValue::Ref(p, _)) => pointer = p.clone(),
                        _ => break,
                    }
                }
                (
                    RuntimeValue::Enum(_, _, Some(data))
                    | RuntimeValue::Option(Some(data), _)
                    | RuntimeValue::Result(Ok(data), _)
                    | RuntimeValue::Result(Err(data), _),
                    _,
                ) => {
                    if let RuntimeValue::Ref(p, _) = &**data {
                        pointer = p.clone();
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }

        let final_var = self.variables.get(&pointer).unwrap();
        let typ = (&final_var.value).into();
        Ok(RuntimeValue::Ref(pointer, typ))
    }

    pub fn update_var<F>(&mut self, name: &str, mut f: F) -> Result<(), ScopeErr>
    where
        F: FnMut(&mut Variable<RuntimeValue>),
    {
        if let Some(value) = self.variables.get_mut(name) {
            f(value);
            Ok(())
        } else {
            Err(ScopeErr::Variable(name.to_string()))
        }
    }

    pub fn assign_var(
        &mut self,
        name: &str,
        value: RuntimeValue,
    ) -> Result<RuntimeValue, InterpreterErr> {
        let typ: RuntimeType = (&value).into();

        let var_type = if let Some(var) = self.variables.get(name) {
            match var.var_type {
                VarType::Mutable => var.var_type.clone(),
                _ => return Err(ScopeErr::AssignConstant(name.to_string()).into()),
            }
        } else {
            return Err(ScopeErr::Variable(name.to_string()).into());
        };

        let new_var = self.convert_runtime_var_into_saveable(Variable { value, var_type });

        self.variables.insert(name.to_string(), new_var);
        Ok(RuntimeValue::Ref(name.to_string(), typ))
    }
}
