use calibre_common::{environment::Variable, errors::ScopeErr};
use calibre_parser::ast::{ObjectMap, ObjectType, VarType};

use crate::runtime::{
    interpreter::{InterpreterErr, expressions::member::MemberPathType},
    scope::InterpreterEnvironment,
    values::{RuntimeType, RuntimeValue},
};
use std::collections::HashMap;

impl InterpreterEnvironment {
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

    fn _convert_saveable_into_runtime_var(
        &self,
        value: Variable<RuntimeValue>,
    ) -> Variable<RuntimeValue> {
        let var_type = value.var_type.clone();

        fn unwrap_list(env: &InterpreterEnvironment, list: Vec<RuntimeValue>) -> Vec<RuntimeValue> {
            list.into_iter()
                .map(|v| match v {
                    RuntimeValue::Ref(pointer, _) => {
                        let inner = env.variables.get(&pointer).unwrap().clone();
                        unwrap_runtime_value(env, inner.value)
                    }
                    other => unwrap_runtime_value(env, other),
                })
                .collect()
        }

        fn unwrap_map(
            env: &InterpreterEnvironment,
            map: HashMap<String, RuntimeValue>,
        ) -> HashMap<String, RuntimeValue> {
            map.into_iter()
                .map(|(k, v)| {
                    let val = match v {
                        RuntimeValue::Ref(pointer, _) => {
                            let inner = env.variables.get(&pointer).unwrap().clone();
                            unwrap_runtime_value(env, inner.value)
                        }
                        other => unwrap_runtime_value(env, other),
                    };
                    (k, val)
                })
                .collect()
        }

        fn unwrap_singular(env: &InterpreterEnvironment, value: RuntimeValue) -> RuntimeValue {
            match value {
                RuntimeValue::Ref(pointer, _) => {
                    let inner = env.variables.get(&pointer).unwrap().clone();
                    unwrap_runtime_value(env, inner.value)
                }
                other => unwrap_runtime_value(env, other),
            }
        }

        fn unwrap_runtime_value(env: &InterpreterEnvironment, value: RuntimeValue) -> RuntimeValue {
            match value {
                RuntimeValue::Aggregate(x, ObjectMap(map)) => {
                    RuntimeValue::Aggregate(x, ObjectMap(unwrap_map(env, map)))
                }
                RuntimeValue::Enum(x, y, Some(ObjectMap(map))) => {
                    RuntimeValue::Enum(x, y, Some(ObjectMap(unwrap_map(env, map))))
                }
                RuntimeValue::Option(Some(data), typ) => {
                    RuntimeValue::Option(Some(Box::new(unwrap_singular(env, *data))), typ)
                }
                RuntimeValue::Result(Ok(data), typ) => {
                    RuntimeValue::Result(Ok(Box::new(unwrap_singular(env, *data))), typ)
                }
                RuntimeValue::Result(Err(data), typ) => {
                    RuntimeValue::Result(Err(Box::new(unwrap_singular(env, *data))), typ)
                }
                RuntimeValue::List { data, data_type } => RuntimeValue::List {
                    data: unwrap_list(env, data),
                    data_type,
                },
                RuntimeValue::Ref(pointer, _) => {
                    let inner = env.variables.get(&pointer).unwrap().clone();
                    unwrap_runtime_value(env, inner.value)
                }
                other => other,
            }
        }

        let runtime_value = unwrap_runtime_value(self, value.value);

        Variable {
            value: runtime_value,
            var_type,
        }
    }

    fn convert_runtime_var_into_saveable(
        &mut self,
        value: Variable<RuntimeValue>,
    ) -> Variable<RuntimeValue> {
        let var_type = value.var_type.clone();

        let get_new_list = |this: &mut Self, list: Vec<RuntimeValue>| -> Vec<RuntimeValue> {
            let mut new_vec = Vec::new();

            for v in list {
                let typ = (&v).into();

                let value = this.convert_runtime_var_into_saveable(Variable {
                    value: v,
                    var_type: var_type.clone(),
                });

                let name = rand::random_range(0..10000000).to_string();
                this.variables.insert(name.clone(), value);

                new_vec.push(RuntimeValue::Ref(name, typ));
            }
            new_vec
        };

        let get_new_map = |this: &mut Self,
                           map: HashMap<String, RuntimeValue>|
         -> HashMap<String, RuntimeValue> {
            let mut new_map = HashMap::new();

            for (k, v) in map {
                let typ = (&v).into();

                let value = this.convert_runtime_var_into_saveable(Variable {
                    value: v,
                    var_type: var_type.clone(),
                });
                let name = rand::random_range(0..10000000).to_string();
                this.variables.insert(name.clone(), value);

                new_map.insert(k.clone(), RuntimeValue::Ref(name, typ));
            }

            new_map
        };

        let get_singular = |this: &mut Self, value: RuntimeValue| -> RuntimeValue {
            let typ = (&value).into();
            let value = this.convert_runtime_var_into_saveable(Variable {
                value: value,
                var_type: var_type.clone(),
            });
            let name = rand::random_range(0..10000000).to_string();
            this.variables.insert(name.clone(), value);

            let value = RuntimeValue::Ref(name, typ);

            value
        };

        match value.value {
            RuntimeValue::Aggregate(x, ObjectMap(map)) => Variable {
                value: RuntimeValue::Aggregate(x, ObjectMap(get_new_map(self, map))),
                var_type,
            },
            RuntimeValue::Enum(x, y, Some(ObjectMap(map))) => Variable {
                value: RuntimeValue::Enum(x, y, Some(ObjectMap(get_new_map(self, map)))),
                var_type,
            },
            RuntimeValue::Option(Some(data), typ) => Variable {
                value: RuntimeValue::Option(Some(Box::new(get_singular(self, *data))), typ),
                var_type,
            },
            RuntimeValue::Result(Ok(data), typ) => Variable {
                value: RuntimeValue::Result(Ok(Box::new(get_singular(self, *data))), typ),
                var_type,
            },
            RuntimeValue::Result(Err(data), typ) => Variable {
                value: RuntimeValue::Result(Err(Box::new(get_singular(self, *data))), typ),
                var_type,
            },
            RuntimeValue::List { data, data_type } => Variable {
                value: RuntimeValue::List {
                    data: get_new_list(self, data),
                    data_type,
                },
                var_type,
            },
            _ => value,
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
        let var = {
            let value = self.variables.get(pointer).unwrap();

            let pointer: Option<String> = if let RuntimeValue::Ref(pointer, _) = &value.value {
                Some(pointer.to_string())
            } else {
                None
            };

            (pointer, value.var_type.clone())
        };

        if let Some(pointer) = var.0 {
            return self.assign_var_from_ref_pointer(&pointer, value);
        }

        let value = self.convert_runtime_var_into_saveable(Variable {
            value,
            var_type: var.1.clone(),
        });

        if var.1 != VarType::Mutable {
            Err(ScopeErr::AssignConstant(pointer.to_string()))
        } else {
            self.variables.insert(pointer.to_string(), value);

            Ok(())
        }
    }

    pub fn get_var_ref(&self, name: &str) -> Result<RuntimeValue, ScopeErr> {
        if let Some(typ) = self.variables.get(name).map(|x| (&x.value).into()) {
            Ok(RuntimeValue::Ref(name.to_string(), typ))
        } else {
            Err(ScopeErr::Variable(name.to_string()))
        }
    }

    pub fn get_member_ref(&self, keys: &[MemberPathType]) -> Result<RuntimeValue, InterpreterErr> {
        let first = self.get_var_ref({
            let Some(MemberPathType::Dot(first)) = keys.get(0) else {
                panic!()
            };

            first
        })?;

        if keys.len() <= 1 {
            return Ok(first);
        }

        let RuntimeValue::Ref(mut pointer, _) = first else {
            panic!()
        };

        for key in keys.iter().skip(1) {
            match (&self.variables.get(&pointer).unwrap().value, key) {
                (RuntimeValue::Aggregate(_, ObjectMap(map)), _) => {
                    match map.get(&key.to_string()) {
                        Some(RuntimeValue::Ref(p, _)) => pointer = p.clone(),
                        _ => break,
                    }
                }
                (RuntimeValue::Enum(_, _, Some(ObjectMap(map))), MemberPathType::Dot(key)) => {
                    match map.get(key) {
                        Some(RuntimeValue::Ref(p, _)) => pointer = p.clone(),
                        _ => break,
                    }
                }
                (RuntimeValue::List { data, data_type: _ }, MemberPathType::Computed(key)) => {
                    match data.get(key.parse::<usize>().unwrap()) {
                        Some(RuntimeValue::Ref(p, _)) => pointer = p.clone(),
                        x => {
                            println!("why {x:?}");
                            break;
                        }
                    }
                }

                (
                    RuntimeValue::Option(Some(data), _)
                    | RuntimeValue::Result(Ok(data), _)
                    | RuntimeValue::Result(Err(data), _),
                    _,
                ) => match &**data {
                    RuntimeValue::Ref(p, _) => pointer = p.clone(),
                    _ => break,
                },
                (RuntimeValue::Ref(x, _), _) => pointer = x.clone(),
                _ => unimplemented!(),
            }
        }

        let typ = (&self.variables.get(&pointer).unwrap().value).into();
        Ok(RuntimeValue::Ref(pointer, typ))
    }

    pub fn progress_var(
        &self,
        value: &RuntimeValue,
        key: &str,
    ) -> Result<RuntimeValue, InterpreterErr> {
        match value {
            RuntimeValue::Ref(pointer, _) => {
                self.progress_var(&self.variables.get(pointer).unwrap().value, key)
            }
            RuntimeValue::Aggregate(_, ObjectMap(map))
            | RuntimeValue::Enum(_, _, Some(ObjectMap(map))) => match map.get(key) {
                Some(x) => Ok(x.clone()),
                _ => panic!(),
            },
            RuntimeValue::List { data, data_type: _ } => {
                match data.get(key.parse::<usize>().unwrap()) {
                    Some(x) => Ok(x.clone()),
                    _ => panic!(),
                }
            }
            RuntimeValue::Option(Some(data), _)
            | RuntimeValue::Result(Ok(data), _)
            | RuntimeValue::Result(Err(data), _) => Ok(*data.clone()),
            _ => panic!(),
        }
    }

    pub fn update_var<F>(&mut self, name: &str, mut f: F) -> Result<(), ScopeErr>
    where
        F: FnMut(&mut Variable<RuntimeValue>),
    {
        if let Some(value) = self.variables.get_mut(name) {
            Ok(f(value))
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

        if let Some(var) = self.variables.remove(name) {
            match var.var_type {
                VarType::Mutable => {}
                _ => return Err(ScopeErr::AssignConstant(name.to_string()).into()),
            }
            if var.value.is_type(self, &typ) {
                let new_var = self.convert_runtime_var_into_saveable(Variable {
                    value,
                    var_type: VarType::Mutable,
                });

                self.variables.insert(name.to_string(), new_var);
                Ok(RuntimeValue::Ref(name.to_string(), typ))
            } else {
                Err(ScopeErr::TypeMismatch(name.to_string()).into())
            }
        } else {
            Err(ScopeErr::Variable(name.to_string()).into())
        }
    }
}
