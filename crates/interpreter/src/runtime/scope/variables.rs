use calibre_common::{environment::Variable, errors::ScopeErr};
use calibre_parser::ast::{ObjectType, VarType};

use crate::runtime::{
    interpreter::InterpreterErr,
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
    ) -> Result<RuntimeValue, ScopeErr<RuntimeValue>> {
        let typ = (&value.value).into();
        let counter = self.var_counter.clone();

        let value = self.convert_runtime_var_into_saveable(value);
        self.scopes
            .get_mut(scope)
            .unwrap()
            .variables
            .insert(key, counter);
        self.variables.insert(counter, value);
        self.var_counter += 1;

        Ok(RuntimeValue::Ref(self.var_counter - 1, typ))
    }

    fn convert_saveable_into_runtime_var(
        &self,
        value: Variable<RuntimeValue>,
    ) -> Variable<RuntimeValue> {
        let var_type = value.var_type.clone();
        let location = value.location.clone();

        let mut get_new_list = |list: Vec<RuntimeValue>| -> Vec<RuntimeValue> {
            let mut new_vec = Vec::new();

            for v in list {
                new_vec.push(match v {
                    RuntimeValue::Ref(pointer, _) => {
                        self.variables.get(&pointer).unwrap().value.clone()
                    }
                    _ => v,
                });
            }
            new_vec
        };

        let mut get_new_map =
            |map: HashMap<String, RuntimeValue>| -> HashMap<String, RuntimeValue> {
                let mut new_map = HashMap::new();

                for (k, v) in map {
                    new_map.insert(
                        k.clone(),
                        match v {
                            RuntimeValue::Ref(pointer, _) => {
                                self.variables.get(&pointer).unwrap().value.clone()
                            }
                            _ => v,
                        },
                    );
                }

                new_map
            };

        let mut get_singular = |value: RuntimeValue| -> RuntimeValue {
            match value {
                RuntimeValue::Ref(pointer, _) => {
                    self.variables.get(&pointer).unwrap().value.clone()
                }
                _ => value,
            }
        };

        let mut runtime_value = value.value;
        let mut unwrapped = false;

        loop {
            runtime_value = match runtime_value {
                RuntimeValue::Struct(x, y, ObjectType::Map(map)) => {
                    RuntimeValue::Struct(x, y, ObjectType::Map(get_new_map(map)))
                }
                RuntimeValue::Struct(x, y, ObjectType::Tuple(vec)) => {
                    RuntimeValue::Struct(x, y, ObjectType::Tuple(get_new_list(vec)))
                }
                RuntimeValue::Tuple(vec) => RuntimeValue::Tuple(get_new_list(vec)),
                RuntimeValue::Enum(x, y, z, Some(ObjectType::Map(map))) => {
                    RuntimeValue::Enum(x, y, z, Some(ObjectType::Map(get_new_map(map))))
                }
                RuntimeValue::Option(Some(data), typ) => {
                    RuntimeValue::Option(Some(Box::new(get_singular(*data))), typ)
                }
                RuntimeValue::Result(Ok(data), typ) => {
                    RuntimeValue::Result(Ok(Box::new(get_singular(*data))), typ)
                }
                RuntimeValue::Result(Err(data), typ) => {
                    RuntimeValue::Result(Err(Box::new(get_singular(*data))), typ)
                }
                RuntimeValue::Enum(x, y, z, Some(ObjectType::Tuple(vec))) => {
                    RuntimeValue::Enum(x, y, z, Some(ObjectType::Tuple(get_new_list(vec))))
                }
                RuntimeValue::List { data, data_type } => RuntimeValue::List {
                    data: get_new_list(data),
                    data_type,
                },
                RuntimeValue::Ref(pointer, _) if !unwrapped => {
                    runtime_value = self.variables.get(&pointer).unwrap().value.clone();
                    unwrapped = true;
                    continue;
                }
                x => x,
            };

            break;
        }

        Variable {
            value: runtime_value,
            var_type,
            location,
        }
    }

    fn convert_runtime_var_into_saveable(
        &mut self,
        value: Variable<RuntimeValue>,
    ) -> Variable<RuntimeValue> {
        let var_type = value.var_type.clone();
        let location = value.location.clone();

        let mut get_new_list = |this: &mut Self, list: Vec<RuntimeValue>| -> Vec<RuntimeValue> {
            let mut new_vec = Vec::new();

            for v in list {
                let typ = (&v).into();
                let counter = this.var_counter;
                this.variables.insert(
                    counter,
                    Variable {
                        value: v,
                        var_type: var_type.clone(),
                        location: location.clone(),
                    },
                );

                new_vec.push(RuntimeValue::Ref(this.var_counter.clone(), typ));

                this.var_counter += 1;
            }
            new_vec
        };

        let get_new_map = |this: &mut Self,
                           map: HashMap<String, RuntimeValue>|
         -> HashMap<String, RuntimeValue> {
            let mut new_map = HashMap::new();

            for (k, v) in map {
                let typ = (&v).into();
                let counter = this.var_counter;
                this.variables.insert(
                    counter,
                    Variable {
                        value: v,
                        var_type: var_type.clone(),
                        location: location.clone(),
                    },
                );

                new_map.insert(k.clone(), RuntimeValue::Ref(this.var_counter.clone(), typ));

                this.var_counter += 1;
            }

            new_map
        };

        let get_singular = |this: &mut Self, value: RuntimeValue| -> RuntimeValue {
            let typ = (&value).into();
            let counter = this.var_counter;
            this.variables.insert(
                counter,
                Variable {
                    value: value,
                    var_type: var_type.clone(),
                    location: location.clone(),
                },
            );

            let value = RuntimeValue::Ref(this.var_counter, typ);
            this.var_counter += 1;

            value
        };

        match value.value {
            RuntimeValue::Struct(x, y, ObjectType::Map(map)) => Variable {
                value: RuntimeValue::Struct(x, y, ObjectType::Map(get_new_map(self, map))),
                var_type,
                location,
            },
            RuntimeValue::Struct(x, y, ObjectType::Tuple(vec)) => Variable {
                value: RuntimeValue::Struct(x, y, ObjectType::Tuple(get_new_list(self, vec))),
                var_type,
                location,
            },
            RuntimeValue::Tuple(vec) => Variable {
                value: RuntimeValue::Tuple(get_new_list(self, vec)),
                var_type,
                location,
            },
            RuntimeValue::Enum(x, y, z, Some(ObjectType::Map(map))) => Variable {
                value: RuntimeValue::Enum(x, y, z, Some(ObjectType::Map(get_new_map(self, map)))),
                var_type,
                location,
            },
            RuntimeValue::Option(Some(data), typ) => Variable {
                value: RuntimeValue::Option(Some(Box::new(get_singular(self, *data))), typ),
                var_type,
                location,
            },
            RuntimeValue::Result(Ok(data), typ) => Variable {
                value: RuntimeValue::Result(Ok(Box::new(get_singular(self, *data))), typ),
                var_type,
                location,
            },
            RuntimeValue::Result(Err(data), typ) => Variable {
                value: RuntimeValue::Result(Err(Box::new(get_singular(self, *data))), typ),
                var_type,
                location,
            },
            RuntimeValue::Enum(x, y, z, Some(ObjectType::Tuple(vec))) => Variable {
                value: RuntimeValue::Enum(
                    x,
                    y,
                    z,
                    Some(ObjectType::Tuple(get_new_list(self, vec))),
                ),
                var_type,
                location,
            },
            RuntimeValue::List { data, data_type } => Variable {
                value: RuntimeValue::List {
                    data: get_new_list(self, data),
                    data_type,
                },
                var_type,
                location,
            },
            _ => value,
        }
    }

    pub fn push_var(
        &mut self,
        scope: &u64,
        key: String,
        value: Variable<RuntimeValue>,
    ) -> Result<RuntimeValue, ScopeErr<RuntimeValue>> {
        if let Some(var) = self.scopes.get(scope).unwrap().variables.get(&key) {
            if self.variables.get(var).unwrap().var_type == VarType::Constant {
                return Err(ScopeErr::AssignConstant(key));
            }
        };

        self.force_var(scope, key, value)
    }

    pub fn assign_var_from_ref_pointer(
        &mut self,
        pointer: &u64,
        value: RuntimeValue,
    ) -> Result<RuntimeValue, ScopeErr<RuntimeValue>> {
        let var = {
            let value = self.variables.get(pointer).unwrap();

            let pointer = if let RuntimeValue::Ref(pointer, _) = value.value {
                Some(pointer.clone())
            } else {
                None
            };

            (pointer, value.var_type.clone(), value.location.clone())
        };

        if let Some(pointer) = var.0 {
            return self.assign_var_from_ref_pointer(&pointer, value);
        }

        let value = self.convert_runtime_var_into_saveable(Variable {
            value,
            var_type: var.1.clone(),
            location: var.2,
        });

        let typ = (&value.value).into();

        if var.1 != VarType::Mutable {
            Err(ScopeErr::AssignConstant(pointer.to_string()))
        } else {
            self.variables.insert(*pointer, value);

            Ok(RuntimeValue::Ref(*pointer, typ))
        }
    }

    pub fn get_value_from_ref_pointer(
        &self,
        pointer: &u64,
    ) -> Result<Variable<RuntimeValue>, ScopeErr<RuntimeValue>> {
        if let Some(var) = self.variables.get(&pointer).map(|x| x.clone()) {
            Ok(self.convert_saveable_into_runtime_var(var))
        } else {
            Err(ScopeErr::Variable(format!("pointer : {}", pointer)))
        }
    }

    pub fn get_member_ref(
        &self,
        scope: &u64,
        keys: &[String],
    ) -> Result<RuntimeValue, InterpreterErr> {
        let first = self.get_var_ref(scope, keys.get(0).unwrap())?;

        if keys.len() <= 1 {
            return Ok(first);
        }

        let RuntimeValue::Ref(mut pointer, _) = first else {
            panic!()
        };

        for key in keys.iter().skip(1) {
            match &self.variables.get(&pointer).unwrap().value {
                RuntimeValue::Struct(_, _, ObjectType::Map(map))
                | RuntimeValue::Enum(_, _, _, Some(ObjectType::Map(map))) => match map.get(key) {
                    Some(RuntimeValue::Ref(p, _)) => pointer = p.clone(),
                    _ => break,
                },
                RuntimeValue::List { data, data_type: _ }
                | RuntimeValue::Struct(_, _, ObjectType::Tuple(data))
                | RuntimeValue::Enum(_, _, _, Some(ObjectType::Tuple(data)))
                | RuntimeValue::Tuple(data) => match data.get(key.parse::<usize>().unwrap()) {
                    Some(RuntimeValue::Ref(p, _)) => pointer = p.clone(),
                    _ => break,
                },
                RuntimeValue::Option(Some(data), _)
                | RuntimeValue::Result(Ok(data), _)
                | RuntimeValue::Result(Err(data), _) => match **data {
                    RuntimeValue::Ref(p, _) => pointer = p.clone(),
                    _ => break,
                },
                RuntimeValue::Ref(x, _) => pointer = x.clone(),
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
                self.progress_var(&self.variables.get(&pointer).unwrap().value, key)
            }
            RuntimeValue::Struct(_, _, ObjectType::Map(map))
            | RuntimeValue::Enum(_, _, _, Some(ObjectType::Map(map))) => match map.get(key) {
                Some(x) => Ok(x.clone()),
                _ => panic!(),
            },
            RuntimeValue::List { data, data_type: _ }
            | RuntimeValue::Struct(_, _, ObjectType::Tuple(data))
            | RuntimeValue::Enum(_, _, _, Some(ObjectType::Tuple(data)))
            | RuntimeValue::Tuple(data) => match data.get(key.parse::<usize>().unwrap()) {
                Some(x) => Ok(x.clone()),
                _ => panic!(),
            },
            RuntimeValue::Option(Some(data), _)
            | RuntimeValue::Result(Ok(data), _)
            | RuntimeValue::Result(Err(data), _) => Ok(*data.clone()),
            _ => panic!(),
        }
    }

    pub fn get_var_ref(
        &self,
        scope: &u64,
        key: &str,
    ) -> Result<RuntimeValue, ScopeErr<RuntimeValue>> {
        if let Some(pointer) = self
            .scopes
            .get(scope)
            .unwrap()
            .variables
            .get(key)
            .map(|x| x.clone())
        {
            let typ = (&self.variables.get(&pointer).unwrap().value).into();
            Ok(RuntimeValue::Ref(pointer, typ))
        } else if let Some(scope) = self.scopes.get(scope).unwrap().parent {
            self.get_var_ref(&scope, key)
        } else {
            Err(ScopeErr::Variable(key.to_string()))
        }
    }

    pub fn get_var<'a>(
        &'a self,
        scope: &u64,
        key: &str,
    ) -> Result<&'a Variable<RuntimeValue>, ScopeErr<RuntimeValue>> {
        if let Some(pointer) = self
            .scopes
            .get(scope)
            .unwrap()
            .variables
            .get(key)
            .map(|x| x.clone())
        {
            Ok(self.variables.get(&pointer).unwrap())
        } else if let Some(scope) = self.scopes.get(scope).unwrap().parent {
            self.get_var(&scope, key)
        } else {
            Err(ScopeErr::Variable(key.to_string()))
        }
    }

    pub fn update_var<F>(
        &mut self,
        scope: &u64,
        key: &str,
        mut f: F,
    ) -> Result<(), ScopeErr<RuntimeValue>>
    where
        F: FnMut(&mut Variable<RuntimeValue>),
    {
        if let Some(pointer) = self
            .scopes
            .get(scope)
            .unwrap()
            .variables
            .get(key)
            .map(|x| x.clone())
        {
            Ok(f(self.variables.get_mut(&pointer).unwrap()))
        } else if let Some(scope) = self.scopes.get(scope).unwrap().parent {
            self.update_var(&scope, key, f)
        } else {
            Err(ScopeErr::Variable(key.to_string()))
        }
    }

    pub fn assign_var(
        &mut self,
        scope: &u64,
        key: &str,
        value: RuntimeValue,
    ) -> Result<RuntimeValue, InterpreterErr> {
        let typ: RuntimeType = (&value).into();

        if let Some(pointer) = self
            .scopes
            .get(scope)
            .unwrap()
            .variables
            .get(key)
            .map(|x| x.clone())
        {
            if let Some(var) = self.variables.remove(&pointer) {
                match var.var_type {
                    VarType::Mutable => {}
                    _ => return Err(ScopeErr::AssignConstant(key.to_string()).into()),
                }
                if var.value.is_type(self, scope, &typ) {
                    let new_var = self.convert_runtime_var_into_saveable(Variable {
                        value,
                        var_type: VarType::Mutable,
                        location: var.location.clone(),
                    });

                    self.variables.insert(pointer, new_var);
                    Ok(RuntimeValue::Ref(pointer, typ))
                } else {
                    Err(ScopeErr::TypeMismatch(var.value.clone(), value.clone()).into())
                }
            } else {
                Err(ScopeErr::Variable(key.to_string()).into())
            }
        } else if let Some(scope) = self.scopes.get(scope).unwrap().parent {
            self.assign_var(&scope, key, value)
        } else {
            Err(ScopeErr::Variable(key.to_string()).into())
        }
    }
}
