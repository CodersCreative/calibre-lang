use calibre_common::{environment::Variable, errors::ScopeErr};
use calibre_parser::ast::{ObjectType, VarType};

use crate::runtime::{
    interpreter::InterpreterErr, scope::InterpreterEnvironment, values::RuntimeValue,
};
use std::{collections::HashMap, mem::discriminant};

impl InterpreterEnvironment {
    pub fn force_var(
        &mut self,
        scope: &u64,
        key: String,
        value: Variable<RuntimeValue>,
    ) -> Result<RuntimeValue, ScopeErr<RuntimeValue>> {
        let typ = (&value.value).into();
        let counter = self.counter.clone();

        self.scopes
            .get_mut(scope)
            .unwrap()
            .variables
            .insert(key, self.counter);
        self.variables
            .insert(self.counter, self.convert_runtime_var_into_saveable(value));
        self.counter += 1;

        Ok(RuntimeValue::Ref(self.counter - 1, typ))
    }

    fn convert_saveable_into_runtime_var(
        &mut self,
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
            match v {
                RuntimeValue::Ref(pointer, _) => {
                    self.variables.get(&pointer).unwrap().value.clone()
                }
                _ => v,
            }
        };

        match value.value {
            RuntimeValue::Struct(x, y, ObjectType::Map(map)) => Variable {
                value: RuntimeValue::Struct(x, y, ObjectType::Map(get_new_map(map))),
                var_type,
                location,
            },
            RuntimeValue::Struct(x, y, ObjectType::Tuple(vec)) => Variable {
                value: RuntimeValue::Struct(x, y, ObjectType::Tuple(get_new_list(vec))),
                var_type,
                location,
            },
            RuntimeValue::Tuple(vec) => Variable {
                value: RuntimeValue::Tuple(get_new_list(vec)),
                var_type,
                location,
            },
            RuntimeValue::Enum(x, y, z, Some(ObjectType::Map(map))) => Variable {
                value: RuntimeValue::Enum(x, y, z, Some(ObjectType::Map(get_new_map(map)))),
                var_type,
                location,
            },
            RuntimeValue::Option(Some(data), typ) => Variable {
                value: RuntimeValue::Option(Some(Box::new(get_singular(*data))), typ),
                var_type,
                location,
            },
            RuntimeValue::Result(Ok(data), typ) => Variable {
                value: RuntimeValue::Result(Ok(Box::new(get_singular(*data))), typ),
                var_type,
                location,
            },
            RuntimeValue::Result(Err(data), typ) => Variable {
                value: RuntimeValue::Result(Err(Box::new(get_singular(*data))), typ),
                var_type,
                location,
            },
            RuntimeValue::Enum(x, y, z, Some(ObjectType::Tuple(vec))) => Variable {
                value: RuntimeValue::Enum(x, y, z, Some(ObjectType::Tuple(get_new_list(vec)))),
                var_type,
                location,
            },
            RuntimeValue::List { data, data_type } => Variable {
                value: RuntimeValue::List {
                    data: get_new_list(data),
                    data_type,
                },
                var_type,
                location,
            },
            _ => value,
        }
    }

    fn convert_runtime_var_into_saveable(
        &mut self,
        value: Variable<RuntimeValue>,
    ) -> Variable<RuntimeValue> {
        let var_type = value.var_type.clone();
        let location = value.location.clone();

        let mut get_new_list = |list: Vec<RuntimeValue>| -> Vec<RuntimeValue> {
            let mut new_vec = Vec::new();

            for v in list {
                let typ = (&v).into();
                self.variables.insert(
                    self.counter,
                    Variable {
                        value: v,
                        var_type: var_type.clone(),
                        location: location.clone(),
                    },
                );

                new_vec.push(RuntimeValue::Ref(self.counter.clone(), typ));

                self.counter += 1;
            }
            new_vec
        };

        let mut get_new_map =
            |map: HashMap<String, RuntimeValue>| -> HashMap<String, RuntimeValue> {
                let mut new_map = HashMap::new();

                for (k, v) in map {
                    let typ = (&v).into();
                    self.variables.insert(
                        self.counter,
                        Variable {
                            value: v,
                            var_type: var_type.clone(),
                            location: location.clone(),
                        },
                    );

                    new_map.insert(k.clone(), RuntimeValue::Ref(self.counter.clone(), typ));

                    self.counter += 1;
                }

                new_map
            };

        let mut get_singular = |value: RuntimeValue| -> RuntimeValue {
            self.variables.insert(
                self.counter,
                Variable {
                    value: v,
                    var_type: var_type.clone(),
                    location: location.clone(),
                },
            );

            self.counter += 1;
        };

        match value.value {
            RuntimeValue::Struct(x, y, ObjectType::Map(map)) => Variable {
                value: RuntimeValue::Struct(x, y, ObjectType::Map(get_new_map(map))),
                var_type,
                location,
            },
            RuntimeValue::Struct(x, y, ObjectType::Tuple(vec)) => Variable {
                value: RuntimeValue::Struct(x, y, ObjectType::Tuple(get_new_list(vec))),
                var_type,
                location,
            },
            RuntimeValue::Tuple(vec) => Variable {
                value: RuntimeValue::Tuple(get_new_list(vec)),
                var_type,
                location,
            },
            RuntimeValue::Enum(x, y, z, Some(ObjectType::Map(map))) => Variable {
                value: RuntimeValue::Enum(x, y, z, Some(ObjectType::Map(get_new_map(map)))),
                var_type,
                location,
            },
            RuntimeValue::Option(Some(data), typ) => Variable {
                value: RuntimeValue::Option(Some(Box::new(get_singular(*data))), typ),
                var_type,
                location,
            },
            RuntimeValue::Result(Ok(data), typ) => Variable {
                value: RuntimeValue::Result(Ok(Box::new(get_singular(*data))), typ),
                var_type,
                location,
            },
            RuntimeValue::Result(Err(data), typ) => Variable {
                value: RuntimeValue::Result(Err(Box::new(get_singular(*data))), typ),
                var_type,
                location,
            },
            RuntimeValue::Enum(x, y, z, Some(ObjectType::Tuple(vec))) => Variable {
                value: RuntimeValue::Enum(x, y, z, Some(ObjectType::Tuple(get_new_list(vec)))),
                var_type,
                location,
            },
            RuntimeValue::List { data, data_type } => Variable {
                value: RuntimeValue::List {
                    data: get_new_list(data),
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
        let typ = (&value.value).into();

        if let Some(var) = self.scopes.get(scope).unwrap().variables.get(&key) {
            if var.var_type == VarType::Constant {
                return Err(ScopeErr::AssignConstant(key));
            }
        };

        self.force_var(scope, key, value)
    }

    pub fn get_value_from_ref_pointer(
        &'a self,
        scope: &u64,
        pointer: &u64,
    ) -> Result<Variable<RuntimeValue>, ScopeErr<RuntimeValue>> {
        if let Some(var) = self.variables.get(&pointer) {
            Ok(self.convert_saveable_into_runtime_var(var.clone()))
        } else {
            Err(ScopeErr::Variable(format!("pointer : {}", pointer)))
        }
    }

    pub fn get_var_ref<'a>(
        &'a self,
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
            let typ = self.variables.get(&pointer).unwrap().value.into();
            Ok(RuntimeValue::Ref(pointer, typ))
        } else if let Some(scope) = self.scopes.get(scope).unwrap().parent {
            self.get_var_ref(&scope, key)?
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
        let typ = (&value).into();

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
                if var.value.is_type(self, scope, typ) {
                    let new_var = self.convert_runtime_var_into_saveable(Variable {
                        value,
                        var_type: VarType::Mutable,
                        location: var.location.clone(),
                    });

                    self.variables.insert(pointer, new_var)
                    Ok(RuntimeValue::Ref(pointer, typ))
                } else {
                    Err(ScopeErr::TypeMismatch(var.value.clone(), value.clone()).into())
                }
            } else {
                Err(ScopeErr::Variable(key.to_string()))
            }
        } else if let Some(scope) = self.scopes.get(scope).unwrap().parent {
            self.assign_var(&scope, key, value)
        } else {
            Err(ScopeErr::Variable(key.to_string()))
        }
    }
}
