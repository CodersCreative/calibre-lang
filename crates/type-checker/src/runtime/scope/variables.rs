use std::collections::HashMap;

use calibre_common::{
    environment::{ Scope, Variable},
    errors::ScopeErr,
};
use calibre_parser::ast::{ObjectType, VarType};

use crate::runtime::{interpreter::InterpreterErr, scope::CheckerEnvironment, values::RuntimeType};

impl CheckerEnvironment {
    pub fn force_var(
        &mut self,
        scope: &u64,
        key: String,
        value: Variable<RuntimeType>,
    ) -> Result<RuntimeType, ScopeErr<RuntimeType>> {
        let typ = (&value.value).into();
        let counter = self.counter.clone();

        let value = self.convert_runtime_var_into_saveable(value);
        self.scopes
            .get_mut(scope)
            .unwrap()
            .variables
            .insert(key, counter);
        self.variables.insert(counter, value);
        self.counter += 1;

        Ok(RuntimeType::Ref(self.counter - 1, typ))
    }

    fn convert_saveable_into_runtime_var(
        &self,
        value: Variable<RuntimeType>,
    ) -> Variable<RuntimeType> {
        let var_type = value.var_type.clone();
        let location = value.location.clone();

        let mut get_new_list = |list: Vec<RuntimeType>| -> Vec<RuntimeType> {
            let mut new_vec = Vec::new();

            for v in list {
                new_vec.push(match v {
                    RuntimeType::Ref(pointer, _) => {
                        self.variables.get(&pointer).unwrap().value.clone()
                    }
                    _ => v,
                });
            }
            new_vec
        };

        let mut get_new_map = |map: HashMap<String, RuntimeType>| -> HashMap<String, RuntimeType> {
            let mut new_map = HashMap::new();

            for (k, v) in map {
                new_map.insert(
                    k.clone(),
                    match v {
                        RuntimeType::Ref(pointer, _) => {
                            self.variables.get(&pointer).unwrap().value.clone()
                        }
                        _ => v,
                    },
                );
            }

            new_map
        };

        match value.value {
            RuntimeType::Struct(x, y, ObjectType::Map(map)) => Variable {
                value: RuntimeType::Struct(x, y, ObjectType::Map(get_new_map(map))),
                var_type,
                location,
            },
            RuntimeType::Struct(x, y, ObjectType::Tuple(vec)) => Variable {
                value: RuntimeType::Struct(x, y, ObjectType::Tuple(get_new_list(vec))),
                var_type,
                location,
            },
            RuntimeType::Tuple(vec) => Variable {
                value: RuntimeType::Tuple(get_new_list(vec)),
                var_type,
                location,
            },
            RuntimeType::Enum(x, y, Some(ObjectType::Map(map))) => Variable {
                value: RuntimeType::Enum(x, y, Some(ObjectType::Map(get_new_map(map)))),
                var_type,
                location,
            }
            RuntimeType::Enum(x, y, Some(ObjectType::Tuple(vec))) => Variable {
                value: RuntimeType::Enum(x, y, Some(ObjectType::Tuple(get_new_list(vec)))),
                var_type,
                location,
            }
            _ => value,
        }
    }

    fn convert_runtime_var_into_saveable(
        &mut self,
        value: Variable<RuntimeType>,
    ) -> Variable<RuntimeType> {
        let var_type = value.var_type.clone();
        let location = value.location.clone();

        let mut get_new_list = |this: &mut Self, list: Vec<RuntimeType>| -> Vec<RuntimeType> {
            let mut new_vec = Vec::new();

            for v in list {
                let typ = v.clone();
                let counter = this.counter;
                this.variables.insert(
                    counter,
                    Variable {
                        value: v,
                        var_type: var_type.clone(),
                        location: location.clone(),
                    },
                );

                new_vec.push(RuntimeType::Ref(this.counter.clone(), Box::new(typ)));

                this.counter += 1;
            }
            new_vec
        };

        let get_new_map = |this: &mut Self,
                           map: HashMap<String, RuntimeType>|
         -> HashMap<String, RuntimeType> {
            let mut new_map = HashMap::new();

            for (k, v) in map {
                let typ = v.clone();
                let counter = this.counter;
                this.variables.insert(
                    counter,
                    Variable {
                        value: v,
                        var_type: var_type.clone(),
                        location: location.clone(),
                    },
                );

                new_map.insert(k.clone(), RuntimeType::Ref(this.counter.clone(), Box::new(typ)));

                this.counter += 1;
            }

            new_map
        };

        match value.value {
            RuntimeType::Struct(x, y, ObjectType::Map(map)) => Variable {
                value: RuntimeType::Struct(x, y, ObjectType::Map(get_new_map(self, map))),
                var_type,
                location,
            },
            RuntimeType::Struct(x, y, ObjectType::Tuple(vec)) => Variable {
                value: RuntimeType::Struct(x, y, ObjectType::Tuple(get_new_list(self, vec))),

                var_type,
                location,
            },
            RuntimeType::Tuple(vec) => Variable {
                value: RuntimeType::Tuple(get_new_list(self, vec)),
                var_type,
                location,
            },
            RuntimeType::Enum(x, y,  Some(ObjectType::Map(map))) => Variable {
                value: RuntimeType::Enum(x, y, Some(ObjectType::Map(get_new_map(self, map)))),
                var_type,
                location,
            },
            RuntimeType::Enum(x, y,  Some(ObjectType::Tuple(vec))) => Variable {
                value: RuntimeType::Enum(
                    x,
                    y,
                                   Some(ObjectType::Tuple(get_new_list(self, vec))),
                ),
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
        value: Variable<RuntimeType>,
    ) -> Result<RuntimeType, ScopeErr<RuntimeType>> {
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
        value: RuntimeType,
    ) -> Result<RuntimeType, ScopeErr<RuntimeType>> {
        let var = self.variables.get(pointer).unwrap().clone();
        let value = self.convert_runtime_var_into_saveable(Variable {
            value,
            var_type: var.var_type.clone(),
            location: var.location,
        });

        let typ = value.value.clone();

        if var.var_type != VarType::Mutable {
            Err(ScopeErr::AssignConstant(pointer.to_string()))
        } else {
            self.variables.insert(*pointer, value);

            Ok(RuntimeType::Ref(*pointer, Box::new(typ)))
        }
    }


    pub fn get_value_from_ref_pointer(
        &self,
        pointer: &u64,
    ) -> Result<Variable<RuntimeType>, ScopeErr<RuntimeType>> {
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
    ) -> Result<RuntimeType, InterpreterErr> {
        let first = self.get_var_ref(scope, keys.get(0).unwrap())?;

        if keys.len() <= 1 {
            return Ok(first);
        }

        let Runtime::Ref(mut pointer, _) = first else {
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

    pub fn get_var_ref(
        &self,
        scope: &u64,
        key: &str,
    ) -> Result<RuntimeType, ScopeErr<RuntimeType>> {
        if let Some(pointer) = self
            .scopes
            .get(scope)
            .unwrap()
            .variables
            .get(key)
            .map(|x| x.clone())
        {
            let typ = (&self.variables.get(&pointer).unwrap().value).into();
            Ok(RuntimeType::Ref(pointer, typ))
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
    ) -> Result<&'a Variable<RuntimeType>, ScopeErr<RuntimeType>> {
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
    ) -> Result<(), ScopeErr<RuntimeType>>
    where
        F: FnMut(&mut Variable<RuntimeType>),
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
        value: RuntimeType,
    ) -> Result<RuntimeType, InterpreterErr> {
        let typ: RuntimeType = value.clone();

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
                if var.value.is_type(&typ) {
                    let new_var = self.convert_runtime_var_into_saveable(Variable {
                        value,
                        var_type: VarType::Mutable,
                        location: var.location.clone(),
                    });

                    self.variables.insert(pointer, new_var);
                    Ok(RuntimeType::Ref(pointer, Box::new(typ)))
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

    pub fn get_global_scope<'a>(&'a self) -> &'a Scope {
        self.scopes.get(&0).unwrap()
    }
}
