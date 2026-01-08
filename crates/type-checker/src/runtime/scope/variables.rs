use std::collections::HashMap;

use calibre_common::{
    environment::{Scope, Variable},
    errors::ScopeErr,
};
use calibre_parser::ast::{ObjectType, VarType};

use crate::runtime::{
    interpreter::{InterpreterErr, expressions::member::MemberPathType},
    scope::CheckerEnvironment,
    values::RuntimeType,
};

impl CheckerEnvironment {
    pub fn force_var(
        &mut self,
        scope: &u64,
        key: String,
        value: Variable<RuntimeType>,
    ) -> Result<RuntimeType, ScopeErr> {
        let typ = value.value.clone();
        let counter = self.var_counter.clone();

        let value = self.convert_runtime_var_into_saveable(value);
        self.scopes
            .get_mut(scope)
            .unwrap()
            .variables
            .insert(key, counter);
        self.variables.insert(counter, value);
        self.var_counter += 1;

        Ok(RuntimeType::Ref(Box::new(typ)))
    }

    fn _convert_saveable_into_runtime_var(
        &self,
        value: Variable<RuntimeType>,
    ) -> Variable<RuntimeType> {
        let var_type = value.var_type.clone();
        let location = value.location.clone();

        let get_new_list = |list: Vec<RuntimeType>| -> Vec<RuntimeType> {
            let mut new_vec = Vec::new();

            for v in list {
                new_vec.push(match v {
                    RuntimeType::Ref(t) => *t.clone(),
                    _ => v,
                });
            }
            new_vec
        };

        let get_new_map = |map: HashMap<String, RuntimeType>| -> HashMap<String, RuntimeType> {
            let mut new_map = HashMap::new();

            for (k, v) in map {
                new_map.insert(
                    k.clone(),
                    match v {
                        RuntimeType::Ref(t) => *t.clone(),
                        _ => v,
                    },
                );
            }

            new_map
        };

        match value.value {
            RuntimeType::Struct(x, ObjectType::Map(map)) => Variable {
                value: RuntimeType::Struct(x, ObjectType::Map(get_new_map(map))),
                var_type,
                location,
            },
            RuntimeType::Struct(x, ObjectType::Tuple(vec)) => Variable {
                value: RuntimeType::Struct(x, ObjectType::Tuple(get_new_list(vec))),
                var_type,
                location,
            },
            RuntimeType::Tuple(vec) => Variable {
                value: RuntimeType::Tuple(get_new_list(vec)),
                var_type,
                location,
            },
            RuntimeType::Enum(x, Some(ObjectType::Map(map))) => Variable {
                value: RuntimeType::Enum(x, Some(ObjectType::Map(get_new_map(map)))),
                var_type,
                location,
            },
            RuntimeType::Enum(x, Some(ObjectType::Tuple(vec))) => Variable {
                value: RuntimeType::Enum(x, Some(ObjectType::Tuple(get_new_list(vec)))),
                var_type,
                location,
            },
            _ => value,
        }
    }

    fn convert_runtime_var_into_saveable(
        &mut self,
        value: Variable<RuntimeType>,
    ) -> Variable<RuntimeType> {
        let var_type = value.var_type.clone();
        let location = value.location.clone();

        let get_new_list = |_this: &mut Self, list: Vec<RuntimeType>| -> Vec<RuntimeType> {
            let mut new_vec = Vec::new();

            for v in list {
                new_vec.push(RuntimeType::Ref(Box::new(v.clone())));
            }
            new_vec
        };

        let get_new_map =
            |_this: &mut Self, map: HashMap<String, RuntimeType>| -> HashMap<String, RuntimeType> {
                let mut new_map = HashMap::new();

                for (k, v) in map {
                    new_map.insert(k.clone(), RuntimeType::Ref(Box::new(v.clone())));
                }

                new_map
            };

        match value.value {
            RuntimeType::Struct(x, ObjectType::Map(map)) => Variable {
                value: RuntimeType::Struct(x, ObjectType::Map(get_new_map(self, map))),
                var_type,
                location,
            },
            RuntimeType::Struct(x, ObjectType::Tuple(vec)) => Variable {
                value: RuntimeType::Struct(x, ObjectType::Tuple(get_new_list(self, vec))),

                var_type,
                location,
            },
            RuntimeType::Tuple(vec) => Variable {
                value: RuntimeType::Tuple(get_new_list(self, vec)),
                var_type,
                location,
            },
            RuntimeType::Enum(x, Some(ObjectType::Map(map))) => Variable {
                value: RuntimeType::Enum(x, Some(ObjectType::Map(get_new_map(self, map)))),
                var_type,
                location,
            },
            RuntimeType::Enum(x, Some(ObjectType::Tuple(vec))) => Variable {
                value: RuntimeType::Enum(x, Some(ObjectType::Tuple(get_new_list(self, vec)))),
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
    ) -> Result<RuntimeType, ScopeErr> {
        if let Some(var) = self.scopes.get(scope).unwrap().variables.get(&key) {
            if self.variables.get(var).unwrap().var_type == VarType::Constant {
                return Err(ScopeErr::AssignConstant(key));
            }
        };

        self.force_var(scope, key, value)
    }
    pub fn get_member_ref(
        &self,
        scope: &u64,
        keys: &[MemberPathType],
    ) -> Result<RuntimeType, InterpreterErr> {
        let first = self.get_var_ref(scope, {
            let Some(MemberPathType::Dot(first)) = keys.get(0) else {
                panic!()
            };

            first
        })?;

        if keys.len() <= 1 {
            return Ok(first);
        }

        let RuntimeType::Ref(val) = first else {
            panic!()
        };

        let mut val = *val;
        for key in keys.iter().skip(1) {
            match (val.clone(), key) {
                (RuntimeType::Struct(_, ObjectType::Map(map)), _) => {
                    match map.get(&key.to_string()) {
                        Some(RuntimeType::Ref(p)) => val = *p.clone(),
                        Some(x) => val = x.clone(),
                        _ => break,
                    }
                }
                (RuntimeType::Enum(_, Some(ObjectType::Map(map))), MemberPathType::Dot(key)) => {
                    match map.get(key) {
                        Some(RuntimeType::Ref(p)) => val = *p.clone(),
                        Some(x) => val = x.clone(),
                        _ => break,
                    }
                }

                (RuntimeType::Struct(_, ObjectType::Tuple(data)), _) => {
                    match data.get(key.to_string().parse::<usize>().unwrap()) {
                        Some(RuntimeType::Ref(p)) => val = *p.clone(),
                        Some(x) => val = x.clone(),
                        _ => break,
                    }
                }

                (
                    RuntimeType::Enum(_, Some(ObjectType::Tuple(data))) | RuntimeType::Tuple(data),
                    MemberPathType::Dot(key),
                ) => match data.get(key.parse::<usize>().unwrap()) {
                    Some(RuntimeType::Ref(p)) => val = *p.clone(),
                    Some(x) => val = x.clone(),
                    _ => break,
                },
                (RuntimeType::List(x), MemberPathType::Computed(_)) => match x {
                    Some(x) => val = *x.clone(),
                    None => val = RuntimeType::Dynamic,
                },
                (RuntimeType::Result(x, _), _) if key.to_string() == "Err" => val = *x,
                (RuntimeType::Result(_, x), _) if key.to_string() == "Ok" => val = *x,
                (RuntimeType::Option(x), _) if key.to_string() == "Some" => val = *x,
                (RuntimeType::Ref(x), _) => val = *x.clone(),
                _ => unimplemented!(),
            }
        }
        Ok(RuntimeType::Ref(Box::new(val)))
    }

    pub fn get_var_ref(&self, scope: &u64, key: &str) -> Result<RuntimeType, ScopeErr> {
        let pointer = self.get_var_pointer(scope, key)?;
        if let Some(typ) = self.variables.get(&pointer).map(|x| x.value.clone()) {
            Ok(RuntimeType::Ref(Box::new(typ)))
        } else {
            Err(ScopeErr::Variable(pointer.to_string()))
        }
    }

    pub fn update_var<F>(&mut self, pointer: &u64, mut f: F) -> Result<(), ScopeErr>
    where
        F: FnMut(&mut Variable<RuntimeType>),
    {
        if let Some(value) = self.variables.get_mut(pointer) {
            Ok(f(value))
        } else {
            Err(ScopeErr::Variable(pointer.to_string()))
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
                    Ok(RuntimeType::Ref(Box::new(typ)))
                } else {
                    Err(ScopeErr::TypeMismatch(key.to_string()).into())
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
