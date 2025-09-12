pub mod children;
pub mod links;
pub mod objects;
pub mod variables;

use super::values::RuntimeType;
use crate::runtime::values::{RuntimeValue, helper::StopValue};
use crate::runtime::{interpreter::InterpreterErr, values::ValueErr};
use calibre_parser::ast::{ObjectType, ParserDataType, TypeDefType, VarType};
use std::{collections::HashMap, fmt::Debug, path::PathBuf};
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum ScopeErr {
    #[error("Unable to resolve variable : {0}.")]
    Variable(String),
    #[error("Unable to assign immutable variable : {0}.")]
    AssignConstant(String),
    #[error("Unable to shadow immutable variable : {0}.")]
    ShadowConstant(String),
    #[error("Variable types dont match : {0:?} and {1:?}.")]
    TypeMismatch(RuntimeValue, RuntimeValue),
    #[error("Unable to resolve object : {0:?}.")]
    Object(String),
    #[error("Unable to resolve static function : {0}.")]
    Function(String),
    #[error("Unable to resolve scope : {0}.")]
    Scope(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Enum(Vec<(String, Option<ObjectType<RuntimeType>>)>),
    Struct(ObjectType<RuntimeType>),
    NewType(RuntimeType),
    Link(u64, String),
}

fn type_def_type_obj_to_runtime(obj: ObjectType<ParserDataType>) -> ObjectType<RuntimeType> {
    match obj {
        ObjectType::Tuple(x) => {
            let mut vec: Vec<RuntimeType> = Vec::new();

            for v in x {
                vec.push(RuntimeType::from(v));
            }

            ObjectType::Tuple(vec)
        }
        ObjectType::Map(x) => {
            let mut map: HashMap<String, RuntimeType> = HashMap::new();
            for (k, v) in x {
                map.insert(k, RuntimeType::from(v));
            }
            ObjectType::Map(map)
        }
    }
}

impl From<TypeDefType> for Type {
    fn from(value: TypeDefType) -> Self {
        match value {
            TypeDefType::Enum(x) => Type::Enum(
                x.into_iter()
                    .map(|x| {
                        (
                            x.0,
                            match x.1 {
                                Some(x) => Some(type_def_type_obj_to_runtime(x)),
                                None => None,
                            },
                        )
                    })
                    .collect(),
            ),
            TypeDefType::Struct(x) => Type::Struct(type_def_type_obj_to_runtime(x)),
            TypeDefType::NewType(x) => Type::NewType(RuntimeType::from(x)),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Object {
    pub object_type: Type,
    pub functions: HashMap<String, (RuntimeValue, bool)>,
    pub traits: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub value: RuntimeValue,
    pub var_type: VarType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    pub counter: u64,
    pub scopes: HashMap<u64, Scope>,
    pub variables: HashMap<u64, HashMap<String, Variable>>,
    pub objects: HashMap<u64, HashMap<String, Object>>,
    pub stop: Option<StopValue>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    pub id: u64,
    pub counter: u64,
    pub parent: Option<u64>,
    pub children: HashMap<String, u64>,
    pub namespace: String,
    pub path: PathBuf,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            counter: 0,
            scopes: HashMap::new(),
            variables: HashMap::new(),
            objects: HashMap::new(),
            stop: None,
        }
    }

    pub fn remove_scope(&mut self, scope: &u64) {
        let parent = if let Some(parent) = self.scopes.get(scope).unwrap().parent {
            if let Some(parent_obj) = self.scopes.get(&parent) {
                if let Some(name) = parent_obj.children.iter().find(|x| x.1 == scope) {
                    Some((parent, name.0.clone()))
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };

        if let Some(parent) = parent {
            let _ = self
                .scopes
                .get_mut(&parent.0)
                .unwrap()
                .children
                .remove(&parent.1);
        }

        if &(self.counter - 1) == scope {
            while !self.scopes.contains_key(&(self.counter - 1)) {
                self.counter -= 1;
            }
        }

        self.variables.remove(scope);
        self.objects.remove(scope);
        self.scopes.remove(scope);
    }

    pub fn add_scope(&mut self, mut scope: Scope) {
        let has_parent = if let Some(parent) = &scope.parent {
            if let Some(parent) = self.scopes.get_mut(&parent) {
                parent
                    .children
                    .insert(scope.namespace.clone(), self.counter);
            }

            true
        } else {
            false
        };

        scope.id = self.counter;
        self.variables.insert(
            self.counter,
            HashMap::from([
                (
                    String::from("__name__"),
                    Variable {
                        value: RuntimeValue::Str(String::from(
                            if !has_parent || scope.namespace == "root" {
                                "__main__"
                            } else {
                                &scope.namespace
                            },
                        )),
                        var_type: VarType::Constant,
                    },
                ),
                (
                    String::from("__file__"),
                    Variable {
                        value: RuntimeValue::Str(String::from(scope.path.to_str().unwrap())),
                        var_type: VarType::Constant,
                    },
                ),
            ]),
        );
        self.objects.insert(self.counter, HashMap::new());
        self.scopes.insert(self.counter, scope);
        self.counter += 1;
    }

    pub fn get_scope_from_path(
        &self,
        path: &[String],
        mut parent: Option<u64>,
    ) -> Result<u64, InterpreterErr> {
        let mut skip = 0;
        if let None = parent {
            parent = Some(
                self.scopes
                    .iter()
                    .find(|(_, v)| v.namespace == path[0])
                    .map(|x| x.0)
                    .unwrap()
                    .clone(),
            );
            skip = 1;
        }

        for name in path.iter().skip(skip) {
            if let Some(p) = parent {
                parent = Some(self.get_scope_from_parent(p, name)?);
            }
        }

        Ok(parent.unwrap())
    }

    pub fn get_scope_from_parent(
        &self,
        parent: u64,
        namespace: &str,
    ) -> Result<u64, InterpreterErr> {
        for (_, child) in self.scopes.get(&parent).unwrap().children.iter() {
            if let Some(x) = self.scopes.get(&child) {
                if x.namespace == namespace {
                    return Ok(x.id);
                }
            }
        }
        Err(InterpreterErr::Value(ValueErr::Scope(ScopeErr::Scope(
            namespace.to_string(),
        ))))
    }

    pub fn new_scope_from_parent_shallow(&mut self, parent: u64) -> u64 {
        let path = self.scopes.get(&parent).unwrap().path.clone();
        self.new_scope(Some(parent), path, None)
    }

    pub fn new_scope_from_parent(&mut self, parent: u64, namespace: &str) -> u64 {
        if let Ok(scope) = self.get_scope_from_parent(parent, namespace) {
            return scope;
        }

        let path = self.scopes.get(&parent).unwrap().path.clone();
        let parent_name = path.file_name().unwrap();
        let folder = path.parent().unwrap().to_path_buf();

        let extra = if parent_name == "main.cl" {
            String::new()
        } else {
            format!(
                "{}/",
                parent_name.to_str().unwrap().split(".").nth(0).unwrap()
            )
        };

        let mut path1 = folder.clone();
        path1 = path1.join(format!("{extra}{namespace}.cl"));

        let mut path2 = folder.clone();
        path2 = path2.join(format!("{extra}{namespace}/main.cl"));

        if path1.exists() {
            self.new_scope(Some(parent), path1, Some(namespace))
        } else if path2.exists() {
            self.new_scope(Some(parent), path2, Some(namespace))
        } else {
            panic!("Tried:\n{path1:?}\n{path2:?}")
        }
    }

    pub fn new_scope(
        &mut self,
        parent: Option<u64>,
        path: PathBuf,
        namespace: Option<&str>,
    ) -> u64 {
        if let Some(parent) = parent {
            let scope = Scope {
                id: self.counter.clone(),
                namespace: namespace.unwrap_or(&self.counter.to_string()).to_string(),
                parent: Some(parent),
                children: HashMap::new(),
                counter: 0,
                path,
            };
            let _ = self.add_scope(scope);

            self.counter - 1
        } else {
            self.new_scope_with_stdlib(None, path, namespace.clone())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::values::RuntimeValue;
    use std::str::FromStr;

    fn get_new_env() -> (Environment, u64) {
        let mut env = Environment::new();
        let scope = env.new_scope_with_stdlib(None, PathBuf::from_str("./main.cl").unwrap(), None);
        (env, scope)
    }

    #[test]
    fn test_scope_global_variables() {
        let (env, scope) = get_new_env();
        assert!(env.get_var(&scope, "PI").is_ok());
        assert!(env.get_var(&scope, "true").is_ok());
        assert!(env.get_var(&scope, "false").is_ok());
        assert!(env.get_var(&scope, "print").is_ok());
        assert_eq!(
            env.get_var(&scope, "print").unwrap().var_type,
            VarType::Constant
        );
    }

    #[test]
    fn test_scope_push_and_get_var() {
        let (mut env, scope) = get_new_env();
        env.push_var(
            &scope,
            "x".to_string(),
            Variable {
                value: RuntimeValue::Int(42),
                var_type: VarType::Mutable,
            },
        )
        .unwrap();
        assert_eq!(
            env.get_var(&scope, "x").unwrap().value,
            RuntimeValue::Int(42)
        );
    }

    #[test]
    fn test_scope_assign_var_and_type_mismatch() {
        let (mut env, scope) = get_new_env();
        env.push_var(
            &scope,
            "x".to_string(),
            Variable {
                value: RuntimeValue::Int(1),
                var_type: VarType::Mutable,
            },
        )
        .unwrap();

        assert!(env.assign_var(&scope, "x", RuntimeValue::Int(2)).is_ok());
        assert!(
            env.assign_var(&scope, "x", RuntimeValue::Bool(true))
                .is_err()
        )
    }

    #[test]
    fn test_scope_assign_const_var_error() {
        let (mut env, scope) = get_new_env();
        env.push_var(
            &scope,
            "x".to_string(),
            Variable {
                value: RuntimeValue::Int(1),
                var_type: VarType::Constant,
            },
        )
        .unwrap();

        assert!(env.assign_var(&scope, "x", RuntimeValue::Int(2)).is_err());
    }

    #[test]
    fn test_scope_variable_shadowing() {
        let (mut env, scope) = get_new_env();
        env.push_var(
            &scope,
            "x".to_string(),
            Variable {
                value: RuntimeValue::Int(1),
                var_type: VarType::Immutable,
            },
        )
        .unwrap();

        env.push_var(
            &scope,
            "x".to_string(),
            Variable {
                value: RuntimeValue::Int(2),
                var_type: VarType::Immutable,
            },
        )
        .unwrap();

        assert_eq!(
            env.get_var(&scope, "x").unwrap().value,
            RuntimeValue::Int(2)
        );
    }

    #[test]
    fn test_scope_push_and_get_object() {
        let (mut env, scope) = get_new_env();

        let obj = Type::NewType(RuntimeType::Int);
        env.push_object(
            &scope,
            "MyType".to_string(),
            Object {
                object_type: obj.clone(),
                functions: HashMap::new(),
                traits: Vec::new(),
            },
        );

        assert_eq!(env.get_object(&scope, "MyType").unwrap().object_type, obj);
    }

    #[test]
    fn test_scope_push_and_get_function() {
        let (mut env, scope) = get_new_env();

        let obj = Type::NewType(RuntimeType::Int);
        env.push_object(
            &scope,
            "MyType".to_string(),
            Object {
                object_type: obj.clone(),
                functions: HashMap::new(),
                traits: Vec::new(),
            },
        );

        env.push_function(
            &scope,
            "MyType",
            ("foo".to_string(), RuntimeValue::Int(1), false),
        );

        let (val, is_static) = env.get_function(&scope, "MyType", "foo").unwrap();

        assert_eq!(val, &RuntimeValue::Int(1));
        assert!(!is_static);
    }

    #[test]
    fn test_scope_get_object_error() {
        let (env, scope) = get_new_env();

        assert!(env.get_function(&scope, "MyType", "foo").is_err());
        assert!(env.get_object(&scope, "MyType").is_err());
    }
}
