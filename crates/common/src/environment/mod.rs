pub mod children;
pub mod objects;
pub mod scopes;
pub mod variables;

use calibre_parser::{
    ast::{NodeType, ObjectType, ParserDataType, TypeDefType, VarType},
    lexer::Span,
};
use std::{collections::HashMap, fmt::Debug, ops::Deref, path::PathBuf};

use crate::errors::{RuntimeErr, ScopeErr, ValueErr};

pub trait RuntimeType: From<ParserDataType> + PartialEq + Clone + Debug {}
pub trait RuntimeValue: PartialEq + Clone + Debug {
    fn string(txt: String) -> Self;
    fn natives() -> HashMap<String, Self>;
    fn constants() -> HashMap<String, Self>;
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type<U: RuntimeType> {
    Enum(Vec<(String, Option<ObjectType<U>>)>),
    Struct(ObjectType<U>),
    NewType(U),
}

fn type_def_type_obj_to_runtime<U: RuntimeType>(obj: ObjectType<ParserDataType>) -> ObjectType<U> {
    match obj {
        ObjectType::Tuple(x) => {
            let mut vec: Vec<U> = Vec::new();

            for v in x {
                vec.push(U::from(v));
            }

            ObjectType::Tuple(vec)
        }
        ObjectType::Map(x) => {
            let mut map: HashMap<String, U> = HashMap::new();
            for (k, v) in x {
                map.insert(k, U::from(v));
            }
            ObjectType::Map(map)
        }
    }
}

impl<U: RuntimeType> From<TypeDefType> for Type<U> {
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
            TypeDefType::NewType(x) => Type::NewType(U::from(x)),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Location {
    pub path: PathBuf,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Object<T: RuntimeValue, U: RuntimeType> {
    pub object_type: Type<U>,
    pub functions: HashMap<String, (T, Option<Location>, bool)>,
    pub traits: Vec<String>,
    pub location: Option<Location>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable<T> {
    pub value: T,
    pub var_type: VarType,
    pub location: Option<Location>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Environment<T: RuntimeValue, U: RuntimeType> {
    pub var_counter: u64,
    pub scope_counter: u64,
    pub scopes: HashMap<u64, Scope>,
    pub variables: HashMap<u64, Variable<T>>,
    pub objects: HashMap<u64, Object<T, U>>,
    pub current_location: Option<Location>,
    pub strict_removal: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    pub id: u64,
    pub counter: u64,
    pub parent: Option<u64>,
    pub children: HashMap<String, u64>,
    pub namespace: String,
    pub path: PathBuf,
    pub variables: HashMap<String, u64>,
    pub objects: HashMap<String, u64>,
}

impl<T: RuntimeValue, U: RuntimeType> Environment<T, U> {
    pub fn new(strict_removal: bool) -> Environment<T, U> {
        Environment {
            var_counter: 0,
            scope_counter: 0,
            scopes: HashMap::new(),
            variables: HashMap::new(),
            objects: HashMap::new(),
            current_location: None,
            strict_removal,
        }
    }

    pub fn get_location(&self, scope: &u64, span: Span) -> Option<Location> {
        Some(Location {
            path: self.scopes.get(scope).unwrap().path.clone(),
            span,
        })
    }

    pub fn remove_scope(&mut self, scope: &u64) {
        if !self.strict_removal {
            return;
        }
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

        if &(self.scope_counter - 1) == scope {
            self.scope_counter -= 1;
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
                    .insert(scope.namespace.clone(), self.scope_counter);
            }

            true
        } else {
            false
        };

        scope.id = self.scope_counter;

        self.variables.insert(
            self.var_counter,
            Variable {
                value: T::string(String::from(if !has_parent || scope.namespace == "root" {
                    "__main__"
                } else {
                    &scope.namespace
                })),
                var_type: VarType::Constant,
                location: None,
            },
        );

        self.variables.insert(
            self.var_counter + 1,
            Variable {
                value: T::string(String::from(scope.path.to_str().unwrap())),
                var_type: VarType::Constant,
                location: None,
            },
        );

        scope
            .variables
            .insert(String::from("__name__"), self.var_counter);
        scope
            .variables
            .insert(String::from("__file__"), self.var_counter + 1);
        self.scopes.insert(scope.id.clone(), scope);
        self.scope_counter += 1;
        self.var_counter += 2;
    }

    pub fn get_scope_from_path(
        &self,
        path: &[String],
        mut parent: Option<u64>,
    ) -> Result<u64, RuntimeErr<T, U>> {
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
    ) -> Result<u64, RuntimeErr<T, U>> {
        for (_, child) in self.scopes.get(&parent).unwrap().children.iter() {
            if let Some(x) = self.scopes.get(&child) {
                if x.namespace == namespace {
                    return Ok(x.id);
                }
            }
        }
        Err(RuntimeErr::Value(ValueErr::Scope(ScopeErr::Scope(
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
                id: self.scope_counter.clone(),
                namespace: namespace
                    .unwrap_or(&self.scope_counter.to_string())
                    .to_string(),
                parent: Some(parent),
                children: HashMap::new(),
                counter: 0,
                variables: HashMap::new(),
                objects: HashMap::new(),
                path,
            };
            let _ = self.add_scope(scope);

            self.scope_counter - 1
        } else {
            todo!()
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
