pub mod objects;
pub mod variables;

use calibre_mir::environment::{MiddleEnvironment, MiddleObject, MiddleTypeDefType};
use calibre_parser::{
    ast::{ObjectMap, ParserDataType, VarType},
    lexer::Location,
};
use std::{collections::HashMap, fmt::Debug, marker::PhantomData};

use crate::errors::ScopeErr;

pub trait InterpreterFrom<U>: Sized {
    type Interpreter;
    fn interpreter_from(env: &Self::Interpreter, scope: &u64, value: U) -> Result<Self, ScopeErr>;
}

pub trait FromParser: Sized {
    fn from_parser_type<I, T: RuntimeValue>(
        env: &I,
        scope: &u64,
        value: ParserDataType,
    ) -> Result<Self, ScopeErr>;
}

pub trait RuntimeType: PartialEq + Clone + Debug + InterpreterFrom<ParserDataType> {}
pub trait RuntimeValue: PartialEq + Clone + Debug {
    fn string(txt: String) -> Self;
    fn natives() -> HashMap<String, Self>;
    fn constants() -> HashMap<String, Self>;
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type<U: RuntimeType> {
    Enum(Vec<(String, Option<ObjectMap<U>>)>),
    Struct(ObjectMap<U>),
    NewType(U),
}

impl<U: RuntimeType> InterpreterFrom<ObjectMap<ParserDataType>> for ObjectMap<U> {
    type Interpreter = U::Interpreter;
    fn interpreter_from(
        env: &Self::Interpreter,
        scope: &u64,
        value: ObjectMap<ParserDataType>,
    ) -> Result<Self, ScopeErr> {
        let mut map: HashMap<String, U> = HashMap::new();
        for (k, v) in value.0 {
            map.insert(k, U::interpreter_from(env, scope, v)?);
        }
        Ok(ObjectMap(map))
    }
}

impl<U: RuntimeType> InterpreterFrom<MiddleTypeDefType> for Type<U> {
    type Interpreter = U::Interpreter;
    fn interpreter_from(
        env: &Self::Interpreter,
        scope: &u64,
        value: MiddleTypeDefType,
    ) -> Result<Self, ScopeErr> {
        Ok(match value {
            MiddleTypeDefType::Enum(x) => Type::Enum(
                x.into_iter()
                    .map(|x| {
                        (
                            x.0.to_string(),
                            match x.1 {
                                Some(x) => {
                                    Some(ObjectMap::<U>::interpreter_from(env, scope, x).unwrap())
                                }
                                None => None,
                            },
                        )
                    })
                    .collect(),
            ),
            MiddleTypeDefType::Struct(x) => {
                Type::Struct(ObjectMap::<U>::interpreter_from(env, scope, x)?)
            }
            MiddleTypeDefType::NewType(x) => Type::NewType(U::interpreter_from(env, scope, x)?),
        })
    }
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct Environment<T: RuntimeValue, U: RuntimeType> {
    pub scope_counter: u64,
    pub scopes: HashMap<u64, Scope>,
    pub variables: HashMap<String, Variable<T>>,
    pub mappings: Vec<String>,
    pub objects: HashMap<String, MiddleObject>,
    pub current_location: Option<Location>,
    pub runtime_type: PhantomData<U>,
    pub strict_removal: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    pub id: u64,
    pub variables: Vec<String>,
    pub children: Vec<u64>,
    pub parent: Option<u64>,
}

impl<T: RuntimeValue, U: RuntimeType> Environment<T, U> {
    pub fn new(strict_removal: bool, middle_env: &MiddleEnvironment) -> Environment<T, U> {
        Environment {
            scope_counter: 0,
            scopes: HashMap::new(),
            variables: HashMap::new(),
            objects: middle_env.objects.clone(),
            current_location: None,
            mappings: middle_env
                .variables
                .iter()
                .map(|x| x.0.to_string())
                .collect(),
            runtime_type: PhantomData,
            strict_removal,
        }
    }

    pub fn remove_scope(&mut self, scope: &u64) {
        if !self.strict_removal {
            return;
        }

        self.scopes
            .iter_mut()
            .for_each(|x| x.1.children.retain(|x| x != scope));

        if &(self.scope_counter - 1) == scope {
            self.scope_counter -= 1;
        }

        self.scopes.remove(scope);
    }

    pub fn add_scope(&mut self, mut scope: Scope) {
        scope.id = self.scope_counter;
        self.scopes.insert(scope.id.clone(), scope);
        self.scope_counter += 1;
    }

    pub fn new_scope(&mut self, parent: Option<u64>) -> u64 {
        if let Some(parent) = parent {
            let scope = Scope {
                id: self.scope_counter.clone(),
                parent: Some(parent),
                children: Vec::new(),
                variables: Vec::new(),
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
