use std::{collections::HashMap, time::Duration};

use thiserror::Error;

use crate::{
    environment::{Object, RuntimeType, RuntimeValue, Variable},
    errors::RuntimeErr,
};

#[derive(Error, Debug, Clone)]

pub enum TestingError<T: RuntimeValue, U: RuntimeType> {
    #[error("{0}")]
    Runtime(RuntimeErr<T, U>),
    #[error("Variable not found : {0}")]
    Variable(String),
    #[error("Object not found : {0}")]
    Object(String),
    #[error("Took too long : {0:?}")]
    Duration(Duration),
    #[error("Incorrect value returned : {0}")]
    Result(T),
}

pub trait Testable<T: RuntimeValue, U: RuntimeType> {
    fn test(program: String, params: TestingParams<T, U>) -> (Vec<TestingError<T, U>>, Self);
}

pub struct TestingParams<T: RuntimeValue, U: RuntimeType> {
    pub variables: HashMap<String, Variable<T>>,
    pub objects: HashMap<String, Object<T, U>>,
    pub duration: Option<Duration>,
    pub result: Option<T>,
}

impl<T: RuntimeValue, U: RuntimeType> Default for TestingParams<T, U> {
    fn default() -> Self {
        Self {
            variables: HashMap::new(),
            objects: HashMap::new(),
            duration: None,
            result: None,
        }
    }
}

impl<T: RuntimeValue, U: RuntimeType> TestingParams<T, U> {
    pub fn variables(mut self, variables: HashMap<String, Variable<T>>) -> Self {
        self.variables = variables;
        self
    }

    pub fn objects(mut self, objects: HashMap<String, Object<T, U>>) -> Self {
        self.objects = objects;
        self
    }

    pub fn duration(mut self, duration: Duration) -> Self {
        self.duration = Some(duration);
        self
    }

    pub fn result(mut self, result: T) -> Self {
        self.result = Some(result);
        self
    }
}
