use std::{collections::HashMap, time::Duration};

use crate::environment::{Object, RuntimeType, RuntimeValue, Variable};

pub enum TestingResult<T: RuntimeValue> {
    Ok,
    Variable(String),
    Object(String),
    Duration,
    Result(T),
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
