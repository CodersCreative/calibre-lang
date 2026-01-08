pub mod variables;

use std::ops::{Deref, DerefMut};

use crate::runtime::values::{RuntimeType, RuntimeValue, helper::StopValue};
use calibre_common::environment::Environment;
use calibre_mir::environment::MiddleEnvironment;

#[derive(Debug, Clone, PartialEq)]
pub struct InterpreterEnvironment {
    pub env: Environment<RuntimeValue, RuntimeType>,
    pub stop: Option<StopValue>,
}

impl InterpreterEnvironment {
    pub fn new(env: &MiddleEnvironment) -> Self {
        Self {
            env: Environment::new(true, env),
            stop: None,
        }
    }
}
impl Deref for InterpreterEnvironment {
    type Target = Environment<RuntimeValue, RuntimeType>;
    fn deref(&self) -> &Self::Target {
        &self.env
    }
}

impl DerefMut for InterpreterEnvironment {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.env
    }
}
