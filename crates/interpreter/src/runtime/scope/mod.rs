pub mod variables;

use crate::runtime::{
    interpreter::InterpreterErr,
    values::{RuntimeType, RuntimeValue, helper::StopValue},
};
use calibre_common::environment::Environment;
use calibre_mir::environment::MiddleEnvironment;
use calibre_mir_ty::MiddleNode;
use std::ops::{Deref, DerefMut};

#[derive(Debug, Clone, PartialEq)]
pub struct InterpreterEnvironment {
    pub env: Environment<RuntimeValue, RuntimeType>,
    pub stop: Option<StopValue>,
}

impl InterpreterEnvironment {
    pub fn new(env: &MiddleEnvironment) -> Self {
        Self {
            env: Environment::new(false, env),
            stop: None,
        }
    }

    pub fn new_with_strict(env: &MiddleEnvironment, strict_removal: bool) -> Self {
        Self {
            env: Environment::new(strict_removal, env),
            stop: None,
        }
    }

    pub fn new_and_evaluate(
        node: MiddleNode,
        env: &MiddleEnvironment,
    ) -> Result<(Self, u64, RuntimeValue), InterpreterErr> {
        let mut env = Self::new(env);
        let scope = env.new_scope_with_stdlib(None);
        let res = env.evaluate(&scope, node);
        res.map(|x| (env, scope, x))
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
