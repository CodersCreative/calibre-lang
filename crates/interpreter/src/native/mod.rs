use std::{cmp::Ordering, fmt::Debug};

use calibre_common::environment::Scope;
use calibre_parser::Parser;

use crate::runtime::{
    interpreter::InterpreterErr, scope::InterpreterEnvironment, values::RuntimeValue,
};

pub mod global;
pub mod stdlib;

pub trait NativeFunction {
    fn run(
        &self,
        env: &mut InterpreterEnvironment,
        scope: &u64,
        args: &[(RuntimeValue, Option<RuntimeValue>)],
    ) -> Result<RuntimeValue, InterpreterErr>;

    fn name(&self) -> String;

    fn get_resolved_name(&self, env: &InterpreterEnvironment) -> String {
        let name = self.name();
        let name = env
            .mappings
            .iter()
            .find(|x| x.split_once(":").unwrap().1 == name)
            .map(|x| x.to_string())
            .unwrap_or(name);

        name
    }
}

impl Debug for dyn NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name())
    }
}

impl PartialEq for dyn NativeFunction {
    fn ne(&self, _other: &Self) -> bool {
        false
    }

    fn eq(&self, _other: &Self) -> bool {
        true
    }
}
impl PartialOrd for dyn NativeFunction {
    fn gt(&self, _other: &Self) -> bool {
        false
    }

    fn lt(&self, _other: &Self) -> bool {
        false
    }

    fn ge(&self, _other: &Self) -> bool {
        true
    }

    fn le(&self, _other: &Self) -> bool {
        true
    }

    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        Some(Ordering::Equal)
    }
}

impl InterpreterEnvironment {
    pub fn new_scope_with_stdlib<'a>(&'a mut self, parent: Option<u64>) -> u64 {
        let _parser = Parser::default();
        let counter = self.scope_counter;

        self.add_scope(Scope {
            id: counter,
            parent,
            children: Vec::new(),
            variables: Vec::new(),
        });

        calibre_common::native::global::setup(self, &counter);

        let std = self.new_scope(Some(counter));

        stdlib::setup(self, &std);

        let root = self.new_scope(Some(counter));

        root
    }
}
