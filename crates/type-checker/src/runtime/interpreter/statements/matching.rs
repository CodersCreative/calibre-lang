use calibre_parser::ast::{Node, RefMutability};

use crate::runtime::{interpreter::InterpreterErr, scope::CheckerEnvironment, values::RuntimeType};
use std::panic;

impl CheckerEnvironment {
    pub fn evaluate_match_function(
        &mut self,
        _scope: &u64,
        _mutability: &RefMutability,
        _value: Node,
        _patterns: Vec<(Node, Vec<Node>, Box<Node>)>,
    ) -> Result<RuntimeType, InterpreterErr> {
        Ok(RuntimeType::Dynamic)
    }

    pub fn handle_conditionals(
        &mut self,
        scope: &u64,
        conditionals: Vec<Node>,
    ) -> Result<(), InterpreterErr> {
        for condition in conditionals.into_iter() {
            if RuntimeType::Bool != self.evaluate(scope, condition)? {
                panic!()
            }
        }

        Ok(())
    }
}
