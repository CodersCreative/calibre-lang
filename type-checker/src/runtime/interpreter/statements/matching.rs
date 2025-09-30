use calibre_parser::ast::{NodeType, ObjectType, RefMutability, VarType};

use crate::runtime::{
    interpreter::InterpreterErr, scope::CheckerEnvironment, values::RuntimeType
};
use std::panic;

impl CheckerEnvironment {
    pub fn evaluate_match_function(
        &mut self,
        scope: &u64,
        mutability: &RefMutability,
        value: NodeType,
        patterns: Vec<(NodeType, Vec<NodeType>, Box<NodeType>)>,
    ) -> Result<RuntimeType, InterpreterErr> {
        Ok(RuntimeType::Dynamic)
    }

    pub fn handle_conditionals(
        &mut self,
        scope: &u64,
        conditionals: Vec<NodeType>,
    ) -> Result<(), InterpreterErr> {
        for condition in conditionals.into_iter() {
            if RuntimeType::Bool != self.evaluate(scope, condition)? {
                panic!()
            }
        }

        Ok(())
    }
}
