use std::fs;

use crate::runtime::scope::InterpreterEnvironment;
use crate::runtime::values::{RuntimeType, RuntimeValue};
use calibre_common::environment::{RuntimeValue as Value, Variable};
use calibre_parser::Parser;
use calibre_parser::ast::VarType;
use calibre_parser::lexer::Tokenizer;

pub mod console;
pub mod random;
pub mod thread;

pub fn setup(env: &mut InterpreterEnvironment, scope: &u64) {
    let mut parser = Parser::default();

    let mut tokenizer = Tokenizer::default();
    let program = parser
        .produce_ast(
            tokenizer
                .tokenize(fs::read_to_string(env.scopes.get(scope).unwrap().path.clone()).unwrap())
                .unwrap(),
        )
        .unwrap();

    let _ = env.evaluate(scope, program).unwrap();

    setup_scope(env, scope, "thread", &["wait"]);
    setup_scope(env, scope, "console", &["out", "input", "err", "clear"]);
    setup_scope(env, scope, "random", &["generate", "bool", "ratio"]);
}

pub fn setup_scope(
    env: &mut InterpreterEnvironment,
    parent: &u64,
    name: &str,
    funcs: &[&'static str],
) {
    let scope = env.new_scope_from_parent(*parent, name);
    let map: std::collections::HashMap<String, RuntimeValue> = RuntimeValue::natives();
    let funcs = funcs.into_iter().map(|x| {
        (
            String::from(*x),
            map.get(&format!("{}.{}", name, x)).clone().unwrap(),
        )
    });

    for var in funcs {
        let counter = env.counter;
        let _ = env.variables.insert(
            counter,
            Variable {
                value: var.1.clone(),
                var_type: VarType::Constant,
                location: None,
            },
        );

        env.scopes
            .get_mut(&scope)
            .unwrap()
            .variables
            .insert(var.0, counter);

        env.counter += 1;
    }
}
