use std::fs;

use calibre_common::environment::{RuntimeValue as Value, Variable};
use calibre_parser::Parser;
use calibre_parser::ast::VarType;
use calibre_parser::lexer::Tokenizer;

use crate::runtime::scope::CheckerEnvironment;
use crate::runtime::values::RuntimeType;

pub fn setup(env: &mut CheckerEnvironment, scope: &u64) {
    let mut parser = Parser::default();
    let mut tokenizer = Tokenizer::default();

    let program = parser
        .produce_ast(
            tokenizer
                .tokenize(
                    fs::read_to_string(env.scopes.get(scope).unwrap().path.clone())
                        .unwrap_or(String::new()),
                )
                .unwrap(),
        )
        .unwrap();

    let _ = env.evaluate(scope, program).unwrap();

    setup_scope(env, scope, "thread", &["wait"]);
    setup_scope(env, scope, "console", &["out", "input", "err", "clear"]);
    setup_scope(env, scope, "random", &["generate", "bool", "ratio"]);
}

pub fn setup_scope(env: &mut CheckerEnvironment, parent: &u64, name: &str, funcs: &[&'static str]) {
    let scope = env.new_scope_from_parent(*parent, name);
    let map: std::collections::HashMap<String, RuntimeType> = RuntimeType::natives();

    let funcs = funcs.into_iter().map(|x| {
        (
            String::from(*x),
            map.get(&format!("{}.{}", name, x)).clone().unwrap(),
        )
    });

    for var in funcs {
        let counter = env.var_counter;
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

        env.var_counter += 1;
    }
}
