use calibre_parser::ast::VarType;

use crate::{
    environment::{Environment, RuntimeType, RuntimeValue, Variable}
};
use std::{collections::HashMap};

pub fn setup<T : RuntimeValue, U : RuntimeType>(env: &mut Environment<T, U>, scope: &u64) {
    let funcs: Vec<&str> = vec![
        "ok",
        "err",
        "some",
        "range",
        "trim",
        "print",
        "len",
    ];

    let map = T::natives();
    let funcs = funcs.into_iter().map(|x| (String::from(x), map.get(x).clone().unwrap()));

    if let Some(map) = env.variables.get_mut(scope) {
        for func in funcs {
            let _ = map.insert(
                func.0,
                Variable {
                    value: func.1.clone(),
                    var_type: VarType::Constant,
                    location : None,
                },
            );
        }
    }

    let vars : HashMap<String, T> = T::constants();

    if let Some(map) = env.variables.get_mut(scope) {
        for var in vars {
            let _ = map.insert(
                var.0,
                Variable {
                    value: var.1,
                    var_type: VarType::Constant,
                    location : None,
                },
            );
        }
    }
}
