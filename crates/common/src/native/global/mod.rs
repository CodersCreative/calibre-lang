use calibre_parser::ast::VarType;

use crate::environment::{Environment, RuntimeType, RuntimeValue, Variable};

pub fn setup<T: RuntimeValue, U: RuntimeType>(env: &mut Environment<T, U>, scope: &u64) {
    let funcs: Vec<&str> = vec![
        "ok", "err", "some", "trim", "print", "len", "panic", "tuple",
    ];

    let map = T::natives();

    let mut funcs = funcs
        .into_iter()
        .map(|x| (String::from(x), map.get(x).unwrap().clone()))
        .collect();

    let mut vars: Vec<(String, T)> = T::constants().into_iter().map(|x| x).collect();
    vars.append(&mut funcs);

    for var in vars {
        let _ = env.variables.insert(
            env.var_counter,
            Variable {
                value: var.1,
                var_type: VarType::Constant,
                location: None,
            },
        );

        env.scopes
            .get_mut(&scope)
            .unwrap()
            .variables
            .insert(var.0, env.var_counter);

        env.var_counter += 1;
    }
}
