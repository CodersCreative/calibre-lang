// A retry of my cranelift backend using the lir as a base
// Its going to based off of the last commit with cranelift
// A url for the repo at this point : https://github.com/CodersCreative/calibre-lang/tree/bbfdeeba4eafa54dfc9598018448a2fc8b79f82b
// I need to aim to learn more about cranelift and use best practices during this attempt

use calibre_cranelift::JitCompiler;
use calibre_lir::environment::{LirEnvironment, LirRegistry};
use calibre_mir::environment::MiddleEnvironment;
use calibre_parser::Parser;
use std::{fmt::Debug, path::PathBuf, str::FromStr, time::Instant};

const BASIC_CODE: &str = r#"
let sixty : int = 60;

const add := fn (a : int, b : int) -> int => {
    return a + b;
};

const mul := fn (a : int, b : int) -> int => {
    return a * b;
};

const calc := fn (x : int) -> int => {
    let x := add(x, sixty);
    return mul(x, 2);
};

const conversions := fn () -> int => {
    let f := 10 as! float;
    let fi := f as! int;

    let b : byte = 5;
    let bi := b as! int;

    let c := 'A';
    let ci := c as! int;

    let u := fi as! uint;
    let ui := u as! int;

    return fi + bi + ci + ui;
};

const test_option_next := fn () -> int => {
    let res := some(50);
    return res.next;
}

const test_result_ok := fn () -> int => {
    let res := ok(100);
    return res.ok;
}

const test_result_err := fn () -> int => {
    let res := err(200);
    return res.err;
}

const main := fn () -> int => {
    return test_option_next() + test_result_ok() + test_result_err();
};
"#;

fn parse(text: String) -> (LirRegistry, MiddleEnvironment) {
    let mut parser = Parser::default();
    let path = PathBuf::from_str("./main.cal").unwrap();
    parser.set_source_path(Some(path.clone()));
    let program = parser.produce_ast(&text);

    let (env, _scope, middle) =
        MiddleEnvironment::new_and_evaluate(program, path, Vec::new(), true, false);

    println!("Lowering to LIR...");
    let lir_result = LirEnvironment::lower(&env, middle);

    println!("Starting JIT...");
    (lir_result, env)
}

fn run<T: Debug>() {
    let (lir, env) = parse(BASIC_CODE.to_string());

    let jit = match JitCompiler::new(lir.clone()) {
        Ok(j) => j,
        Err(e) => {
            eprintln!("Failed to initialize JIT: {e}");
            return;
        }
    };

    if let Some(init_key) = jit
        .data
        .registry
        .functions
        .keys()
        .find(|k| k.name() == "__init_globals")
        .cloned()
        && let Some(init_code_ptr) = jit.get_function(&init_key)
    {
        let init_fn = unsafe { std::mem::transmute::<*const u8, extern "C" fn()>(init_code_ptr) };
        init_fn();
    }

    let main_ptr = env
        .symbols
        .variables
        .iter()
        .find(|(k, _)| k.name() == "main")
        .and_then(|(k, _)| jit.get_function(k));

    if let Some(code_ptr) = main_ptr {
        let code_fn = unsafe { std::mem::transmute::<*const u8, fn() -> T>(code_ptr) };
        let result = code_fn();
        println!("Result: {:?}", result);
    } else {
        eprintln!("Failed to find `main`");
        for (k, _) in env.symbols.variables.iter() {
            eprintln!("\tAvailable : {}", k.name());
        }
    }
}

fn main() {
    let start = Instant::now();
    run::<i64>();
    println!("elapsed - {}ms", start.elapsed().as_millis());
}
