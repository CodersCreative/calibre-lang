// A retry of my cranelift backend using the lir as a base
// Its going to based off of the last commit with cranelift
// A url for the repo at this point : https://github.com/CodersCreative/calibre-lang/tree/bbfdeeba4eafa54dfc9598018448a2fc8b79f82b
// I need to aim to learn more about cranelift and use best practices during this attempt

use calibre_cranelift::JitCompiler;
use calibre_lir::environment::{LirEnvironment, LirRegistry};
use calibre_mir::environment::MiddleEnvironment;
use calibre_parser::Parser;
use std::{fmt::Debug, path::PathBuf, str::FromStr};

const BASIC_CODE: &str = r#"
const sixty : int = 60;

const main := fn (x : int) -> int => {
    calibre_assert_ints(3, 2);
    return x + sixty;
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

fn run<I, T: Debug>(input: I) {
    let (lir, env) = parse(BASIC_CODE.to_string());

    let jit = match JitCompiler::new(lir.clone()) {
        Ok(j) => j,
        Err(e) => {
            eprintln!("Failed to initialize JIT: {e}");
            return;
        }
    };

    let main_ptr = env
        .symbols
        .variables
        .iter()
        .find(|(k, _)| k.name() == "main")
        .and_then(|(k, _)| jit.get_function(k));

    if let Some(code_ptr) = main_ptr {
        let code_fn = unsafe { std::mem::transmute::<*const u8, fn(I) -> T>(code_ptr) };
        println!("Result: {:?}", code_fn(input));
    } else {
        todo!()
    }
}

fn main() {
    run::<i64, i64>(300);
}
