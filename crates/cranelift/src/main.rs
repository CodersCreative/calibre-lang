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

const fib := fn (n : int) -> int => 
    return n if n < 2 else (fib(n - 1) + fib(n - 2));

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

const test_result_discriminant := fn () -> int => {
    let res := err(200);
    return discriminant(res) as! int;
}

const test_string_literal := fn () -> int => {
    let s := "héllo wörld";
    console_output(0, s);
    return 42;
}

const test_char_literal := fn () -> int => {
    let c := '€';
    console_output(0, c);
    return 42;
}

type Point := struct {
    x : int,
    y : int,
};

const test_struct_literal := fn () => {
    let p := Point { x: 10, y: 20 };
    console_output(0, p);
    console_output(0, '\n');
}

const test_tuple_literal := fn () => {
    let t := (1, 2, 3);
    console_output(0, t);
    console_output(0, '\n');
}

const test_list_literal := fn () => {
    let lst := [1, 2, 3];
    console_output(0, lst);
    console_output(0, '\n');
    let lst := [420 for 69];
    console_output(0, lst);
    console_output(0, '\n');
}

type Language := enum {
    ENGLISH : int,
    SPANISH,
};

const test_enum_literal := fn () => {
    let lang := Language.ENGLISH : 42000;
    console_output(0, "hi\n");
    console_output(0, lang);
    console_output(0, '\n');
    console_output(0, lang.next);
    console_output(0, '\n');
    console_output(0, discriminant(lang));
    console_output(0, '\n');
}

const test_range := fn () -> int => {
    let r := 0..10;
    console_output(0, r);
    console_output(0, '\n');
    let l := len(r);
    console_output(0, l);
    console_output(0, '\n');
    let m := min_or_zero(r);
    console_output(0, m);
    console_output(0, '\n');
    return l as! int + m;
}

const test_gen := fn -> gen:<int> => {
    let mut i := 0;
    for i < 30 => {
        return i;
        i += 1;
    }
}

const test_generator := fn () -> int => {
    let mut g := test_gen();
    let value : int? = g.next();
    return value.next;
}

const main := fn (x : int) -> int => {
    let result := calc(x) if x is int else calc(x+1);
    let value := conversions();
    let fib_value := fib(28);
    
    test_struct_literal();
    test_tuple_literal();
    test_enum_literal();
    test_list_literal();
    test_range();
    test_generator();

    let mut i := 0;
    for i < 30 => {
        console_output(0, i);
        console_output(0, ' ');
        i += 1;
    }
    console_output(0, '\n');

    console_output(0, fib_value);
    console_output(0, '\n');
    test_string_literal();
    console_output(0, '\n');
    test_char_literal();
    console_output(0, '\n');
    return result + value + test_option_next() + test_result_discriminant() + test_result_ok() + test_result_err() + fib_value;
};
"#;

fn parse(text: String) -> (LirRegistry, MiddleEnvironment) {
    let mut parser = Parser::default();
    let path = PathBuf::from_str("./main.cal").unwrap();
    parser.set_source_path(Some(path.clone()));
    let program = parser.produce_ast(&text);

    println!("Lowering to MIR...");
    let (env, _scope, middle) =
        MiddleEnvironment::new_and_evaluate(program, path, Vec::new(), false, true);

    if !env.context.errors.is_empty() {
        println!("{:?}", env.context.errors);
    }

    println!("Lowering to LIR...");
    let lir_result = LirEnvironment::lower(&env, middle);

    let main = env
        .symbols
        .variables
        .iter()
        .find(|(k, _)| k.name() == "main").map(|x| vec![x.0.clone()]).unwrap_or_default();

    let lir_result = lir_result.eliminate_dead_code(main, false);

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

    if let Some(init_key) = jit
        .data
        .registry
        .functions
        .keys()
        .find(|k| k.name() == "calibre_init_globals")
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
        let code_fn = unsafe { std::mem::transmute::<*const u8, fn(I) -> T>(code_ptr) };
        let result = code_fn(input);
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
    run::<i64, i64>(300);
    println!("elapsed - {}ms", start.elapsed().as_millis());
}
