const BASIC_CODE: &str = r#"
    const thirty = fn () -> int => 30;
    type Language = enum {
    	// Enums can have hasmap type data structuress.
    	FRENCH,
    	// Enums can have tuple type data structuress.
    	ENGLISH : int,
    	SPANISH,
    	ARABIC: <Language, Language>
    };
    type Smth = struct {
        txt : str,
        flt : float,
    }
    type Id = struct (float);
    const sixty : int = 60;
    
    // const main = fn () -> int => if 1 => 10 + 4 else => 30;
    const main = fn (x : int) -> float => {
        const true = 1;
        const false = 0;   
        let id = Id (70000.0);

        let smth = Smth{
    		txt : "hello",
    		flt : 8.0,
    	};

    	if true => let _ = "fie";

        id.0 + smth.flt;
    };
    const min = fn () -> float => 100.8 + 11.4;
    const man = fn () -> str => "abcd";
    
"#;

use calibre_comptime::ComptimeEnvironment;
use calibre_cranelift::Compiler;
use calibre_lir::{LirEnvironment, LirNodeType, LirRegistry};
use calibre_mir::environment::MiddleEnvironment;
use calibre_mir_ty::MiddleNode;
use calibre_parser::{Parser, lexer::Tokenizer};
use std::{fmt::Debug, mem, path::PathBuf, str::FromStr};

fn parse(text: String) -> (LirRegistry, MiddleEnvironment) {
    let mut parser = Parser::default();
    let mut tokenizer = Tokenizer::default();
    let program = parser.produce_ast(tokenizer.tokenize(&text).unwrap());

    let mut middle_result =
        MiddleEnvironment::new_and_evaluate(program, PathBuf::from_str("./main.cl").unwrap())
            .unwrap();
    println!("Starting comptime...");
    middle_result.2 =
        ComptimeEnvironment::new_and_evaluate(middle_result.2, &middle_result.0).unwrap();
    println!("Lowering...");
    let lir_result = LirEnvironment::lower(&middle_result.0, middle_result.2);
    println!("Starting jit...");
    (lir_result, middle_result.0)
}

fn run<I, T: Debug>(input: I) {
    let program = parse(BASIC_CODE.to_string());
    let mut jit = Compiler::default();
    let code_ptr = jit.compile(program.0, program.1).unwrap();
    let code_fn = unsafe { mem::transmute::<_, fn(I) -> T>(code_ptr) };

    println!("{:?}", code_fn(input))
}

fn main() {
    run::<i64, f64>(20);
}
