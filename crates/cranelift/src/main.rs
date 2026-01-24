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
    
    // const main = fn () -> int => if 1 => 10 + 4 else => 30;
    const main = fn (x : int) -> float => {
        // let forty = fn () -> float => 40.0;    
        let mut index : float = 1.0;
        let arr : list:<float> = list[10.5, 40.8, 50.2];
        let tpl : <float, float> = tuple(10.0, 90.8);
        let mut counter : float = 1.0;
        let french = Language.FRENCH;
        let id = Id (70000.0);

        for counter < 98.0 => {
            counter += 1.2 + 10.9;
            let _ = "11";
            index *= counter;
        }
                
        let itm : float = tpl.0;
        let txt = "djsdalk";
        let smth = Smth{
    		txt : "hello",
    		flt : 8.0,
    	};

    	let _ = "dshjk";

        /*for i in 100 => counter += i as float;

        for 100 => counter += 2 as float;*/

        id.0 + smth.flt;
    };
    // const main = fn () -> float => 100.8;
    // const main = fn () -> str => "abcd";
    
"#;

use calibre_comptime::ComptimeEnvironment;
use calibre_cranelift::Compiler;
use calibre_mir::environment::MiddleEnvironment;
use calibre_mir_ty::MiddleNode;
use calibre_parser::{Parser, lexer::Tokenizer};
use std::{fmt::Debug, mem, path::PathBuf, str::FromStr};

fn parse(text: String) -> (MiddleNode, MiddleEnvironment) {
    let mut parser = Parser::default();
    let mut tokenizer = Tokenizer::default();
    let program = parser.produce_ast(tokenizer.tokenize(text).unwrap());

    let mut middle_result =
        MiddleEnvironment::new_and_evaluate(program, PathBuf::from_str("./main.cl").unwrap())
            .unwrap();
    println!("Starting comptime...");
    middle_result.2 =
        ComptimeEnvironment::new_and_evaluate(middle_result.2, &middle_result.0).unwrap();
    println!("Starting jit...");
    (middle_result.2, middle_result.0)
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
