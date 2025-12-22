const BASIC_CODE: &str = r#"
    const thirty = fn () -> int => 30;
    // const main = fn () -> int => if 1 => 10 + 4 else => 30;
    const main = fn (x : int) -> float =>  {
        let mut index : float = 1.0;
        let arr : list<float> = [10.5, 40.8, 50.2];
        let tpl : <float, float> = tuple(10.0, 90.8);
        let mut counter : float = 1 as float;
        for counter < 98.0 => {
            counter += 1.2 + 1 as float;
            index *= counter;
        }
        let itm : float = tpl[0];
        let txt = "djsdalk";

        for i in 100 => counter += i as float;

        for 100 => counter += 2 as float;

        arr[0];
    }
    // const main = fn () -> float => 100.8;
    // const main = fn () -> str => "abcd";
    const forty = fn () -> int => 40;
"#;

use calibre_cranelift::Compiler;
use calibre_parser::{Parser, ast::Node, lexer::Tokenizer};
use std::{fmt::Debug, mem};

fn parse(text: String) -> Node {
    let mut parser = Parser::default();

    let mut tokenizer = Tokenizer::default();
    parser.produce_ast(tokenizer.tokenize(text).unwrap())
}

fn run<I, T: Debug>(input: I) {
    let program = parse(BASIC_CODE.to_string());
    let mut jit = Compiler::default();
    let code_ptr = jit.compile(program).unwrap();
    let code_fn = unsafe { mem::transmute::<_, fn(I) -> T>(code_ptr) };

    println!("{:?}", code_fn(input))
}

fn main() {
    run::<i64, f64>(20);
}
