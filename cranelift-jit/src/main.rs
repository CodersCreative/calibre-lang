const BASIC_CODE: &str = r#"
   const thirty = fn () -> int => 30;
   // const main = fn () -> int => if 1 => 10 + 4 else => 30;
   // const main = fn () -> float => 100.8;
   const main = fn () -> str => "abcd";
   const forty = fn () -> int => 40;
"#;

use std::{fmt::Debug, mem};

use calibre_cranelift_jit::jit::JIT;
use calibre_parser::{Parser, ast::NodeType, lexer::tokenize};

fn parse(text: String) -> NodeType {
    let mut parser = Parser::default();
    parser.produce_ast(text).unwrap()
}

fn run<I, T: Debug>(input: I) {
    let program = parse(BASIC_CODE.to_string());
    let mut jit = JIT::default();
    let code_ptr = jit.compile(program).unwrap();
    let code_fn = unsafe { mem::transmute::<_, fn(I) -> T>(code_ptr) };

    println!("{:?}", code_fn(input))
}

fn main() {
    run::<(), String>(());
}
