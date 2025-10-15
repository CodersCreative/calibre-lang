use std::error::Error;

use calibre_parser::{
    Parser,
    lexer::{Span, Token, TokenType, Tokenizer},
};

pub mod format;

pub struct Formatter {
    pub lines: Vec<String>,
    pub comments: Vec<Token>,
    pub max_width: usize,
    pub position: Option<Span>,
}

impl Default for Formatter {
    fn default() -> Self {
        Self {
            lines: Vec::new(),
            comments: Vec::new(),
            max_width: 200,
            position: None,
        }
    }
}

impl Formatter {
    pub fn start_format(&mut self, text: String) -> Result<String, Box<dyn Error>> {
        self.lines.clear();
        self.position = None;

        let mut tokenizer = Tokenizer::new(true);
        let tokens = tokenizer.tokenize(text)?;

        self.comments = tokens
            .iter()
            .filter(|x| x.token_type == TokenType::Comment)
            .cloned()
            .collect();
        let tokens = tokens
            .into_iter()
            .filter(|x| x.token_type != TokenType::Comment)
            .collect();

        let mut parser = Parser::default();
        let ast = parser.produce_ast(tokens)?;

        Ok(self.format(ast))
    }
}

fn main() {
    println!("Hello, world!");
}
