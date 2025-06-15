use crate::{
    ast::NodeType,
    lexer::{Token, TokenType, tokenize},
};

pub mod parse;

#[derive(Debug, Default)]
pub struct Parser {
    tokens: Vec<Token>,
}

impl Parser {
    fn is_eof(&self) -> bool {
        if let Some(token) = self.tokens.first() {
            token.token_type == TokenType::EOF
        } else {
            true
        }
    }
    pub fn produce_ast(&mut self, source: String) -> NodeType {
        self.tokens = tokenize(source);
        let mut program_body = Vec::new();

        while !self.is_eof() {
            program_body.push(self.parse_statement())
        }

        NodeType::Program(Box::new(program_body))
    }
}
