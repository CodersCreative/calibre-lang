use thiserror::Error;

use crate::{
    ast::NodeType,
    lexer::{Bracket, LexerError, Token, TokenType, tokenize},
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
    pub fn produce_ast(&mut self, source: String) -> Result<NodeType, ParserError> {
        self.tokens = tokenize(source)?;
        let mut program_body = Vec::new();

        while !self.is_eof() {
            program_body.push(self.parse_statement()?)
        }

        Ok(NodeType::Program(Box::new(program_body)))
    }
}

impl From<LexerError> for ParserError {
    fn from(value: LexerError) -> Self {
        Self::Lexer(value)
    }
}

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("{0}")]
    Lexer(LexerError),
    #[error("{0}\nFound : {1:?}\nNext : {2:?}")]
    Syntax(SyntaxErr, Token, Token),
}

#[derive(Error, Debug)]
pub enum SyntaxErr {
    #[error("Expected opening bracket, {0:?}.")]
    ExpectedOpeningBracket(Bracket),
    #[error("Expected closing bracket, {0:?}.")]
    ExpectedClosingBracket(Bracket),
    #[error("Expected token, {0:?}.")]
    ExpectedToken(TokenType),
    #[error("Expected identifier.")]
    ExpectedIdentifier,
    #[error("Expected name.")]
    ExpectedName,
    #[error("Expected token.")]
    UnexpectedToken,
    #[error("Expected {0} keyword.")]
    ExpectedKeyword(String),
    #[error("Expected key.")]
    ExpectedKey,
    #[error("Expected data type.")]
    ExpectedType,
    #[error("Expected only functions.")]
    ExpectedFunctions,
    #[error("Cant use while loop with iterators syntax.")]
    UnexpectedWhileLoop,
    #[error("Constants cannot be null.")]
    NullConstant,
    #[error("Cannot use self outside of an implementation block.")]
    This,
    #[error("Expected character, '{0:?}'.")]
    ExpectedChar(char),
}
