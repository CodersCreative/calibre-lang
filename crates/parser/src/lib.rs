use crate::{
    ast::{Node, NodeType},
    lexer::{Bracket, LexerError, Span, Token, TokenType},
};
use thiserror::Error;

pub mod ast;
pub mod lexer;
pub mod parse;

#[derive(Debug, Default)]
pub struct Parser {
    tokens: Vec<Token>,
    pub errors: Vec<ParserError>,
}

impl Parser {
    fn is_eof(&self) -> bool {
        if let Some(token) = self.tokens.first() {
            token.token_type == TokenType::EOF
        } else {
            true
        }
    }
    pub fn produce_ast(&mut self, tokens: Vec<Token>) -> Node {
        self.errors.clear();
        self.tokens = tokens;
        let mut body = Vec::new();

        while !self.is_eof() {
            body.push(self.parse_statement())
        }

        let span = if body.len() > 0 {
            Span::new_from_spans(body.first().unwrap().span, body.last().unwrap().span)
        } else {
            Span::new(
                lexer::Position { line: 1, col: 1 },
                lexer::Position { line: 1, col: 1 },
            )
        };

        Node::new(
            NodeType::ScopeDeclaration {
                body,
                is_temp: false,
            },
            span,
        )
    }
}

impl From<LexerError> for ParserError {
    fn from(value: LexerError) -> Self {
        Self::Lexer(value)
    }
}

#[derive(Error, Debug, Clone)]
pub enum ParserError {
    #[error("{0}")]
    Lexer(LexerError),
    #[error("{0}\nFound : {1:?}\nNext : {2:?}")]
    Syntax(SyntaxErr, Token, Token, Token, Token),
}

#[derive(Error, Debug, Clone)]
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
