use crate::{
    ast::{Node, NodeType},
    lexer::{Bracket, LexerError, Span, Token, TokenType},
};
use thiserror::Error;

pub mod ast;
pub mod lexer;
pub mod native;
pub mod parse;

#[derive(Debug, Default)]
pub struct Parser {
    tokens: Vec<Token>,
    pub errors: Vec<ParserError>,
    prev_token: Option<Token>,
    source_path: Option<std::path::PathBuf>,
}

impl Parser {
    pub fn set_source_path(&mut self, path: Option<std::path::PathBuf>) {
        self.source_path = path;
    }
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
        self.prev_token = None;
        let mut body = Vec::new();

        while !self.is_eof() {
            body.push(self.parse_statement());
            self.parse_delimited();
        }

        let body: Vec<Node> = body
            .into_iter()
            .filter(|x| x.node_type != NodeType::EmptyLine)
            .collect();

        let span = if let (Some(first), Some(last)) = (body.first(), body.last()) {
            Span::new_from_spans(first.span, last.span)
        } else {
            Span::new(
                lexer::Position { line: 1, col: 1 },
                lexer::Position { line: 1, col: 1 },
            )
        };

        Node::new(
            span,
            NodeType::ScopeDeclaration {
                body: Some(body),
                is_temp: false,
                define: false,
                named: None,
                create_new_scope: Some(false),
            },
        )
    }
}

impl From<LexerError> for ParserError {
    fn from(value: LexerError) -> Self {
        Self::Lexer(value)
    }
}

#[allow(unused_assignments)]
#[derive(Error, Debug, Clone, PartialEq)]
pub enum ParserError {
    #[error(transparent)]
    Lexer(LexerError),
    #[error("{err} at {span}")]
    Syntax { err: SyntaxErr, span: Span },
}

#[derive(Error, Debug, Clone, PartialEq)]
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
    #[error("Unexpectedly found EOF")]
    UnexpectedEOF,
    #[error("Constants cannot be null.")]
    NullConstant,
    #[error("Cannot use self outside of an implementation block.")]
    This,
    #[error("Expected character, '{0:?}'.")]
    ExpectedChar(char),
}
