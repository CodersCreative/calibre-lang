use crate::{
    ast::{Node, NodeType},
    lexer::{Bracket, LexerError, Span, Token, TokenType},
};
use miette::Diagnostic;
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
                body: Some(body),
                is_temp: false,
                define: false,
                named: None,
                create_new_scope: false,
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

#[derive(Error, Debug, Clone, Diagnostic)]
pub enum ParserError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Lexer(LexerError),
    #[error("{err} at {span}")]
    Syntax {
        #[source_code]
        input: String,
        err: SyntaxErr,
        span: Span,
        #[label("here")]
        token: Option<(usize, usize)>,
    },
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
    #[error("Unexpectedly found EOF")]
    UnexpectedEOF,
    #[error("Constants cannot be null.")]
    NullConstant,
    #[error("Cannot use self outside of an implementation block.")]
    This,
    #[error("Expected character, '{0:?}'.")]
    ExpectedChar(char),
}
