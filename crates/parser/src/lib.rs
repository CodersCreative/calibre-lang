use crate::{
    ast::{Node, NodeType},
    lexer::{Bracket, LexerError, Span},
    parse::parse_program_with_source,
};
use thiserror::Error;

pub mod ast;
pub mod lexer;
pub mod native;
pub mod parse;

#[derive(Debug, Default)]
pub struct Parser {
    pub errors: Vec<ParserError>,
    source_path: Option<std::path::PathBuf>,
}

impl Parser {
    pub fn set_source_path(&mut self, path: Option<std::path::PathBuf>) {
        self.source_path = path;
    }

    pub fn produce_ast(&mut self, source: &str) -> Node {
        match parse_program_with_source(source, self.source_path.as_deref()) {
            Ok(ast) => {
                self.errors.clear();
                ast
            }
            Err(errs) => {
                self.errors = errs;
                Node::new(
                    Span::default(),
                    NodeType::ScopeDeclaration {
                        body: Some(Vec::new()),
                        is_temp: false,
                        define: false,
                        named: None,
                        create_new_scope: Some(false),
                    },
                )
            }
        }
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
    #[error("Expected token, {0}.")]
    ExpectedToken(String),
    #[error("Expected identifier.")]
    ExpectedIdentifier,
    #[error("Expected name.")]
    ExpectedName,
    #[error("Expected token.")]
    UnexpectedToken,
    #[error("Invalid literal: {0}.")]
    InvalidLiteral(String),
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
