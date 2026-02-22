use std::{fmt::Display, path::PathBuf};

use crate::{
    ast::{Node, NodeType},
    parse::parse_program_with_source,
};
use serde::{Deserialize, Serialize};
use thiserror::Error;

pub mod ast;
pub mod native;
pub mod parse;

#[derive(
    Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Serialize, Deserialize,
)]
pub struct Position {
    pub line: u32,
    pub col: u32,
}

#[derive(Debug, Clone, PartialEq, Default, Serialize, Deserialize)]
pub struct Location {
    pub path: PathBuf,
    pub span: Span,
}
#[derive(
    Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Serialize, Deserialize,
)]
pub struct Span {
    pub from: Position,
    pub to: Position,
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({}:{}) -> ({}:{})",
            self.from.line, self.from.col, self.to.line, self.to.col
        )
    }
}

impl Span {
    pub fn new(from: Position, to: Position) -> Self {
        Self { from, to }
    }

    pub fn new_from_spans(from: Self, to: Self) -> Self {
        Self {
            from: from.from,
            to: to.to,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Bracket {
    Curly,
    Paren,
    Square,
}

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

#[allow(unused_assignments)]
#[derive(Error, Debug, Clone, PartialEq)]
pub enum ParserError {
    #[error("{err} at {span}")]
    Syntax { err: SyntaxErr, span: Span },
}

impl ParserError {
    pub fn span(&self) -> Span {
        match self {
            Self::Syntax { span, .. } => *span,
        }
    }

    pub fn source_name(&self) -> &'static str {
        "calibre-parser"
    }

    pub fn code(&self) -> &'static str {
        match self {
            Self::Syntax { err, .. } => err.code(),
        }
    }

    pub fn summary(&self) -> &'static str {
        match self {
            Self::Syntax { err, .. } => err.summary(),
        }
    }

    pub fn hint(&self) -> Option<&'static str> {
        match self {
            Self::Syntax { err, .. } => err.hint(),
        }
    }

    pub fn hint_message(&self) -> Option<String> {
        match self {
            Self::Syntax { err, .. } => err.hint_message(),
        }
    }

    pub fn message_with_hint(&self) -> String {
        if let Some(hint) = self.hint_message() {
            format!("{self}. Hint: {hint}")
        } else {
            self.to_string()
        }
    }
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

impl SyntaxErr {
    pub fn code(&self) -> &'static str {
        match self {
            Self::ExpectedOpeningBracket(_) => "CAL001",
            Self::ExpectedClosingBracket(_) => "CAL002",
            Self::ExpectedToken(_) => "CAL003",
            Self::ExpectedIdentifier => "CAL004",
            Self::ExpectedName => "CAL005",
            Self::UnexpectedToken => "CAL006",
            Self::InvalidLiteral(_) => "CAL007",
            Self::ExpectedKeyword(_) => "CAL008",
            Self::ExpectedKey => "CAL009",
            Self::ExpectedType => "CAL010",
            Self::ExpectedFunctions => "CAL011",
            Self::UnexpectedWhileLoop => "CAL012",
            Self::UnexpectedEOF => "CAL013",
            Self::NullConstant => "CAL014",
            Self::This => "CAL015",
            Self::ExpectedChar(_) => "CAL016",
        }
    }

    pub fn summary(&self) -> &'static str {
        match self {
            Self::ExpectedOpeningBracket(_) => "missing opening bracket",
            Self::ExpectedClosingBracket(_) => "missing closing bracket",
            Self::ExpectedToken(_) => "unexpected or missing token",
            Self::ExpectedIdentifier => "missing identifier",
            Self::ExpectedName => "missing name",
            Self::UnexpectedToken => "unexpected token",
            Self::InvalidLiteral(_) => "invalid literal",
            Self::ExpectedKeyword(_) => "missing keyword",
            Self::ExpectedKey => "missing key",
            Self::ExpectedType => "missing type",
            Self::ExpectedFunctions => "only functions allowed here",
            Self::UnexpectedWhileLoop => "invalid while loop in iterator syntax",
            Self::UnexpectedEOF => "unexpected end of file",
            Self::NullConstant => "constant cannot be null",
            Self::This => "self used outside impl",
            Self::ExpectedChar(_) => "missing required character",
        }
    }

    pub fn hint(&self) -> Option<&'static str> {
        match self {
            Self::ExpectedOpeningBracket(_) => {
                Some("check that this construct starts with the correct bracket")
            }
            Self::ExpectedClosingBracket(_) => {
                Some("add the missing closing bracket for the nearest unmatched opener")
            }
            Self::ExpectedToken(_) => {
                Some("insert the required token or remove the unexpected token")
            }
            Self::ExpectedIdentifier => {
                Some("add an identifier (letters/digits/underscore, not a keyword)")
            }
            Self::ExpectedName => Some("provide a name after this construct"),
            Self::UnexpectedToken => Some("remove this token or replace it with a valid one"),
            Self::InvalidLiteral(_) => {
                Some("fix the literal syntax (quotes/suffix/numeric format)")
            }
            Self::ExpectedKeyword(_) => Some("insert the expected keyword"),
            Self::ExpectedKey => Some("add a key before ':'"),
            Self::ExpectedType => Some("add an explicit type"),
            Self::ExpectedFunctions => Some("keep only function declarations in this section"),
            Self::UnexpectedWhileLoop => {
                Some("iterator syntax cannot be combined with while loop syntax")
            }
            Self::UnexpectedEOF => Some("complete the current declaration before end of file"),
            Self::NullConstant => Some("replace null with a non-null constant value"),
            Self::This => Some("use self only inside an impl block"),
            Self::ExpectedChar(_) => Some("insert the expected character"),
        }
    }

    pub fn hint_message(&self) -> Option<String> {
        match self {
            Self::ExpectedOpeningBracket(bracket) => Some(format!(
                "insert the matching opening {:?} bracket before this point",
                bracket
            )),
            Self::ExpectedClosingBracket(bracket) => Some(format!(
                "insert the missing closing {:?} bracket to finish the current construct",
                bracket
            )),
            Self::ExpectedToken(token) => {
                let lower = token.to_lowercase();
                if lower.contains("eof") {
                    Some("the file ended early; finish the current expression/block".to_string())
                } else if lower.contains("`:`") {
                    Some("add ':' after the key/label".to_string())
                } else if lower.contains("`;`") {
                    Some("add ';' or a newline to terminate the previous statement".to_string())
                } else if lower.contains("`,`") {
                    Some("add ',' between items/arguments".to_string())
                } else if lower.contains("`)`") {
                    Some("close the current call/group with ')'".to_string())
                } else if lower.contains("`]`") {
                    Some("close the current list/index with ']'".to_string())
                } else if lower.contains("`}`") {
                    Some("close the current block/object with '}'".to_string())
                } else {
                    Some(format!("fix the token sequence near here ({token})"))
                }
            }
            Self::ExpectedIdentifier => {
                Some("add an identifier (letters/digits/underscore, not a keyword)".to_string())
            }
            Self::ExpectedName => Some("provide a name after this construct".to_string()),
            Self::UnexpectedToken => {
                Some("remove this token or replace it with a valid one in this context".to_string())
            }
            Self::InvalidLiteral(literal) => Some(format!(
                "fix the literal format near `{literal}` (quotes/escapes/number suffix)"
            )),
            Self::ExpectedKeyword(keyword) => Some(format!("insert the `{keyword}` keyword here")),
            Self::ExpectedKey => Some("add an object/record key before ':'".to_string()),
            Self::ExpectedType => Some("add an explicit type annotation".to_string()),
            Self::ExpectedFunctions => {
                Some("only function declarations are valid in this section".to_string())
            }
            Self::UnexpectedWhileLoop => {
                Some("iterator syntax cannot be combined with while-loop syntax".to_string())
            }
            Self::UnexpectedEOF => {
                Some("finish the current declaration before the end of file".to_string())
            }
            Self::NullConstant => Some("replace null with a non-null constant value".to_string()),
            Self::This => Some("use self only inside an impl block".to_string()),
            Self::ExpectedChar(ch) => Some(format!("insert `{ch}` here")),
        }
    }
}
