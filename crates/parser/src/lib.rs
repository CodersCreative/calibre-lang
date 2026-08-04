use crate::{
    ast::nodes::{Node, NodeType},
    parse::parse_program_with_source,
};
use serde::{Deserialize, Serialize};
use std::{
    fmt::Display,
    ops::Range,
    path::PathBuf,
    sync::{LazyLock, RwLock},
};
use thiserror::Error;

pub mod ast;
pub mod native;
pub mod parse;

pub static COUNTER: LazyLock<RwLock<u64>> = LazyLock::new(|| RwLock::new(0));

pub trait IdentifiersUsed {
    fn identifiers_used(&self) -> Vec<&String>;

    fn owned_identifiers_used(&self) -> Vec<String> {
        self.identifiers_used().into_iter().cloned().collect()
    }
}

#[inline]
pub fn qualified_name_tail(name: &str) -> &str {
    name.rsplit(':').next().unwrap_or(name)
}

#[inline]
pub fn qualified_name_base(name: &str) -> &str {
    let tail = qualified_name_tail(name);
    tail.split_once("->").map(|(base, _)| base).unwrap_or(tail)
}

#[inline]
pub fn short_name_if_qualified(name: &str) -> Option<&str> {
    let short = qualified_name_tail(name);
    (short != name).then_some(short)
}

#[inline]
pub fn qualified_name_matches(actual: &str, target: &str) -> bool {
    if actual == target {
        return true;
    }
    let actual_short = qualified_name_tail(actual);
    let target_short = qualified_name_tail(target);
    let actual_base = qualified_name_base(actual);
    let target_base = qualified_name_base(target);
    actual_short == target_short
        || actual_base == target_short
        || actual_short == target_base
        || actual_base == target_base
}

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

    pub fn to_range(&self, contents: &str) -> Range<usize> {
        let mut line_starts: Vec<usize> = vec![0];
        line_starts.append(&mut contents.match_indices('\n').map(|(i, _)| i + 1).collect());

        let start = *line_starts
            .get(self.from.line.saturating_sub(1) as usize)
            .unwrap_or(&0);
        let end = *line_starts
            .get(self.to.line.saturating_sub(1) as usize)
            .unwrap_or(&start);

        let start = start
            .saturating_add(self.from.col as usize)
            .min(contents.len());
        let end = end
            .saturating_add(self.to.col as usize)
            .min(contents.len())
            .max(start + 1);

        start..end
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
    source_path: Option<PathBuf>,
}

#[inline]
fn empty_scope_node() -> Node {
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

impl Parser {
    pub fn set_source_path(&mut self, path: Option<PathBuf>) {
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
                empty_scope_node()
            }
        }
    }
}

pub trait CalibreError: Display {
    fn code(&self) -> usize;
    fn hint(&self) -> Option<String>;
    fn step(&self) -> &'static str;

    fn message_with_hint(&self) -> String {
        if let Some(hint) = self.hint() {
            format!("{self}. Hint: {hint}")
        } else {
            self.to_string()
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
}

impl CalibreError for ParserError {
    fn code(&self) -> usize {
        match self {
            Self::Syntax { err, .. } => err.code(),
        }
    }

    fn hint(&self) -> Option<String> {
        match self {
            Self::Syntax { err, .. } => err.hint(),
        }
    }

    fn step(&self) -> &'static str {
        "parser"
    }
}

#[derive(Error, Debug, Clone, PartialEq)]
pub enum SyntaxErr {
    #[error("expected opening bracket: {0:?}")]
    ExpectedOpeningBracket(Bracket),
    #[error("expected closing bracket: {0:?}")]
    ExpectedClosingBracket(Bracket),
    #[error("{0}")]
    ExpectedToken(String),
    #[error("expected identifier")]
    ExpectedIdentifier,
    #[error("expected name")]
    ExpectedName,
    #[error("unexpected token")]
    UnexpectedToken,
    #[error("invalid literal: {0}")]
    InvalidLiteral(String),
    #[error("expected keyword: {0}")]
    ExpectedKeyword(String),
    #[error("expected key")]
    ExpectedKey,
    #[error("expected data type")]
    ExpectedType,
    #[error("expected only function declarations")]
    ExpectedFunctions,
    #[error("cannot use while-loop syntax with iterator syntax")]
    UnexpectedWhileLoop,
    #[error("unexpected end of file")]
    UnexpectedEOF,
    #[error("constant cannot be null")]
    NullConstant,
    #[error("cannot use self outside an impl block")]
    This,
    #[error("expected character: '{0:?}'")]
    ExpectedChar(char),
}

impl CalibreError for SyntaxErr {
    fn code(&self) -> usize {
        match self {
            Self::ExpectedOpeningBracket(_) => 1,
            Self::ExpectedClosingBracket(_) => 2,
            Self::ExpectedToken(_) => 3,
            Self::ExpectedIdentifier => 4,
            Self::ExpectedName => 5,
            Self::UnexpectedToken => 6,
            Self::InvalidLiteral(_) => 7,
            Self::ExpectedKeyword(_) => 8,
            Self::ExpectedKey => 9,
            Self::ExpectedType => 10,
            Self::ExpectedFunctions => 11,
            Self::UnexpectedWhileLoop => 12,
            Self::UnexpectedEOF => 13,
            Self::NullConstant => 14,
            Self::This => 15,
            Self::ExpectedChar(_) => 16,
        }
    }

    fn hint(&self) -> Option<String> {
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

    fn step(&self) -> &'static str {
        "parser"
    }
}
