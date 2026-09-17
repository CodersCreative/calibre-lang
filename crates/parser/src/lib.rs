use crate::{
    ast::nodes::{AstNode, AstNodeType, scopes::AstScopeDef},
    lexer::Token,
    parse::parse_program_with_source,
};
use chumsky::span::SimpleSpan;
use logos::Logos;
use serde::{Deserialize, Serialize};
use std::{
    fmt::Display,
    ops::Range,
    path::PathBuf,
    sync::{LazyLock, RwLock},
};
use thiserror::Error;
use tracing::{debug, info, instrument};
use ustr::{Ustr, UstrMap};

pub mod ast;
pub mod formatter;
pub mod lexer;
pub mod native;
pub mod parse;

pub static COUNTER: LazyLock<RwLock<u64>> = LazyLock::new(|| RwLock::new(0));

#[derive(Default, Clone, Debug)]
pub struct AlphaRenameState {
    pub data: UstrMap<Ustr>,
    pub dont_change_local: bool,
}

impl AlphaRenameState {
    #[inline(always)]
    pub fn mapped_name_or_original(&self, original: Ustr) -> Ustr {
        self.data.get(&original).cloned().unwrap_or(original)
    }

    #[inline(always)]
    pub fn mapped_str_or_original(&self, original: &str) -> String {
        self.data
            .get(&Ustr::from(original))
            .map(|x| x.to_string())
            .unwrap_or_else(|| original.to_string())
    }

    #[inline]
    pub fn from_native_mappings(
        &mut self,
        new_mappings: &UstrMap<Ustr>,
        old_mappings: &UstrMap<Ustr>,
    ) {
        for (k, v) in new_mappings {
            if let Some(old_v) = old_mappings.get(k) {
                self.data.insert(*old_v, *v);
            }
        }
    }
}

pub trait AlphaRenamable {
    fn rename(&mut self, state: &mut AlphaRenameState);

    #[inline(always)]
    fn rename_owned(mut self, state: &mut AlphaRenameState) -> Self
    where
        Self: Sized,
    {
        self.rename(state);
        self
    }
}

pub trait IdentifiersUsed {
    fn identifiers_used(&self) -> Vec<&String>;

    #[inline(always)]
    fn owned_identifiers_used(&self) -> Vec<String> {
        self.identifiers_used().into_iter().cloned().collect()
    }
}

pub trait UstrIdentifiersUsed {
    fn identifiers_used(&self) -> Vec<&Ustr>;

    #[inline(always)]
    fn owned_identifiers_used(&self) -> Vec<Ustr> {
        self.identifiers_used().into_iter().cloned().collect()
    }
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
    pub from: usize,
    pub to: usize,
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}) -> ({})", self.from, self.to)
    }
}

impl From<Range<usize>> for Span {
    fn from(value: Range<usize>) -> Self {
        Self {
            from: value.start,
            to: value.end,
        }
    }
}

impl From<SimpleSpan> for Span {
    fn from(value: SimpleSpan) -> Self {
        Self {
            from: value.start,
            to: value.end,
        }
    }
}

impl From<Span> for SimpleSpan {
    fn from(value: Span) -> Self {
        Self {
            start: value.from,
            end: value.to,
            context: (),
        }
    }
}

impl From<Span> for Range<usize> {
    fn from(value: Span) -> Self {
        value.from..value.to
    }
}

impl Span {
    pub fn new(from: usize, to: usize) -> Self {
        Self { from, to }
    }

    pub fn new_from_spans(from: Self, to: Self) -> Self {
        Self {
            from: from.from,
            to: to.to,
        }
    }

    pub fn is_none(&self) -> bool {
        self.from == 0 && self.to == 0
    }

    pub fn to_range(self) -> Range<usize> {
        self.into()
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
fn empty_scope_node() -> AstNode {
    AstNode::new(
        Span::default(),
        AstNodeType::ScopeDeclaration(AstScopeDef {
            body: Some(Vec::new()),
            is_temp: false,
            define: false,
            named: None,
            create_new_scope: Some(false),
        }),
    )
}

impl Parser {
    pub fn lex<'a>(&self, source: &'a str) -> Result<Vec<Token<'a>>, Vec<ParserError>> {
        let mut tokens = Vec::new();
        let mut lex_errors = Vec::new();

        for result in Token::lexer(source).spanned() {
            match result {
                (Ok(token), _) => {
                    if !matches!(token, Token::LineComment(_) | Token::BlockComment(_)) {
                        tokens.push(token);
                    }
                }
                (Err(_), span) => {
                    lex_errors.push(ParserError::Lexer {
                        err: "invalid token".to_string(),
                        span: Span::from(span),
                    });
                }
            }
        }

        if lex_errors.is_empty() {
            Ok(tokens)
        } else {
            Err(lex_errors)
        }
    }

    pub fn set_source_path(&mut self, path: Option<PathBuf>) {
        self.source_path = path;
    }

    #[instrument(skip_all, fields(bytes = source.len(), path = ?self.source_path))]
    pub fn produce_ast(&mut self, source: &str) -> AstNode {
        debug!(lines = source.lines().count(), "starting parse");
        match self.lex(source).and_then(|x| {
            parse_program_with_source(&x, self.source_path.as_deref()).map_err(|errors| {
                let spans = Token::lexer(source)
                    .spanned()
                    .filter_map(|(token, span)| {
                        let token = token.ok()?;
                        (!matches!(token, Token::LineComment(_) | Token::BlockComment(_)))
                            .then_some(Span::from(span))
                    })
                    .collect::<Vec<_>>();

                errors
                    .into_iter()
                    .map(|mut error| {
                        if let ParserError::Syntax { span, .. } = &mut error {
                            let from = spans
                                .get(span.from)
                                .map(|token| token.from)
                                .unwrap_or(source.len());
                            let to = spans
                                .get(span.to.saturating_sub(1))
                                .map(|token| token.to)
                                .unwrap_or(from);
                            *span =
                                Span::new(from.min(source.len()), to.max(from).min(source.len()));
                        }
                        error
                    })
                    .collect()
            })
        }) {
            Ok(ast) => {
                self.errors.clear();
                info!("parse completed");
                ast
            }
            Err(errs) => {
                tracing::warn!(errors = errs.len(), "parse failed");
                self.errors = errs;
                empty_scope_node()
            }
        }
    }
}

pub trait CalibreError: Display {
    fn code(&self) -> &'static str;
    fn hint(&self) -> Option<String>;
    fn step(&self) -> &'static str;
    fn span(&self) -> Span;

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
    #[error("lexing error: {err}")]
    Lexer { err: String, span: Span },
}

impl CalibreError for ParserError {
    fn code(&self) -> &'static str {
        match self {
            Self::Syntax { err, .. } => err.code(),
            Self::Lexer { .. } => "Lex",
        }
    }

    fn hint(&self) -> Option<String> {
        match self {
            Self::Syntax { err, .. } => err.hint(),
            Self::Lexer { .. } => None,
        }
    }

    fn step(&self) -> &'static str {
        match self {
            Self::Syntax { .. } => "parser",
            Self::Lexer { .. } => "lexer",
        }
    }

    fn span(&self) -> Span {
        match self {
            Self::Syntax { span, .. } => *span,
            Self::Lexer { span, .. } => *span,
        }
    }
}

#[derive(Error, Debug, Clone, PartialEq)]
pub enum SyntaxErr {
    #[error("expected opening bracket: {0:?}")]
    ExpectedOpeningBracket(Bracket),
    #[error("expected closing bracket: {0:?}")]
    ExpectedClosingBracket(Bracket),
    #[error("unclosed parenthesis: missing ')' to match opening '('")]
    UnclosedParen,
    #[error("unclosed bracket: missing ']' to match opening '['")]
    UnclosedBracket,
    #[error("unclosed brace: missing '}}' to match opening '{{'")]
    UnclosedBrace,
    #[error("missing semicolon after statement")]
    MissingSemicolon,
    #[error("missing comma between items")]
    MissingComma,
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
    fn code(&self) -> &'static str {
        match self {
            Self::ExpectedOpeningBracket(_) => "P001",
            Self::ExpectedClosingBracket(_) => "P002",
            Self::UnclosedParen => "P017",
            Self::UnclosedBracket => "P018",
            Self::UnclosedBrace => "P019",
            Self::MissingSemicolon => "P020",
            Self::MissingComma => "P021",
            Self::ExpectedToken(_) => "P003",
            Self::ExpectedIdentifier => "P004",
            Self::ExpectedName => "P005",
            Self::UnexpectedToken => "P006",
            Self::InvalidLiteral(_) => "P007",
            Self::ExpectedKeyword(_) => "P008",
            Self::ExpectedKey => "P009",
            Self::ExpectedType => "P010",
            Self::ExpectedFunctions => "P011",
            Self::UnexpectedWhileLoop => "P012",
            Self::UnexpectedEOF => "P013",
            Self::NullConstant => "P014",
            Self::This => "P015",
            Self::ExpectedChar(_) => "P016",
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
            Self::UnclosedParen => Some(String::from("add a closing ')' to match the opening '('")),
            Self::UnclosedBracket => {
                Some(String::from("add a closing ']' to match the opening '['"))
            }
            Self::UnclosedBrace => {
                Some(String::from("add a closing '}}' to match the opening '{{'"))
            }
            Self::MissingSemicolon => {
                Some("add ';' or a newline to terminate the previous statement".to_string())
            }
            Self::MissingComma => Some("add ',' between items/arguments".to_string()),
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

    fn span(&self) -> Span {
        Span::default()
    }
}
