use crate::ast::{
    binary::BinaryOperator,
    comparison::{BooleanOperator, ComparisonOperator},
};
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, fmt::Display, path::PathBuf};
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Default, Serialize, Deserialize)]
pub struct Location {
    pub path: PathBuf,
    pub span: Span,
}
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Serialize, Deserialize)]
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Serialize, Deserialize)]
pub struct Position {
    pub line: u32,
    pub col: u32,
}

pub struct Tokenizer {
    pub include_comments: bool,
    line: u32,
    col: u32,
}

#[derive(Error, Debug, Clone)]
pub enum LexerError {
    #[error("Unrecognized character: '{ch}'")]
    Unrecognized {
        span: Span,
        ch: char,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Bracket {
    Curly,
    Paren,
    Square,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum StopValue {
    Return,
    Break,
    Continue,
    Until,
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenType {
    Float,
    Integer,
    Null,
    String,
    Char,
    ColonAngled,
    Identifier,
    Equals,
    Dollar,
    Overload,
    Open(Bracket),
    Close(Bracket),
    Colon,
    DoubleColon,
    Comment,
    Comma,
    Comparison(ComparisonOperator),
    Boolean(BooleanOperator),
    BinaryOperator(BinaryOperator),
    BinaryAssign(BinaryOperator),
    BooleanAssign(BooleanOperator),
    Stop(StopValue),
    Not,
    Ref,
    RefMut,
    Mut,
    Const,
    Let,
    Match,
    Impl,
    For,
    Range,
    Trait,
    Enum,
    List,
    Arrow,
    LeftArrow,
    FatArrow,
    OrColon,
    Object,
    Question,
    Async,
    Func,
    If,
    In,
    Or,
    Main,
    As,
    Try,
    This,
    At,
    FullStop,
    EOL,
    EOF,
    Struct,
    Else,
    PipeParen,
    Pipe,
    From,
    Import,
    Type,
    WhiteSpace,
    Debug,
    Drop,
    Move,
    Defer,
}

pub fn keywords() -> HashMap<String, TokenType> {
    HashMap::from([
        (String::from("until"), TokenType::Stop(StopValue::Until)),
        (String::from("mut"), TokenType::Mut),
        (String::from("null"), TokenType::Null),
        (String::from("const"), TokenType::Const),
        (String::from("let"), TokenType::Let),
        (String::from("enum"), TokenType::Enum),
        (String::from("match"), TokenType::Match),
        (String::from("obj"), TokenType::Object),
        (String::from("fn"), TokenType::Func),
        (String::from("drop"), TokenType::Drop),
        (String::from("move"), TokenType::Move),
        (String::from("defer"), TokenType::Defer),
        (String::from("else"), TokenType::Else),
        (String::from("list"), TokenType::List),
        (String::from("debug"), TokenType::Debug),
        (String::from("return"), TokenType::Stop(StopValue::Return)),
        (String::from("in"), TokenType::In),
        (String::from("break"), TokenType::Stop(StopValue::Break)),
        (
            String::from("continue"),
            TokenType::Stop(StopValue::Continue),
        ),
        (String::from("try"), TokenType::Try),
        (String::from("if"), TokenType::If),
        (String::from("as"), TokenType::As),
        (String::from("func"), TokenType::Func),
        (String::from("struct"), TokenType::Struct),
        (String::from("async"), TokenType::Async),
        (String::from("impl"), TokenType::Impl),
        (String::from("trait"), TokenType::Trait),
        (String::from("Self"), TokenType::This),
        (String::from("for"), TokenType::For),
        (String::from("import"), TokenType::Import),
        (String::from("from"), TokenType::From),
        (String::from("type"), TokenType::Type),
    ])
}

pub fn special_keywords() -> HashMap<String, TokenType> {
    HashMap::from([
        (String::from("->"), TokenType::Arrow),
        (String::from("|>"), TokenType::Pipe),
        (String::from("<("), TokenType::PipeParen),
        (String::from("|:"), TokenType::OrColon),
        (String::from("<-"), TokenType::LeftArrow),
        (String::from("=>"), TokenType::FatArrow),
        (String::from(".."), TokenType::Range),
        (String::from("::"), TokenType::DoubleColon),
        (String::from(":<"), TokenType::ColonAngled),
        (
            String::from("**"),
            TokenType::BinaryOperator(BinaryOperator::Pow),
        ),
        (
            String::from("**="),
            TokenType::BinaryAssign(BinaryOperator::Pow),
        ),
        (
            String::from("<<="),
            TokenType::BinaryAssign(BinaryOperator::Shl),
        ),
        (
            String::from(">>="),
            TokenType::BinaryAssign(BinaryOperator::Shr),
        ),
        (
            String::from("&="),
            TokenType::BinaryAssign(BinaryOperator::BitAnd),
        ),
        (
            String::from("&&="),
            TokenType::BooleanAssign(BooleanOperator::And),
        ),
        (
            String::from("||="),
            TokenType::BooleanAssign(BooleanOperator::Or),
        ),
        (String::from("&&"), TokenType::Boolean(BooleanOperator::And)),
        (String::from("||"), TokenType::Boolean(BooleanOperator::Or)),
        (
            String::from("|="),
            TokenType::BinaryAssign(BinaryOperator::BitOr),
        ),
        (String::from("@overload"), TokenType::Overload),
        (String::from("&mut"), TokenType::RefMut),
        (
            String::from("=="),
            TokenType::Comparison(ComparisonOperator::Equal),
        ),
        (
            String::from("!="),
            TokenType::Comparison(ComparisonOperator::NotEqual),
        ),
    ])
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Token {
    pub value: String,
    pub token_type: TokenType,
    pub span: Span,
}

impl Default for Tokenizer {
    fn default() -> Self {
        Self {
            include_comments: false,
            line: 1,
            col: 1,
        }
    }
}

impl Tokenizer {
    pub fn new(include_comments: bool) -> Self {
        Self {
            include_comments,
            line: 1,
            col: 1,
        }
    }
    pub fn new_token(&self, token_type: TokenType, value: &str) -> Token {
        let mut from = self.col as i32 - value.len() as i32;
        if from < 0 {
            from = 0;
        }

        Token {
            value: value.to_string(),
            token_type,
            span: Span {
                from: Position {
                    line: self.line,
                    col: from as u32,
                },
                to: Position {
                    line: self.line,
                    col: self.col,
                },
            },
        }
    }

    fn increment_line_col(&mut self, c: &char) {
        if c == &'\n' {
            self.line += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }
    }

    fn get_unrecognized(&self, ch: char) -> LexerError {
        LexerError::Unrecognized {
            span: Span {
                from: Position {
                    line: self.line,
                    col: self.col,
                },
                to: Position {
                    line: self.line,
                    col: self.col.saturating_add(1),
                },
            },
            ch,
        }
    }

    pub fn tokenize(&mut self, txt: &str) -> Result<Vec<Token>, LexerError> {
        let mut tokens: Vec<Token> = Vec::new();
        let mut buffer: Vec<char> = txt.chars().collect();
        self.line = 1;
        self.col = 1;

        while buffer.len() > 0 {
            let first = buffer.first().unwrap();

            let get_token = |c: char| -> Option<TokenType> {
                match c {
                    '(' => Some(TokenType::Open(Bracket::Paren)),
                    '|' => Some(TokenType::Or),
                    ')' => Some(TokenType::Close(Bracket::Paren)),
                    '{' => Some(TokenType::Open(Bracket::Curly)),
                    '}' => Some(TokenType::Close(Bracket::Curly)),
                    '[' => Some(TokenType::Open(Bracket::Square)),
                    ']' => Some(TokenType::Close(Bracket::Square)),
                    ',' => Some(TokenType::Comma),
                    '@' => Some(TokenType::At),
                    '&' => Some(TokenType::Ref),
                    '$' => Some(TokenType::Dollar),
                    '?' => Some(TokenType::Question),
                    '<' | '>' => Some(TokenType::Comparison(
                        ComparisonOperator::from_operator(&c.to_string()).unwrap(),
                    )),
                    '.' => Some(TokenType::FullStop),
                    ':' => Some(TokenType::Colon),
                    '=' => Some(TokenType::Equals),
                    '!' => Some(TokenType::Not),
                    '+' | '-' | '*' | '/' | '^' | '%' => Some(TokenType::BinaryOperator(
                        BinaryOperator::from_symbol(&c.to_string()).unwrap(),
                    )),
                    ';' => Some(TokenType::EOL),
                    _ if c.is_whitespace() => Some(TokenType::WhiteSpace),
                    _ => None,
                }
            };

            let token = match get_token(*first) {
                Some(t) => match t {
                    TokenType::WhiteSpace => {
                        self.increment_line_col(first);
                        let _ = buffer.remove(0);
                        Some(self.new_token(t, ";"))
                    }
                    TokenType::FullStop => {
                        self.increment_line_col(first);
                        let value = buffer.remove(0).to_string();
                        Some(self.new_token(
                            t,
                            &format!(
                                "{}{}",
                                if tokens.last().unwrap().token_type == TokenType::WhiteSpace {
                                    String::from(" ")
                                } else {
                                    String::new()
                                },
                                value.trim()
                            ),
                        ))
                    }
                    t => {
                        self.increment_line_col(first);
                        Some(self.new_token(t, buffer.remove(0).to_string().trim()))
                    }
                },
                _ => {
                    if first == &'\'' {
                        let mut txt = String::new();

                        let c = buffer.remove(0);
                        self.increment_line_col(&c);

                        while buffer[0] != '\'' {
                            let c = buffer.remove(0);
                            self.increment_line_col(&c);
                            txt.push(c);
                        }

                        let c = buffer.remove(0);
                        self.increment_line_col(&c);

                        Some(self.new_token(TokenType::Char, &txt))
                    } else if first == &'"' {
                        let mut txt = String::new();

                        let c = buffer.remove(0);
                        self.increment_line_col(&c);

                        while buffer[0] != '"' {
                            let c = buffer.remove(0);
                            self.increment_line_col(&c);
                            txt.push(c);
                        }

                        let c = buffer.remove(0);
                        self.increment_line_col(&c);

                        Some(self.new_token(TokenType::String, &txt))
                    } else if first.is_numeric() {
                        let mut number = String::new();
                        let mut is_int = true;

                        while buffer.len() > 0
                            && (buffer[0].is_numeric() || buffer[0] == '.' || buffer[0] == '_')
                        {
                            if buffer[0] == '.' {
                                if buffer[1] == '.' {
                                    break;
                                }
                                is_int = false;
                            }
                            let c = buffer.remove(0);
                            self.increment_line_col(&c);
                            number.push(c);
                        }

                        if !buffer.is_empty() && buffer[0] == 'f' {
                            let c = buffer.remove(0);
                            self.increment_line_col(&c);
                            is_int = false;
                        }

                        let mut token = if is_int {
                            self.new_token(TokenType::Integer, number.trim())
                        } else {
                            self.new_token(TokenType::Float, number.trim())
                        };

                        token.value = number.replace("_", "");
                        Some(token)
                    } else if first.is_alphabetic()
                        || first == &'_'
                        || first.to_uppercase().to_string().trim()
                            != first.to_lowercase().to_string().trim()
                    {
                        let mut txt = String::new();
                        while buffer.len() > 0
                            && (buffer[0].is_alphanumeric()
                                || buffer[0] == '_'
                                || buffer[0].to_uppercase().to_string().trim()
                                    != buffer[0].to_lowercase().to_string().trim()
                                || buffer[0].is_numeric())
                            && !buffer[0].is_whitespace()
                        {
                            let char = buffer.remove(0);
                            self.increment_line_col(&char);
                            txt.push(char);
                        }

                        if let Some(identifier) = keywords().get(txt.trim()) {
                            Some(self.new_token(identifier.clone(), txt.trim()))
                        } else {
                            Some(self.new_token(TokenType::Identifier, txt.trim()))
                        }
                    } else {
                        return Err(self.get_unrecognized(*first));
                    }
                }
            };

            if let Some(token) = token {
                if let Some(last) = tokens.last() {
                    if last.value == "/" && (token.value == "*" || token.value == "/") {
                        let _ = tokens.pop();
                        let mut first = '/';
                        let mut second = '*';

                        let mut txt = String::new();
                        let can_continue = |f: char, s: char, short: bool| -> bool {
                            if short {
                                s != '\n'
                            } else {
                                f != '*' || s != '/'
                            }
                        };

                        while buffer.len() > 0 && can_continue(first, second, token.value == "/") {
                            first = second;
                            second = buffer.remove(0);
                            self.increment_line_col(&second);
                            txt.push(second);
                        }

                        txt = txt.trim_end().trim_end_matches("*/").trim().to_string();
                        tokens.push(self.new_token(TokenType::Comment, &txt));

                        continue;
                    }

                    let combined = format!("{}{}", last.value, token.value);

                    if let Some(t) = special_keywords().get(&combined) {
                        let token = self.new_token(t.clone(), &combined);
                        tokens.pop();
                        tokens.push(token);
                        continue;
                    }

                    if tokens.len() > 2 {
                        let combined = format!(
                            "{}{}{}",
                            tokens[tokens.len() - 2].value,
                            last.value,
                            token.value
                        );

                        if let Some(t) = special_keywords().get(&combined) {
                            let token = self.new_token(t.clone(), &combined);
                            tokens.pop();
                            tokens.pop();
                            tokens.push(token);
                            continue;
                        }
                    }

                    if let TokenType::BinaryOperator(x) = &last.token_type {
                        if token.token_type == TokenType::Equals {
                            let token =
                                self.new_token(TokenType::BinaryAssign(x.clone()), &last.value);
                            tokens.pop();
                            tokens.push(token);
                            continue;
                        }
                    }
                }
                tokens.push(token);
            }
        }

        tokens.push(self.new_token(TokenType::EOF, "EndOfFile"));

        if !self.include_comments {
            Ok(tokens
                .into_iter()
                .filter(|x| {
                    x.token_type != TokenType::Comment && x.token_type != TokenType::WhiteSpace
                })
                .collect())
        } else {
            Ok(tokens
                .into_iter()
                .filter(|x| x.token_type != TokenType::WhiteSpace)
                .collect())
        }
    }
}

pub fn tokenize(input: impl AsRef<str>) -> Result<Vec<Token>, LexerError> {
    let mut tokenizer = Tokenizer::default();
    tokenizer.tokenize(input.as_ref())
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_tokenize_same_line() {
        let input = String::from("let x = 5; if x > 3 { x = x + 1; }");

        let fin_tokens = vec![
            TokenType::Let,
            TokenType::Identifier,
            TokenType::Equals,
            TokenType::Integer,
            TokenType::If,
            TokenType::Identifier,
            TokenType::Comparison(ComparisonOperator::Greater),
            TokenType::Integer,
            TokenType::Open(Bracket::Curly),
            TokenType::Identifier,
            TokenType::Equals,
            TokenType::Identifier,
            TokenType::BinaryOperator(BinaryOperator::Add),
            TokenType::Integer,
            TokenType::Close(Bracket::Curly),
            TokenType::EOF,
        ];

        let tokens = tokenize(input).unwrap();

        for (i, token) in tokens.iter().enumerate() {
            assert_eq!(fin_tokens[i], token.token_type);
        }
    }

    #[test]
    fn test_tokenize_float_and_string() {
        let input = String::from("let pi = 3.14; let msg = \"hello\";");
        let fin_tokens = vec![
            TokenType::Let,
            TokenType::Identifier,
            TokenType::Equals,
            TokenType::Float,
            TokenType::Let,
            TokenType::Identifier,
            TokenType::Equals,
            TokenType::String,
            TokenType::EOF,
        ];
        let tokens = tokenize(input).unwrap();
        for (i, token) in tokens.iter().enumerate() {
            assert_eq!(fin_tokens[i], token.token_type);
        }
    }

    #[test]
    fn test_tokenize_operators_and_assignments() {
        let input = String::from("x += 2; y--; z *= 3;");
        let fin_tokens = vec![
            TokenType::Identifier,
            TokenType::BinaryAssign(BinaryOperator::Add),
            TokenType::Integer,
            TokenType::Identifier,
            TokenType::UnaryAssign(BinaryOperator::Sub),
            TokenType::Identifier,
            TokenType::BinaryAssign(BinaryOperator::Mul),
            TokenType::Integer,
            TokenType::EOF,
        ];
        let tokens = tokenize(input).unwrap();
        for (i, token) in tokens.iter().enumerate() {
            assert_eq!(fin_tokens[i], token.token_type);
        }
    }

    #[test]
    fn test_tokenize_special_keywords() {
        let input = String::from("if x >= 10 && y <= 5 { return; }");
        let fin_tokens = vec![
            TokenType::If,
            TokenType::Identifier,
            TokenType::Comparison(ComparisonOperator::GreaterEqual),
            TokenType::Integer,
            TokenType::Boolean(BooleanOperator::And),
            TokenType::Identifier,
            TokenType::Comparison(ComparisonOperator::LesserEqual),
            TokenType::Integer,
            TokenType::Open(Bracket::Curly),
            TokenType::Stop(StopValue::Return),
            TokenType::Close(Bracket::Curly),
            TokenType::EOF,
        ];
        let tokens = tokenize(input).unwrap();
        for (i, token) in tokens.iter().enumerate() {
            assert_eq!(fin_tokens[i], token.token_type);
        }
    }

    #[test]
    fn test_tokenize_comment_skipping() {
        let input = String::from("let x = 1; // this is a comment\nx = x + 1;");
        let fin_tokens = vec![
            TokenType::Let,
            TokenType::Identifier,
            TokenType::Equals,
            TokenType::Integer,
            TokenType::Identifier,
            TokenType::Equals,
            TokenType::Identifier,
            TokenType::BinaryOperator(BinaryOperator::Add),
            TokenType::Integer,
            TokenType::EOF,
        ];
        let tokens = tokenize(input).unwrap();
        for (i, token) in tokens.iter().enumerate() {
            assert_eq!(fin_tokens[i], token.token_type);
        }
    }

    #[test]
    fn test_tokenize_multiline_comment() {
        let input = String::from("let x = 1; /* comment \n block */ x = 2;");
        let fin_tokens = vec![
            TokenType::Let,
            TokenType::Identifier,
            TokenType::Equals,
            TokenType::Integer,
            TokenType::Identifier,
            TokenType::Equals,
            TokenType::Integer,
            TokenType::EOF,
        ];
        let tokens = tokenize(input).unwrap();
        for (i, token) in tokens.iter().enumerate() {
            assert_eq!(fin_tokens[i], token.token_type);
        }
    }
}
