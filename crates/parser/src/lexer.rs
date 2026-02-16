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

#[derive(
    Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Serialize, Deserialize,
)]
pub struct Position {
    pub line: u32,
    pub col: u32,
}

pub struct Tokenizer {
    pub include_comments: bool,
    line: u32,
    col: u32,
}

#[derive(Error, Debug, Clone, PartialEq)]
pub enum LexerError {
    #[error("Unrecognized character: '{ch}'")]
    Unrecognized { span: Span, ch: char },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Bracket {
    Curly,
    Paren,
    Square,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum StopValue {
    Return,
    Break,
    Continue,
    Until,
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    Func,
    If,
    In,
    Or,
    Main,
    As,
    Try,
    Use,
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
    Extern,
    Spawn,
    Select,
}

pub fn keywords() -> &'static HashMap<String, TokenType> {
    static KEYWORDS: std::sync::OnceLock<HashMap<String, TokenType>> = std::sync::OnceLock::new();
    KEYWORDS.get_or_init(|| {
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
            (String::from("use"), TokenType::Use),
            (String::from("if"), TokenType::If),
            (String::from("as"), TokenType::As),
            (String::from("struct"), TokenType::Struct),
            (String::from("impl"), TokenType::Impl),
            (String::from("trait"), TokenType::Trait),
            (String::from("Self"), TokenType::This),
            (String::from("for"), TokenType::For),
            (String::from("import"), TokenType::Import),
            (String::from("from"), TokenType::From),
            (String::from("type"), TokenType::Type),
            (String::from("extern"), TokenType::Extern),
            (String::from("select"), TokenType::Select),
            (String::from("spawn"), TokenType::Spawn),
        ])
    })
}

pub fn special_keywords() -> &'static HashMap<String, TokenType> {
    static SPECIAL_KEYWORDS: std::sync::OnceLock<HashMap<String, TokenType>> =
        std::sync::OnceLock::new();
    SPECIAL_KEYWORDS.get_or_init(|| {
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
    })
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
            self.col = 1;
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
        let mut tokens: Vec<Token> = Vec::with_capacity(txt.len().saturating_add(1) / 2);
        let buffer: Vec<char> = txt.chars().collect();
        let keywords = keywords();
        let special_keywords = special_keywords();
        self.line = 1;
        self.col = 1;

        let mut i = 0usize;
        let len = buffer.len();

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
                '<' | '>' => {
                    ComparisonOperator::from_operator(&c.to_string()).map(TokenType::Comparison)
                }
                '.' => Some(TokenType::FullStop),
                ':' => Some(TokenType::Colon),
                '=' => Some(TokenType::Equals),
                '!' => Some(TokenType::Not),
                '+' | '-' | '*' | '/' | '^' | '%' => {
                    BinaryOperator::from_symbol(&c.to_string()).map(TokenType::BinaryOperator)
                }
                ';' => Some(TokenType::EOL),
                _ if c.is_whitespace() => Some(TokenType::WhiteSpace),
                _ => None,
            }
        };

        while i < len {
            let first = buffer[i];

            let token: Option<Token> = if let Some(token_type) = get_token(first) {
                i += 1;
                self.increment_line_col(&first);
                Some(self.new_token(token_type, &first.to_string()))
            } else if first.is_numeric() {
                let mut is_int = true;
                if first == '0' && i + 1 < len {
                    let prefix = buffer[i + 1];
                    let base = match prefix {
                        'b' | 'B' => Some(2),
                        'x' | 'X' => Some(16),
                        _ => None,
                    };
                    if let Some(base) = base {
                        i += 1;
                        self.increment_line_col(&first);
                        i += 1;
                        self.increment_line_col(&prefix);
                        let mut value: u64 = 0;
                        let mut saw = false;
                        while i < len {
                            let c = buffer[i];
                            if c == '_' {
                                i += 1;
                                self.increment_line_col(&c);
                                continue;
                            }
                            if let Some(digit) = c.to_digit(base) {
                                value = value
                                    .checked_mul(base as u64)
                                    .and_then(|v| v.checked_add(digit as u64))
                                    .unwrap_or(value);
                                saw = true;
                                i += 1;
                                self.increment_line_col(&c);
                                continue;
                            }
                            break;
                        }
                        if !saw {
                            return Err(self.get_unrecognized(first));
                        }
                        let token = self.new_token(TokenType::Integer, &value.to_string());
                        Some(token)
                    } else {
                        let mut number = String::new();
                        while i < len
                            && (buffer[i].is_numeric() || buffer[i] == '.' || buffer[i] == '_')
                        {
                            if buffer[i] == '.' {
                                if i + 1 < len && buffer[i + 1] == '.' {
                                    break;
                                }
                                is_int = false;
                            }
                            let c = buffer[i];
                            i += 1;
                            self.increment_line_col(&c);
                            number.push(c);
                        }

                        if i < len && buffer[i] == 'f' {
                            let c = buffer[i];
                            i += 1;
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
                    }
                } else {
                    let mut number = String::new();
                    while i < len
                        && (buffer[i].is_numeric() || buffer[i] == '.' || buffer[i] == '_')
                    {
                        if buffer[i] == '.' {
                            if i + 1 < len && buffer[i + 1] == '.' {
                                break;
                            }
                            is_int = false;
                        }
                        let c = buffer[i];
                        i += 1;
                        self.increment_line_col(&c);
                        number.push(c);
                    }

                    if i < len && buffer[i] == 'f' {
                        let c = buffer[i];
                        i += 1;
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
                }
            } else if first == '"' {
                i += 1;
                self.increment_line_col(&first);
                let mut txt = String::new();
                let mut escaped = false;
                while i < len {
                    let ch = buffer[i];
                    i += 1;
                    self.increment_line_col(&ch);
                    if escaped {
                        let resolved = match ch {
                            'n' => '\n',
                            'r' => '\r',
                            't' => '\t',
                            '\\' => '\\',
                            '"' => '"',
                            other => other,
                        };
                        txt.push(resolved);
                        escaped = false;
                        continue;
                    }
                    if ch == '\\' {
                        escaped = true;
                        continue;
                    }
                    if ch == '"' {
                        break;
                    }
                    txt.push(ch);
                }
                if escaped || (i == len && buffer[len - 1] != '"') {
                    return Err(self.get_unrecognized(first));
                }
                Some(self.new_token(TokenType::String, &txt))
            } else if first == '\'' {
                i += 1;
                self.increment_line_col(&first);
                if i >= len {
                    return Err(self.get_unrecognized(first));
                }
                let mut ch = buffer[i];
                i += 1;
                self.increment_line_col(&ch);
                if ch == '\\' {
                    if i >= len {
                        return Err(self.get_unrecognized(first));
                    }
                    let next = buffer[i];
                    i += 1;
                    self.increment_line_col(&next);
                    ch = match next {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '\\' => '\\',
                        '\'' => '\'',
                        other => other,
                    };
                }
                if i >= len || buffer[i] != '\'' {
                    return Err(self.get_unrecognized(first));
                }
                let closing = buffer[i];
                i += 1;
                self.increment_line_col(&closing);
                let ch_str = ch.to_string();
                Some(self.new_token(TokenType::Char, &ch_str))
            } else if first.is_alphabetic()
                || first == '_'
                || first.to_uppercase().to_string().trim()
                    != first.to_lowercase().to_string().trim()
            {
                let mut txt = String::new();
                while i < len
                    && (buffer[i].is_alphanumeric()
                        || buffer[i] == '_'
                        || buffer[i].to_uppercase().to_string().trim()
                            != buffer[i].to_lowercase().to_string().trim()
                        || buffer[i].is_numeric())
                    && !buffer[i].is_whitespace()
                {
                    let ch = buffer[i];
                    i += 1;
                    self.increment_line_col(&ch);
                    txt.push(ch);
                }

                if let Some(identifier) = keywords.get(txt.trim()) {
                    Some(self.new_token(identifier.clone(), txt.trim()))
                } else {
                    Some(self.new_token(TokenType::Identifier, txt.trim()))
                }
            } else {
                return Err(self.get_unrecognized(first));
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

                        while i < len && can_continue(first, second, token.value == "/") {
                            first = second;
                            second = buffer[i];
                            i += 1;
                            self.increment_line_col(&second);
                            txt.push(second);
                        }

                        txt = txt.trim_end().trim_end_matches("*/").trim().to_string();
                        tokens.push(self.new_token(TokenType::Comment, &txt));

                        continue;
                    }

                    let combined = format!("{}{}", last.value, token.value);

                    if let Some(t) = special_keywords.get(&combined) {
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

                        if let Some(t) = special_keywords.get(&combined) {
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
            TokenType::BinaryAssign(BinaryOperator::Sub),
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
