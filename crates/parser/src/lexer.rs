use crate::ast::{
    binary::BinaryOperator,
    comparison::{BooleanOperation, Comparison},
};
use std::{collections::HashMap, path::PathBuf};
use thiserror::Error;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    pub from: Position,
    pub to: Position,
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Position {
    pub line: u32,
    pub col: u32,
}

pub struct Tokenizer {
    pub include_comments: bool,
    line: u32,
    col: u32,
}

#[derive(Error, Debug)]
pub enum LexerError {
    #[error("Unrecognized character '{0}'")]
    Unrecognized(char),

    #[error("Can only use short hand assignment with similar operators e.g. x++ or x--")]
    BinaryOperatorShortHand,
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
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenType {
    Float,
    Integer,
    String,
    Identifier,
    Equals,
    Open(Bracket),
    Close(Bracket),
    Colon,
    Comma,
    Comparison(Comparison),
    Boolean(BooleanOperation),
    BinaryOperator(BinaryOperator),
    BinaryAssign(BinaryOperator),
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
    Object,
    Question,
    Async,
    Func,
    If,
    In,
    Is,
    Or,
    Main,
    As,
    Try,
    This,
    FullStop,
    EOL,
    EOF,
    Struct,
    Else,
    Pipe,
    From,
    Import,
    Type,
    WhiteSpace,
}

pub fn keywords() -> HashMap<String, TokenType> {
    HashMap::from([
        (String::from("mut"), TokenType::Mut),
        (String::from("const"), TokenType::Const),
        (String::from("let"), TokenType::Let),
        (String::from("enum"), TokenType::Enum),
        (String::from("match"), TokenType::Match),
        (String::from("obj"), TokenType::Object),
        (String::from("fn"), TokenType::Func),
        (String::from("else"), TokenType::Else),
        (String::from("list"), TokenType::List),
        (String::from("return"), TokenType::Stop(StopValue::Return)),
        (String::from("in"), TokenType::In),
        (String::from("break"), TokenType::Stop(StopValue::Break)),
        (
            String::from("continue"),
            TokenType::Stop(StopValue::Continue),
        ),
        (String::from("try"), TokenType::Try),
        (String::from("if"), TokenType::If),
        (String::from("is"), TokenType::Is),
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
        (String::from("<-"), TokenType::LeftArrow),
        (String::from("=>"), TokenType::FatArrow),
        (String::from(".."), TokenType::Range),
        (
            String::from("**="),
            TokenType::BinaryAssign(BinaryOperator::Pow),
        ),
        (
            String::from("&="),
            TokenType::BinaryOperator(BinaryOperator::BitAnd),
        ),
        (
            String::from("|="),
            TokenType::BinaryOperator(BinaryOperator::BitOr),
        ),
        (
            String::from("**"),
            TokenType::BinaryOperator(BinaryOperator::Pow),
        ),
        (
            String::from("<<"),
            TokenType::BinaryOperator(BinaryOperator::Shl),
        ),
        (
            String::from(">>"),
            TokenType::BinaryOperator(BinaryOperator::Shr),
        ),
        (
            String::from("&&"),
            TokenType::Boolean(BooleanOperation::And),
        ),
        (String::from("||"), TokenType::Boolean(BooleanOperation::Or)),
        (String::from("&mut"), TokenType::RefMut),
        (
            String::from(">="),
            TokenType::Comparison(Comparison::GreaterEqual),
        ),
        (
            String::from("<="),
            TokenType::Comparison(Comparison::LesserEqual),
        ),
        (String::from("=="), TokenType::Comparison(Comparison::Equal)),
        (
            String::from("!="),
            TokenType::Comparison(Comparison::NotEqual),
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
    pub fn tokenize(&mut self, txt: String) -> Result<Vec<Token>, LexerError> {
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
                    '&' => Some(TokenType::Ref),
                    '$' => Some(TokenType::Identifier),
                    '?' => Some(TokenType::Question),
                    '<' | '>' => Some(TokenType::Comparison(
                        Comparison::from_operator(&c.to_string()).unwrap(),
                    )),
                    '.' => Some(TokenType::FullStop),
                    ':' => Some(TokenType::Colon),
                    '=' => Some(TokenType::Equals),
                    '!' => Some(TokenType::Not),
                    '+' | '-' | '*' | '/' | '^' | '%' => Some(TokenType::BinaryOperator(
                        BinaryOperator::from_symbol(&c.to_string()).unwrap(),
                    )),
                    ';' => Some(TokenType::WhiteSpace),
                    _ if c.is_whitespace() => Some(TokenType::WhiteSpace),
                    _ => None,
                }
            };

            let token = match get_token(*first) {
                Some(t) => {
                    self.increment_line_col(first);

                    Some(self.new_token(t, buffer.remove(0).to_string().trim()))
                }
                _ => {
                    if first == &'"' {
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

                        while buffer.len() > 0 && (buffer[0].is_numeric() || buffer[0] == '.') {
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

                        if is_int {
                            Some(self.new_token(TokenType::Integer, number.trim()))
                        } else {
                            Some(self.new_token(TokenType::Float, number.trim()))
                        }
                    } else if first.is_alphabetic() || first == &'_' {
                        let mut txt = String::new();
                        while buffer.len() > 0
                            && (buffer[0].is_alphanumeric() || buffer[0] == '_')
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
                        return Err(LexerError::Unrecognized(*first));
                    }
                }
            };

            if let Some(token) = token {
                if let Some(last) = tokens.last() {
                    if last.token_type != TokenType::WhiteSpace
                        && token.token_type != TokenType::WhiteSpace
                    {
                        if last.value == "/" && (token.value == "*" || token.value == "/") {
                            let _ = tokens.pop();
                            let mut first = '/';
                            let mut second = '*';

                            let can_continue = |f: char, s: char, short: bool| -> bool {
                                if short {
                                    s != '\n'
                                } else {
                                    f != '*' || s != '/'
                                }
                            };

                            while buffer.len() > 0
                                && can_continue(first, second, token.value == "/")
                            {
                                first = second;
                                second = buffer.remove(0);
                                self.increment_line_col(&second);
                            }

                            continue;
                        }

                        let combined = format!("{}{}", last.value, token.value);

                        if let Some(t) = special_keywords().get(&combined) {
                            if token.span.to.col > 0 && last.span.to.col / token.span.to.col == 1 {
                                let token = self.new_token(t.clone(), &combined);
                                tokens.pop();
                                tokens.push(token);
                                continue;
                            }
                        }

                        if tokens.len() > 2 {
                            let combined = format!(
                                "{}{}{}",
                                tokens[tokens.len() - 2].value,
                                last.value,
                                token.value
                            );

                            if let Some(t) = special_keywords().get(&combined) {
                                if token.span.to.col > 0
                                    && last.span.to.col / token.span.to.col == 1
                                {
                                    let token = self.new_token(t.clone(), &combined);
                                    tokens.pop();
                                    tokens.pop();
                                    tokens.push(token);
                                    continue;
                                }
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
                }
                tokens.push(token);
            }
        }

        tokens.push(self.new_token(TokenType::EOF, "EndOfFile"));

        Ok(tokens
            .into_iter()
            .filter(|x| x.token_type != TokenType::WhiteSpace)
            .collect())
    }
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
            TokenType::Comparison(Comparison::Greater),
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
            TokenType::Comparison(Comparison::GreaterEqual),
            TokenType::Integer,
            TokenType::Boolean(BooleanOperation::And),
            TokenType::Identifier,
            TokenType::Comparison(Comparison::LesserEqual),
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
