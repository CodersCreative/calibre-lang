use crate::ast::BinaryOperator;
use core::panic;
use std::collections::HashMap;

const IGNORE: [char; 1] = [';'];

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenType {
    Float,
    Integer,
    String,
    Identifier,
    Equals,
    OpenBrackets,
    CloseBrackets,
    OpenCurly,
    CloseCurly,
    OpenSquare,
    CloseSquare,
    Colon,
    Comma,
    BinaryOperator,
    BinaryAssign,
    UnaryAssign,
    Greater,
    Var,
    Let,
    List,
    Arrow,
    Async,
    Loop,
    Func,
    Return,
    If,
    Scope,
    FullStop,
    EOF,
    Struct,
}

pub fn keywords() -> HashMap<String, TokenType> {
    HashMap::from([
        (String::from("var"), TokenType::Var),
        (String::from("let"), TokenType::Let),
        (String::from("for"), TokenType::Loop),
        (String::from("scope"), TokenType::Scope),
        (String::from("fn"), TokenType::Func),
        (String::from("list"), TokenType::List),
        (String::from("return"), TokenType::Return),
        (String::from("if"), TokenType::If),
        (String::from("func"), TokenType::Func),
        (String::from("struct"), TokenType::Struct),
        (String::from("async"), TokenType::Async),
    ])
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Token {
    pub value: String,
    pub token_type: TokenType,
}

impl Token {
    pub fn new(token_type: TokenType, value: &str) -> Self {
        Self {
            value: value.to_string(),
            token_type,
        }
    }
}

pub fn tokenize(txt: String) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut buffer: Vec<char> = txt.chars().collect();
    let mut in_str = false;

    while buffer.len() > 0 {
        let first = buffer.first().unwrap();

        let get_token = |c: char| -> Option<TokenType> {
            match c {
                '(' => Some(TokenType::OpenBrackets),
                ')' => Some(TokenType::CloseBrackets),
                '{' => Some(TokenType::OpenCurly),
                '}' => Some(TokenType::CloseCurly),
                '[' => Some(TokenType::OpenSquare),
                ']' => Some(TokenType::CloseSquare),
                ',' => Some(TokenType::Comma),
                '>' => Some(TokenType::Greater),
                '.' => Some(TokenType::FullStop),
                ':' => Some(TokenType::Colon),
                '+' | '-' => Some(TokenType::BinaryOperator),
                '*' | '/' | '^' | '%' => Some(TokenType::BinaryOperator),
                '=' => Some(TokenType::Equals),
                _ => None,
            }
            // Some(Token::new(t, buffer.remove(0).to_string().trim()))
        };

        let token = match get_token(*first) {
            Some(t) => Some(Token::new(t, buffer.remove(0).to_string().trim())),
            _ => {
                let ignore = IGNORE.contains(first);

                if first == &'"' {
                    let mut txt = String::new();

                    let _ = buffer.remove(0);

                    while buffer[0] != '"' {
                        txt.push(buffer.remove(0));
                    }

                    let _ = buffer.remove(0);

                    Some(Token::new(TokenType::String, &txt))
                } else if first.is_whitespace() && !ignore {
                    let _ = buffer.remove(0);
                    None
                } else if first.is_numeric() && !ignore {
                    let mut number = String::new();
                    let mut is_int = true;
                    while buffer.len() > 0 && (buffer[0].is_numeric() || buffer[0] == '.') {
                        if buffer[0] == '.' {
                            is_int = false;
                        }
                        number.push(buffer.remove(0));
                    }

                    if is_int {
                        Some(Token::new(TokenType::Integer, number.trim()))
                    } else {
                        Some(Token::new(TokenType::Float, number.trim()))
                    }
                } else if (first.is_alphabetic() || first == &'_') && !ignore {
                    let mut txt = String::new();
                    while buffer.len() > 0
                        && (buffer[0].is_alphanumeric() || buffer[0] == '_')
                        && !buffer[0].is_whitespace()
                    {
                        txt.push(buffer.remove(0));
                    }

                    if let Some(identifier) = keywords().get(txt.trim()) {
                        Some(Token::new(identifier.clone(), txt.trim()))
                    } else {
                        Some(Token::new(TokenType::Identifier, txt.trim()))
                    }
                } else if ignore {
                    let _ = buffer.remove(0);
                    None
                } else {
                    panic!("Unrecognized character : {}", first);
                }
            }
        };

        if let Some(token) = token {
            if let Some(last) = tokens.last() {
                if last.value == "/" && (token.value == "*" || token.value == "/") {
                    tokens.pop();
                    let mut first = '/';
                    let mut second = '*';

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
                    }

                    continue;
                }

                if last.value == "-" && token.token_type == TokenType::Greater {
                    let token = Token::new(TokenType::Arrow, "->");
                    tokens.pop();
                    tokens.push(token);
                    continue;
                }

                if last.token_type == TokenType::BinaryOperator
                    && token.token_type == TokenType::Equals
                {
                    let token = Token::new(TokenType::BinaryAssign, &last.value);
                    tokens.pop();
                    tokens.push(token);
                    continue;
                }

                if last == &token && token.token_type == TokenType::BinaryOperator {
                    let token = Token::new(TokenType::UnaryAssign, &last.value);
                    tokens.pop();
                    tokens.push(token);
                    continue;
                } else if token.token_type == TokenType::BinaryOperator
                    && last.token_type == TokenType::BinaryOperator
                {
                    panic!(
                        "Can only use short hand assignment with similar operators e.g. x++ or x--"
                    );
                }
            }
            tokens.push(token);
        }
    }

    tokens.push(Token::new(TokenType::EOF, "EndOfFile"));

    tokens
}
