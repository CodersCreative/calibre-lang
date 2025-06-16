use core::panic;
use std::collections::HashMap;

const IGNORE : [char; 1] = [';'];

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenType {
    Number,
    Identifier,
    Equals,
    OpenBrackets,
    CloseBrackets,
    BinaryOperator,
    Var,
    Let,
    EOF,
}

pub fn keywords() -> HashMap<String, TokenType> {
    HashMap::from([
        (String::from("var"), TokenType::Var),
        (String::from("let"), TokenType::Let),
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
    let mut tokens = Vec::new();
    let mut buffer: Vec<char> = txt.chars().collect();
    while buffer.len() > 0 {
        let first = buffer.first().unwrap();
        let token = match first {
            '(' => Some(Token::new(
                TokenType::OpenBrackets,
                buffer.remove(0).to_string().trim(),
            )),
            ')' => Some(Token::new(
                TokenType::CloseBrackets,
                buffer.remove(0).to_string().trim(),
            )),
            '+' | '-' => Some(Token::new(
                TokenType::BinaryOperator,
                buffer.remove(0).to_string().trim(),
            )),
            '*' | '/' | '^' | '%' => Some(Token::new(
                TokenType::BinaryOperator,
                buffer.remove(0).to_string().trim(),
            )),
            '=' => Some(Token::new(
                TokenType::Equals,
                buffer.remove(0).to_string().trim(),
            )),
            _ => {
                let ignore = IGNORE.contains(first);

                if first.is_whitespace() && !ignore {
                    let _ = buffer.remove(0);
                    None
                } else if first.is_numeric() && !ignore {
                    let mut number = String::new();
                    while buffer.len() > 0 && buffer[0].is_numeric() {
                        number.push(buffer.remove(0));
                    }
                    Some(Token::new(TokenType::Number, number.trim()))
                } else if first.is_alphabetic() && !ignore {
                    let mut txt = String::new();
                    while buffer.len() > 0
                        && buffer[0].is_alphanumeric()
                        && !buffer[0].is_whitespace()
                    {
                        txt.push(buffer.remove(0));
                    }

                    if let Some(identifier) = keywords().get(txt.trim()) {
                        Some(Token::new(identifier.clone(), txt.trim()))
                    } else {
                        Some(Token::new(TokenType::Identifier, txt.trim()))
                    }
                } else if ignore{
                    let _ = buffer.remove(0);
                    None
                } else{
                    panic!("Unrecognized character : {}", first);
                }
            }
        };

        if let Some(token) = token {
            tokens.push(token);
        }
    }

    tokens.push(Token::new(TokenType::EOF, "EndOfFile"));

    tokens
}
