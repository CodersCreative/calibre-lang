use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenType {
    Number,
    Identifier,
    Equals,
    OpenBrackets,
    CloseBrackets,
    BinaryOperator,
    Var,
    Const,
    EOF,
}

pub fn keywords() -> HashMap<String, TokenType> {
    HashMap::from([
        (String::from("var"), TokenType::Var),
        (String::from("const"), TokenType::Const),
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
    let mut buffer = String::new();
    let mut tokens: Vec<Token> = txt
        .chars()
        .filter(|x| !x.is_whitespace())
        .enumerate()
        .map(|(i, c)| {
            let mut token = match c {
                '(' => Some(Token::new(TokenType::OpenBrackets, "(")),
                ')' => Some(Token::new(TokenType::CloseBrackets, "(")),
                '+' | '-' => Some(Token::new(TokenType::BinaryOperator, &c.to_string())),
                '*' | '/' | '^' | '%' => {
                    Some(Token::new(TokenType::BinaryOperator, &c.to_string()))
                }
                '=' => Some(Token::new(TokenType::Equals, "=")),
                _ => {
                    if c.is_alphanumeric() && !c.is_whitespace() {
                        buffer.push(c);
                    }

                    None
                }
            };

            if !buffer.is_empty() {
                let next = txt.chars().nth(i + 1).unwrap_or(c);
                if token.is_none()
                    && (i >= txt.len() - 1
                        || next.is_whitespace()
                        || buffer.chars().nth(0).unwrap().is_alphabetic() != next.is_alphabetic()
                        || buffer.chars().nth(0).unwrap().is_numeric() != next.is_numeric())
                {
                    token = match buffer.chars().last().unwrap().is_alphabetic() {
                        true => {
                            if let Some(identifier) = keywords().get(buffer.trim()) {
                                Some(Token::new(identifier.clone(), buffer.trim()))
                            } else {
                                Some(Token::new(TokenType::Identifier, buffer.trim()))
                            }
                        }
                        false => Some(Token::new(TokenType::Number, buffer.trim())),
                    };
                    buffer.clear();
                } else if token.is_some() {
                    buffer.clear();
                }
                // return None;
            }

            token
        })
        .filter(|x| x.is_some())
        .map(|x| x.unwrap())
        .collect();

    tokens.push(Token::new(TokenType::EOF, "EndOfFile"));

    tokens
}
