use crate::ast::{
    binary::BinaryOperator,
    comparison::{BooleanOperation, Comparison},
};
use std::collections::HashMap;
use thiserror::Error;

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
    UnaryAssign(BinaryOperator),
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
    pub line: usize,
    pub col: usize,
}

impl Token {
    pub fn new(token_type: TokenType, value: &str, line: usize, col: usize) -> Self {
        Self {
            value: value.to_string(),
            token_type,
            line,
            col,
        }
    }
}

fn increment_line_col(line: &mut usize, col: &mut usize, c: &char) {
    if c == &'\n' {
        *line += 1;
        *col = 0;
    } else if c.is_whitespace() {
        *col += 1;
    }
}

pub fn tokenize(txt: String) -> Result<Vec<Token>, LexerError> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut buffer: Vec<char> = txt.chars().collect();
    let mut line: usize = 1;
    let mut col: usize = 0;

    while buffer.len() > 0 {
        let first = buffer.first().unwrap();

        increment_line_col(&mut line, &mut col, first);

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
            Some(t) => Some(Token::new(
                t,
                buffer.remove(0).to_string().trim(),
                line,
                col,
            )),
            _ => {
                if first == &'"' {
                    let mut txt = String::new();

                    let _ = buffer.remove(0);

                    while buffer[0] != '"' {
                        increment_line_col(&mut line, &mut col, &buffer[0]);
                        txt.push(buffer.remove(0));
                    }

                    let _ = buffer.remove(0);

                    Some(Token::new(TokenType::String, &txt, line, col))
                } else if first.is_numeric(){
                    let mut number = String::new();
                    let mut is_int = true;

                    while buffer.len() > 0 && (buffer[0].is_numeric() || buffer[0] == '.') {
                        if buffer[0] == '.' {
                            if buffer[1] == '.' {
                                break;
                            }
                            is_int = false;
                        }
                        number.push(buffer.remove(0));
                    }

                    if is_int {
                        Some(Token::new(TokenType::Integer, number.trim(), line, col))
                    } else {
                        Some(Token::new(TokenType::Float, number.trim(), line, col))
                    }
                } else if first.is_alphabetic() || first == &'_'{
                    let mut txt = String::new();
                    while buffer.len() > 0
                        && (buffer[0].is_alphanumeric() || buffer[0] == '_')
                        && !buffer[0].is_whitespace()
                    {
                        txt.push(buffer.remove(0));
                    }

                    if let Some(identifier) = keywords().get(txt.trim()) {
                        Some(Token::new(identifier.clone(), txt.trim(), line, col))
                    } else {
                        Some(Token::new(TokenType::Identifier, txt.trim(), line, col))
                    }
                } else {
                    return Err(LexerError::Unrecognized(*first));
                }
            }
        };

        if let Some(token) = token {
            if let Some(last) = tokens.last(){
                if last.token_type != TokenType::WhiteSpace && token.token_type != TokenType::WhiteSpace{
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

                        while buffer.len() > 0 && can_continue(first, second, token.value == "/") {
                            first = second;
                            second = buffer.remove(0);
                            increment_line_col(&mut line, &mut col, &second);
                        }

                        continue;
                    }

                    let combined = format!("{}{}", last.value, token.value);

                    if let Some(t) = special_keywords().get(&combined) {
                        if token.col > 0 && last.col / token.col == 1 {
                            let token = Token::new(t.clone(), &combined, line, col);
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
                            if token.col > 0 && last.col / token.col == 1 {
                                let token = Token::new(t.clone(), &combined, line, col);
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
                                Token::new(TokenType::BinaryAssign(x.clone()), &last.value, line, col);
                            tokens.pop();
                            tokens.push(token);
                            continue;
                        }
                    }

                    if let TokenType::BinaryOperator(x) = &last.token_type {
                        if let TokenType::BinaryOperator(y) = token.token_type {
                            if x != &y {
                                return Err(LexerError::BinaryOperatorShortHand);
                            }
                            let token = Token::new(TokenType::UnaryAssign(y), &last.value, line, col);
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

    tokens.push(Token::new(TokenType::EOF, "EndOfFile", line, col));

    Ok(tokens.into_iter().filter(|x| x.token_type != TokenType::WhiteSpace).collect())
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
