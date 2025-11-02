pub mod binary;
pub mod declarations;
pub mod expressions;
pub mod functions;
pub mod r#type;

use crate::{
    Parser, ParserError, SyntaxErr,
    ast::{
        LoopType, Node, NodeType, ObjectType, ParserDataType, RefMutability, comparison::Comparison,
    },
    lexer::{Bracket, Span, Token, TokenType},
};
use std::{collections::HashMap, str::FromStr};

impl Parser {
    fn first(&self) -> &Token {
        self.tokens.first().unwrap()
    }

    fn second(&self) -> &Token {
        self.nth(1).unwrap()
    }

    fn nth(&self, i: usize) -> Option<&Token> {
        self.tokens.get(i)
    }

    fn eat(&mut self) -> Token {
        let token = self.tokens.remove(0);
        self.prev_token = Some(token.clone());
        token
    }

    fn add_err(&mut self, err: SyntaxErr) {
        self.errors.push(ParserError::Syntax(
            err,
            self.prev_token.clone(),
            self.tokens.first().map(|x| x.clone()),
        ))
    }

    fn expect_eat(&mut self, t: &TokenType, err: SyntaxErr) -> Token {
        if &self.first().token_type != t {
            self.add_err(err)
        }
        self.eat()
    }

    fn expect_type(&mut self) -> ParserDataType {
        if let Some(x) = self.parse_type() {
            x
        } else {
            self.add_err(SyntaxErr::ExpectedType);
            ParserDataType::Dynamic
        }
    }

    fn parse_key_type_list_ordered_with_ref(
        &mut self,
        open_token: TokenType,
        close_token: TokenType,
    ) -> Vec<(String, ParserDataType, Option<Node>)> {
        let mut properties = Vec::new();
        let mut defaulted = false;
        let _ = self.expect_eat(&open_token, SyntaxErr::ExpectedToken(open_token.clone()));

        while !self.is_eof() && self.first().token_type != close_token {
            let mut keys = Vec::new();

            while self.first().token_type == TokenType::Identifier {
                keys.push(
                    self.expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedKey)
                        .value,
                );
            }

            let typ = if self.first().token_type == TokenType::Colon {
                let _ = self.expect_eat(&TokenType::Colon, SyntaxErr::ExpectedChar(':'));

                self.expect_type()
            } else {
                ParserDataType::Dynamic
            };

            let default = if defaulted || self.first().token_type == TokenType::Equals {
                let _ = self.expect_eat(&TokenType::Equals, SyntaxErr::ExpectedChar('='));
                defaulted = true;
                Some(self.parse_statement())
            } else {
                None
            };

            for key in keys {
                properties.push((key, typ.clone(), default.clone()));
            }

            if self.first().token_type != close_token {
                let _ = self.expect_eat(&TokenType::Comma, SyntaxErr::ExpectedChar(','));
            }
        }

        let _ = self.expect_eat(&close_token, SyntaxErr::ExpectedToken(close_token.clone()));

        properties
    }

    fn parse_key_type_list_object_val(&mut self) -> ObjectType<ParserDataType> {
        let mut tuple = Vec::new();
        let mut properties = HashMap::new();

        let (is_tuple, _, close_token) = match self.eat().token_type {
            TokenType::Open(Bracket::Curly) => (
                false,
                TokenType::Open(Bracket::Curly),
                TokenType::Close(Bracket::Curly),
            ),
            TokenType::Open(Bracket::Paren) => (
                true,
                TokenType::Open(Bracket::Paren),
                TokenType::Close(Bracket::Paren),
            ),
            _ => {
                self.add_err(SyntaxErr::ExpectedOpeningBracket(Bracket::Curly));

                (
                    true,
                    TokenType::Open(Bracket::Paren),
                    TokenType::Close(Bracket::Paren),
                )
            }
        };

        while !self.is_eof() && self.first().token_type != close_token {
            if is_tuple {
                tuple.push(self.expect_type());
            } else {
                let key = self
                    .expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedKey)
                    .value;

                let _ = self.expect_eat(&TokenType::Colon, SyntaxErr::ExpectedChar(':'));

                properties.insert(key, self.expect_type());
            }

            if self.first().token_type != close_token {
                let _ = self.expect_eat(&TokenType::Comma, SyntaxErr::ExpectedChar(','));
            }
        }

        let _ = self.expect_eat(&close_token, SyntaxErr::ExpectedToken(close_token.clone()));

        if is_tuple {
            ObjectType::Tuple(tuple)
        } else {
            ObjectType::Map(properties)
        }
    }

    fn parse_key_type_list(
        &mut self,
        open_token: TokenType,
        close_token: TokenType,
    ) -> HashMap<String, ParserDataType> {
        let mut properties = HashMap::new();
        let _ = self.expect_eat(&open_token, SyntaxErr::ExpectedToken(open_token.clone()));

        while !self.is_eof() && self.first().token_type != close_token {
            let key = self
                .expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedKey)
                .value;

            let _ = self.expect_eat(&TokenType::Colon, SyntaxErr::ExpectedChar(':'));

            properties.insert(key, self.expect_type());

            if self.first().token_type != close_token {
                let _ = self.expect_eat(&TokenType::Comma, SyntaxErr::ExpectedChar(','));
            }
        }

        let _ = self.expect_eat(&close_token, SyntaxErr::ExpectedToken(close_token.clone()));

        properties
    }

    fn parse_type_list(
        &mut self,
        open_token: TokenType,
        close_token: TokenType,
    ) -> Vec<ParserDataType> {
        let mut properties = Vec::new();
        let _ = self.expect_eat(&open_token, SyntaxErr::ExpectedToken(open_token.clone()));

        while !self.is_eof() && self.first().token_type != close_token {
            properties.push(self.expect_type());

            if self.first().token_type != close_token {
                let _ = self.expect_eat(&TokenType::Comma, SyntaxErr::ExpectedChar(','));
            }
        }

        let _ = self.expect_eat(&close_token, SyntaxErr::ExpectedToken(close_token.clone()));

        properties
    }

    pub fn parse_delimited(&mut self) -> Token {
        match self.first().token_type {
            TokenType::Close(Bracket::Curly) => self.first().clone(),
            TokenType::EOL => self.eat(),
            _ => {
                if let Some(prev) = self.prev_token.clone() {
                    match prev.token_type {
                        TokenType::Close(Bracket::Curly) | TokenType::EOL => {
                            return prev;
                        }
                        _ => {}
                    }
                }

                self.expect_eat(&TokenType::EOL, SyntaxErr::ExpectedChar(';'))
            }
        }
    }

    pub fn parse_type(&mut self) -> Option<ParserDataType> {
        let t = self.first().clone();

        if self.first().token_type == TokenType::Not {
            let _ = self.eat();

            return Some(ParserDataType::Result(
                Box::new(ParserDataType::Dynamic),
                Box::new(self.expect_type()),
            ));
        }

        let mutability = RefMutability::from(t.token_type.clone());

        let mut typ = if mutability != RefMutability::Value {
            let _ = self.eat();
            Some(ParserDataType::Ref(
                Box::new(self.expect_type()),
                mutability.clone(),
            ))
        } else if t.token_type == TokenType::Comparison(Comparison::Lesser) {
            Some(ParserDataType::Tuple(self.parse_type_list(
                TokenType::Comparison(Comparison::Lesser),
                TokenType::Comparison(Comparison::Greater),
            )))
        } else if t.token_type == TokenType::Func {
            let _ = self.eat();
            let is_async = if self.first().token_type == TokenType::Async {
                let _ = self.eat();
                true
            } else {
                false
            };

            let args = self.parse_type_list(
                TokenType::Open(Bracket::Paren),
                TokenType::Close(Bracket::Paren),
            );
            let mut ret = None;

            if self.first().token_type == TokenType::Arrow {
                let _ = self.eat();
                ret = Some(Box::new(self.expect_type()));
            }

            Some(ParserDataType::Function {
                return_type: ret,
                parameters: args,
                is_async,
            })
        } else if t.token_type == TokenType::List {
            let _ = self.eat();
            let t = if self.first().token_type == TokenType::Comparison(Comparison::Lesser) {
                let _ = self.eat();
                let t = Some(Box::new(self.expect_type()));
                let _ = self.expect_eat(
                    &TokenType::Comparison(Comparison::Greater),
                    SyntaxErr::ExpectedToken(TokenType::Comparison(Comparison::Greater)),
                );
                t
            } else {
                None
            };
            Some(ParserDataType::List(t))
        } else {
            match ParserDataType::from_str(&t.value) {
                Ok(x) => {
                    let mut path = Vec::new();
                    let _ = self.eat();

                    if let ParserDataType::Struct(_) = x {
                        path.push(x);

                        while self.first().token_type == TokenType::Colon {
                            path.push(ParserDataType::from_str(&self.eat().value).unwrap());
                        }

                        if path.len() == 1 {
                            Some(path.remove(0))
                        } else {
                            Some(ParserDataType::Scope(path))
                        }
                    } else {
                        Some(x)
                    }
                }
                Err(_) => None,
            }
        };

        if self.first().token_type == TokenType::Question {
            let _ = self.eat();

            typ = Some(ParserDataType::Option(Box::new(typ?)));
        }

        if self.first().token_type == TokenType::Not {
            let _ = self.eat();
            let t = self.parse_type();

            typ = Some(ParserDataType::Result(Box::new(typ?), Box::new(t?)));
        }

        typ
    }

    pub fn parse_paren_expression(&mut self) -> Node {
        let _ = self.expect_eat(
            &TokenType::Open(Bracket::Paren),
            SyntaxErr::ExpectedOpeningBracket(Bracket::Paren),
        );

        let value = self.parse_statement();
        let span = value.span.clone();

        let value = Node::new(
            NodeType::ParenExpression {
                value: Box::new(value),
            },
            span,
        );

        let _ = self.expect_eat(
            &TokenType::Close(Bracket::Paren),
            SyntaxErr::ExpectedClosingBracket(Bracket::Paren),
        );

        value
    }

    pub fn parse_list_expression(&mut self) -> Node {
        let mut values = Vec::new();
        let open = self.eat();

        while !self.is_eof() && self.first().token_type != TokenType::Close(Bracket::Square) {
            values.push(self.parse_statement());
            if self.first().token_type == TokenType::For {
                let _ = self.eat();
                let loop_type = self.get_loop_type();

                if let LoopType::While(_) = loop_type {
                    self.add_err(SyntaxErr::UnexpectedWhileLoop);
                }

                let mut conditionals = Vec::new();

                while self.first().token_type == TokenType::If {
                    let _ = self.eat();
                    conditionals.push(self.parse_statement());
                }

                let close = self.expect_eat(
                    &TokenType::Close(Bracket::Square),
                    SyntaxErr::ExpectedClosingBracket(Bracket::Square),
                );

                return Node::new(
                    NodeType::IterExpression {
                        map: Box::new(values[0].clone()),
                        loop_type: Box::new(loop_type),
                        conditionals,
                    },
                    Span::new_from_spans(open.span, close.span),
                );
            } else if self.first().token_type != TokenType::Close(Bracket::Square) {
                let _ = self.expect_eat(&TokenType::Comma, SyntaxErr::ExpectedChar(','));
            }
        }

        let close = self.expect_eat(
            &TokenType::Close(Bracket::Square),
            SyntaxErr::ExpectedClosingBracket(Bracket::Square),
        );

        Node::new(
            NodeType::ListLiteral(values),
            Span::new_from_spans(open.span, close.span),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{Token, TokenType, tokenize};

    fn parser_with_tokens(tokens: Vec<Token>) -> Parser {
        Parser { tokens }
    }

    #[test]
    fn test_parse_type_list() {
        let tokens = tokenize(String::from("(int, float)")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let result = parser.parse_type_list(
            TokenType::Open(Bracket::Paren),
            TokenType::Close(Bracket::Paren),
        );
        assert!(result.is_ok());
        let types = result.unwrap();
        assert_eq!(types.len(), 2);
    }

    #[test]
    fn test_parse_key_type_list_object_val_tuple() {
        let tokens = tokenize(String::from("(int, float)")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let result = parser.parse_key_type_list_object_val();
        assert!(result.is_ok());
        match result.unwrap() {
            ObjectType::Tuple(v) => assert_eq!(v.len(), 2),
            _ => panic!("Expected tuple"),
        }
    }

    #[test]
    fn test_parse_key_type_list_object_val_map() {
        let tokens = tokenize(String::from("{a : int, b : float}")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let result = parser.parse_key_type_list_object_val();
        assert!(result.is_ok());
        match result.unwrap() {
            ObjectType::Map(m) => {
                assert_eq!(m.len(), 2);
                assert!(m.contains_key("a"));
                assert!(m.contains_key("b"));
            }
            _ => panic!("Expected map"),
        }
    }
}
