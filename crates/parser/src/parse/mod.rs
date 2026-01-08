pub mod binary;
pub mod declarations;
pub mod expressions;
pub mod functions;
pub mod r#type;

use crate::{
    Parser, ParserError, SyntaxErr,
    ast::{
        LoopType, Node, NodeType, ObjectType, ParserDataType, ParserInnerType, ParserText,
        RefMutability, comparison::Comparison,
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
        let prev = self.prev_token.clone().unwrap();
        let mut input = prev.value.clone();

        (0..4).into_iter().for_each(|x| {
            let val = self
                .tokens
                .get(x)
                .map(|x| x.value.clone())
                .unwrap_or(String::new());
            input.push_str(&format!(" {}", val));
        });

        self.errors.push(ParserError::Syntax {
            input,
            err,
            span: prev.span,
            token: Some((0, prev.value.len() - 1)),
        })
    }

    fn expect_eat(&mut self, t: &TokenType, err: SyntaxErr) -> Token {
        let token = self.eat();
        if &token.token_type != t {
            self.add_err(err);
            if &self.first().token_type == t {
                self.eat()
            } else {
                token
            }
        } else {
            token
        }
    }

    fn expect_type(&mut self) -> ParserDataType {
        if let Some(x) = self.parse_type() {
            x
        } else {
            self.add_err(SyntaxErr::ExpectedType);
            ParserDataType::new(ParserInnerType::Dynamic, self.first().span.clone())
        }
    }

    fn parse_key_type_list_ordered_with_ref(
        &mut self,
        open_token: TokenType,
        close_token: TokenType,
    ) -> Vec<(ParserText, ParserDataType, Option<Node>)> {
        let mut properties = Vec::new();
        let mut defaulted = false;
        let _ = self.expect_eat(&open_token, SyntaxErr::ExpectedToken(open_token.clone()));

        while !self.is_eof() && self.first().token_type != close_token {
            let mut keys = Vec::new();

            while self.first().token_type == TokenType::Identifier {
                keys.push(
                    self.expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedKey)
                        .into(),
                );
            }

            let typ = if self.first().token_type == TokenType::Colon {
                let _ = self.expect_eat(&TokenType::Colon, SyntaxErr::ExpectedChar(':'));

                self.expect_type()
            } else {
                ParserDataType::new(ParserInnerType::Dynamic, self.first().span.clone())
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
            let not = self.eat();
            let typ = self.expect_type();
            return Some(ParserDataType::new(
                ParserInnerType::Result(
                    Box::new(ParserDataType::new(ParserInnerType::Dynamic, not.span)),
                    Box::new(typ.clone()),
                ),
                Span::new_from_spans(not.span, typ.span),
            ));
        }

        let mutability = RefMutability::from(t.token_type.clone());

        let mut typ = if mutability != RefMutability::Value {
            let mutability_token = self.eat();
            let typ = self.expect_type();
            Some(ParserDataType::new(
                ParserInnerType::Ref(Box::new(typ.clone()), mutability.clone()),
                Span::new_from_spans(mutability_token.span, typ.span),
            ))
        } else if t.token_type == TokenType::Comparison(Comparison::Lesser) {
            let types = self.parse_type_list(
                TokenType::Comparison(Comparison::Lesser),
                TokenType::Comparison(Comparison::Greater),
            );

            let span = if types.is_empty() {
                self.first().span.clone()
            } else {
                Span::new_from_spans(
                    types.first().unwrap().span.clone(),
                    types.last().unwrap().span.clone(),
                )
            };

            Some(ParserDataType::new(ParserInnerType::Tuple(types), span))
        } else if t.token_type == TokenType::Func {
            let open = self.eat();
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

            let close = if let Some(ret) = &ret {
                ret.span.clone()
            } else if let Some(arg) = args.last() {
                arg.span.clone()
            } else {
                open.span.clone()
            };

            Some(ParserDataType::new(
                ParserInnerType::Function {
                    return_type: ret,
                    parameters: args,
                    is_async,
                },
                Span::new_from_spans(open.span, close),
            ))
        } else if t.token_type == TokenType::List {
            let open = self.eat();
            let mut close = open.clone();
            let t = if self.first().token_type == TokenType::Comparison(Comparison::Lesser) {
                let _ = self.eat();
                let t = Some(Box::new(self.expect_type()));
                close = self.expect_eat(
                    &TokenType::Comparison(Comparison::Greater),
                    SyntaxErr::ExpectedToken(TokenType::Comparison(Comparison::Greater)),
                );
                t
            } else {
                None
            };

            Some(ParserDataType::new(
                ParserInnerType::List(t),
                Span::new_from_spans(open.span, close.span),
            ))
        } else {
            match ParserInnerType::from_str(&t.value) {
                Ok(x) => {
                    let mut path = Vec::new();
                    let open = self.eat().span;
                    let mut close = open.clone();
                    let x = ParserDataType::new(x, open.clone());

                    if let ParserInnerType::Struct(_) = x.data_type {
                        path.push(x);

                        while self.first().token_type == TokenType::Colon {
                            let value = self.eat();
                            close = value.span;

                            path.push(ParserDataType::new(
                                ParserInnerType::from_str(&value.value).unwrap(),
                                value.span,
                            ));
                        }

                        if path.len() == 1 {
                            Some(path.remove(0))
                        } else {
                            Some(ParserDataType::new(
                                ParserInnerType::Scope(path),
                                Span::new_from_spans(open, close),
                            ))
                        }
                    } else {
                        Some(x)
                    }
                }
                Err(_) => None,
            }
        };

        if self.first().token_type == TokenType::Question {
            let close = self.eat();

            typ = Some(ParserDataType::new(
                ParserInnerType::Option(Box::new(typ.clone()?)),
                Span::new_from_spans(typ?.span, close.span),
            ));
        }

        if self.first().token_type == TokenType::Not {
            let _ = self.eat();
            let t = self.parse_type()?;

            typ = Some(ParserDataType::new(
                ParserInnerType::Result(Box::new(typ.clone()?), Box::new(t.clone())),
                Span::new_from_spans(typ?.span, t.span),
            ));
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
