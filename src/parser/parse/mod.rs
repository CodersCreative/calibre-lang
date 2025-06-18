pub mod expressions;
pub mod binary;
pub mod declarations;
pub mod functions;

use std::{
    collections::{self, HashMap},
    ops::Not,
    str::FromStr,
};

use crate::{
    ast::{binary::BinaryOperator, NodeType},
    lexer::{Token, TokenType},
    parser::Parser,
    runtime::values::{self, RuntimeType},
};

impl Parser {
    fn first(&self) -> &Token {
        self.tokens.first().unwrap()
    }

    fn nth(&self, i: usize) -> &Token {
        self.tokens.get(i).unwrap()
    }

    fn eat(&mut self) -> Token {
        self.tokens.remove(0)
    }

    fn expect_eat(&mut self, t: &TokenType, msg: &str) -> Token {
        let value = self.eat();
        if &value.token_type != t {
            panic!(
                "Parser Error:\n{:?}, {:?} \nExpecting: {:?} \nNext {:?}",
                msg,
                value,
                t,
                self.first()
            );
        }
        value
    }

    fn parse_key_type_list_ordered(
        &mut self,
        open_token: TokenType,
        close_token: TokenType,
    ) -> Vec<(String, RuntimeType)> {
        let mut properties = Vec::new();
        let _ = self.expect_eat(&open_token, "Expected opening brackets");

        while !self.is_eof() && self.first().token_type != close_token {
            let key = self
                .expect_eat(&TokenType::Identifier, "Expected object literal key.")
                .value;

            let _ = self.expect_eat(&TokenType::Colon, "Object missing colon after identifier.");

            properties.push((key, self.parse_type().expect("Expected data type.")));

            if self.first().token_type != close_token {
                let _ = self.expect_eat(&TokenType::Comma, "Object missing comma after property");
            }
        }

        let _ = self.expect_eat(&close_token, "Object missing closing brace.");

        properties
    }

    fn parse_key_type_list(
        &mut self,
        open_token: TokenType,
        close_token: TokenType,
    ) -> HashMap<String, RuntimeType> {
        let mut properties = HashMap::new();
        let _ = self.expect_eat(&open_token, "Expected opening brackets");

        while !self.is_eof() && self.first().token_type != close_token {
            let key = self
                .expect_eat(&TokenType::Identifier, "Expected object literal key.")
                .value;

            let _ = self.expect_eat(&TokenType::Colon, "Object missing colon after identifier.");

            properties.insert(key, self.parse_type().expect("Expected data type."));

            if self.first().token_type != close_token {
                let _ = self.expect_eat(&TokenType::Comma, "Object missing comma after property");
            }
        }

        let _ = self.expect_eat(&close_token, "Object missing closing brace.");

        properties
    }

    fn parse_type_list(
        &mut self,
        open_token: TokenType,
        close_token: TokenType,
    ) -> Vec<RuntimeType> {
        let mut properties = Vec::new();
        let _ = self.expect_eat(&open_token, "Expected opening brackets");

        while !self.is_eof() && self.first().token_type != close_token {
            properties.push(
                self.parse_type()
                    .expect("Expected data type after identifier"),
            );
            // let value = self
            //     .expect_eat(&TokenType::Identifier, "Expected data type.")
            //     .value;
            //
            // properties.push(RuntimeType::from_str(&value).unwrap());

            if self.first().token_type != close_token {
                let _ = self.expect_eat(&TokenType::Comma, "Object missing comma after property");
            }
        }

        let _ = self.expect_eat(&close_token, "Object missing closing brace.");

        properties
    }

    pub fn parse_type(&mut self) -> Option<RuntimeType> {
        let t = self.eat();

        if t.token_type == TokenType::Func || t.token_type == TokenType::Async {
            let is_async = if t.token_type == TokenType::Async {
                let _ = self.expect_eat(&TokenType::Func, "Expected function keyword after async");
                true
            } else {
                false
            };

            let args = self.parse_type_list(TokenType::OpenBrackets, TokenType::CloseBrackets);
            let mut ret = None;

            if self.first().token_type == TokenType::Arrow {
                let _ = self.eat();
                ret = Some(Box::new(self.parse_type().unwrap()));
            }

            Some(RuntimeType::Function {
                return_type: ret,
                parameters: args,
                is_async,
            })
        } else if t.token_type == TokenType::List {
            let t = if self.first().token_type == TokenType::OpenBrackets {
                let _ = self.eat();
                let t = Some(Box::new(self.parse_type().expect("Expected data type")));
                let _ = self.expect_eat(&TokenType::CloseBrackets, "Expected closing brackets.");
                t
            } else {
                None
            };
            Some(RuntimeType::List(t))
        } else {
            match RuntimeType::from_str(&t.value) {
                Ok(x) => Some(x),
                Err(_) => None,
            }
        }
    }


    pub fn parse_expression(&mut self) -> NodeType {
        self.parse_assignment_expression()
    }


    pub fn parse_list_expression(&mut self) -> NodeType {
        if self.first().token_type != TokenType::OpenSquare {
            return self.parse_object_expression();
        }

        let mut values = Vec::new();
        let _ = self.eat();

        while !self.is_eof() && self.first().token_type != TokenType::CloseSquare {
            values.push(self.parse_expression());

            if self.first().token_type != TokenType::CloseSquare {
                let _ = self.expect_eat(&TokenType::Comma, "Object missing comma after property");
            }
        }

        let _ = self.expect_eat(&TokenType::CloseSquare, "Object missing closing brace.");

        NodeType::ListLiteral(Box::new(values))
    }
}
