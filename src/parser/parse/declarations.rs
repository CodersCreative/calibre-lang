use crate::parser::Parser;
use std::{
    collections::{self, HashMap},
    ops::Not,
    str::FromStr,
};

use crate::{
    ast::{binary::BinaryOperator, NodeType},
    lexer::{Token, TokenType},
    runtime::values::{self, RuntimeType},
};

impl Parser {
    pub fn parse_variable_declaration(&mut self) -> NodeType {
        let is_mutable = self.eat().token_type == TokenType::Var;
        let identifier = self
            .expect_eat(&TokenType::Identifier, "Expected variable identifier")
            .value;

        let data_type = match self.first().token_type {
            TokenType::Colon => {
                let _ = self.eat();
                self.parse_type()
            }
            _ => None,
        };

        match self.eat().token_type {
            TokenType::Equals => {
                let node = NodeType::VariableDeclaration {
                    is_mutable,
                    identifier,
                    data_type,
                    value: Some(Box::new(self.parse_expression())),
                };

                node
            }
            _ => {
                if !is_mutable {
                    panic!("Cannot declare null constant")
                }

                NodeType::VariableDeclaration {
                    is_mutable,
                    identifier,
                    value: None,
                    data_type,
                }
            } // _ => panic!("Expected either variable declaration (EOL) or assignment (=)"),
        }
    }
    pub fn parse_struct_declaration(&mut self) -> NodeType {
        let _ = self.expect_eat(&TokenType::Struct, "Expected struct keyword");

        let identifier = self
            .expect_eat(&TokenType::Identifier, "Expected struct name")
            .value;

        NodeType::StructDeclaration {
            identifier,
            properties: self.parse_key_type_list(TokenType::OpenCurly, TokenType::CloseCurly),
        }
    }
    pub fn parse_function_declaration(&mut self) -> NodeType {
        let _ = self.eat();
        let identifier = self
            .expect_eat(&TokenType::Identifier, "Expected function identifier")
            .value;

        let parameters =
            self.parse_key_type_list_ordered(TokenType::OpenBrackets, TokenType::CloseBrackets);

        let is_async = self.first().token_type == TokenType::Async;

        if is_async {
            let _ = self.eat();
        }

        let return_type = if self.first().token_type == TokenType::OpenCurly {
            None
        } else {
            let _ = self.expect_eat(&TokenType::Arrow, "Expected arrow to show return type");
            // let t = self.expect_eat(&TokenType::Identifier, "Expected return type");
            //
            self.parse_type()
        };

        let _ = self.expect_eat(&TokenType::OpenCurly, "Expected opening brackets");

        let mut body: Vec<NodeType> = Vec::new();

        while ![TokenType::EOF, TokenType::CloseCurly].contains(&self.first().token_type) {
            body.push(self.parse_statement());
        }

        let _ = self.expect_eat(&TokenType::CloseCurly, "Expected closing brackets");

        NodeType::FunctionDeclaration {
            identifier,
            parameters,
            body: Box::new(body),
            return_type,
            is_async,
        }
    }
}
