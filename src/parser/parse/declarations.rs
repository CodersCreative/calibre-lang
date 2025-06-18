use crate::{ast::comparison, parser::Parser};
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
    pub fn parse_statement(&mut self) -> NodeType {
        match self.first().token_type {
            TokenType::Var | TokenType::Let => self.parse_variable_declaration(),
            TokenType::Struct => self.parse_struct_declaration(),
            TokenType::Func => self.parse_function_declaration(),
            TokenType::If => self.parse_if_statement(),
            _ => self.parse_expression(),
        }
    }
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

        NodeType::FunctionDeclaration {
            identifier,
            parameters,
            body: Box::new(self.parse_block()),
            return_type,
            is_async,
        }
    }

    pub fn parse_block(&mut self) -> Vec<NodeType> {
        let _ = self.expect_eat(&TokenType::OpenCurly, "Expected opening brackets");

        let mut body: Vec<NodeType> = Vec::new();

        while ![TokenType::EOF, TokenType::CloseCurly].contains(&self.first().token_type) {
            body.push(self.parse_statement());
        }

        let _ = self.expect_eat(&TokenType::CloseCurly, "Expected closing brackets");
        body
    }

    pub fn parse_if_statement(&mut self) -> NodeType {
        let mut comparisons = Vec::new();
        let mut bodies = Vec::new();
        
        while self.first().token_type == TokenType::If {
            let _ = self.eat();
            comparisons.push(self.parse_expression());
            bodies.push(Box::new(self.parse_block()));

            if self.first().token_type == TokenType::Else{
                let _ = self.eat();
                if self.first().token_type == TokenType::OpenCurly{
                    bodies.push(Box::new(self.parse_block()));
                    break;
                }

            }else {
                break;
            }

        }
        NodeType::IfStatement { comparisons : Box::new(comparisons), bodies}
    }
}
