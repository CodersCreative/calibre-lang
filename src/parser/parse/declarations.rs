use crate::{
    ast::{RefMutability, comparison},
    parser::Parser,
};
use core::panic;
use std::{
    collections::{self, HashMap},
    ops::Not,
    str::FromStr,
};

use crate::{
    ast::{NodeType, binary::BinaryOperator},
    lexer::{Token, TokenType},
    runtime::values::{self, RuntimeType},
};

impl Parser {
    pub fn parse_statement(&mut self) -> NodeType {
        match self.first().token_type {
            TokenType::Let => self.parse_variable_declaration(),
            TokenType::Struct => self.parse_struct_declaration(),
            TokenType::Func => self.parse_function_declaration(false),
            TokenType::If => self.parse_if_statement(),
            TokenType::Trait => self.parse_if_statement(),
            TokenType::Impl => self.parse_impl_declaration(),
            _ => self.parse_expression(),
        }
    }
    pub fn parse_variable_declaration(&mut self) -> NodeType {
        let _ = self.expect_eat(&TokenType::Let, "Expected let keyword");
        let is_mutable = self.first().token_type == TokenType::Mut;
        if is_mutable {
            let _ = self.eat();
        }
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

    pub fn parse_impl_declaration(&mut self) -> NodeType {
        let _ = self.expect_eat(&TokenType::Impl, "Expected impl keyword");

        let identifier = self
            .expect_eat(&TokenType::Identifier, "Expected struct name")
            .value;

        let mut functions = Vec::new();

        let _ = self.expect_eat(&TokenType::OpenCurly, "Expected closing brackets");

        while self.first().token_type == TokenType::Func {
            let func = self.parse_function_declaration(true);

            match func {
                NodeType::FunctionDeclaration {
                    identifier: name,
                    mut parameters,
                    body,
                    return_type,
                    is_async,
                } => {
                    let mut depends = false;
                    if &parameters[0].0 == "self" {
                        parameters[0].1 = RuntimeType::Struct(identifier.clone());
                        depends = true;
                    }

                    functions.push((
                        NodeType::FunctionDeclaration {
                            identifier: name,
                            parameters,
                            body,
                            return_type,
                            is_async,
                        },
                        depends,
                    ));
                }
                _ => panic!("Impl blocks only accept functions"),
            }
        }

        let _ = self.expect_eat(&TokenType::CloseCurly, "Expected closing brackets");

        NodeType::ImplDeclaration {
            identifier,
            functions,
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

    pub fn parse_function_declaration(&mut self, self_valid: bool) -> NodeType {
        let _ = self.eat();
        let identifier = self
            .expect_eat(&TokenType::Identifier, "Expected function identifier")
            .value;

        let mut parameters = Vec::new();

        if self.tokens[1].value == "self" || self.tokens[2].value == "self" {
            if !self_valid {
                panic!("Cannot use self keyword outside of a trait or function");
            }
            let _ = self.expect_eat(&TokenType::OpenBrackets, "Expected opening brackets");

            let ref_mutability = RefMutability::from(self.first().token_type.clone());

            if ref_mutability != RefMutability::Value {
                let _ = self.eat();
            }

            parameters.push((self.eat().value, RuntimeType::Str, ref_mutability));

            if self.first().token_type == TokenType::Comma {
                let mut other = self.parse_key_type_list_ordered_with_ref(
                    TokenType::Comma,
                    TokenType::CloseBrackets,
                );
                parameters.append(&mut other);
            } else {
                let _ = self.expect_eat(&TokenType::CloseBrackets, "Expected closing brackets.");
            }
        } else {
            parameters = self.parse_key_type_list_ordered_with_ref(
                TokenType::OpenBrackets,
                TokenType::CloseBrackets,
            );
        }

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

            if self.first().token_type == TokenType::Else {
                let _ = self.eat();
                if self.first().token_type == TokenType::OpenCurly {
                    bodies.push(Box::new(self.parse_block()));
                    break;
                }
            } else {
                break;
            }
        }
        NodeType::IfStatement {
            comparisons: Box::new(comparisons),
            bodies,
        }
    }
}
