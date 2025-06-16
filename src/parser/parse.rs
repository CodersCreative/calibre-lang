use core::panic;
use std::collections::HashMap;

use crate::{
    ast::{BinaryOperator, NodeType},
    lexer::{Token, TokenType},
    parser::Parser,
    runtime::values,
};

impl Parser {
    fn first(&self) -> &Token {
        self.tokens.first().unwrap()
    }

    fn eat(&mut self) -> Token {
        self.tokens.remove(0)
    }

    fn expect_eat(&mut self, t: &TokenType, msg: &str) -> Token {
        let value = self.eat();
        if &value.token_type != t {
            panic!("Parser Error:\n{:?}, {:?} - Expecting: {:?}", msg, value, t);
        }
        value
    }
    pub fn parse_statement(&mut self) -> NodeType {
        match self.first().token_type {
            TokenType::Var | TokenType::Let => self.parse_variable_declaration(),
            _ => self.parse_expression(),
        }
    }

    pub fn parse_variable_declaration(&mut self) -> NodeType {
        let is_mutable = self.eat().token_type == TokenType::Var;
        let identifier = self
            .expect_eat(&TokenType::Identifier, "Expected variable identifier")
            .value;

        match self.eat().token_type {
            TokenType::Equals => {
                let node = NodeType::VariableDeclaration {
                    is_mutable,
                    identifier,
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
                }
            } // _ => panic!("Expected either variable declaration (EOL) or assignment (=)"),
        }
    }

    pub fn parse_expression(&mut self) -> NodeType {
        self.parse_assignment_expression()
    }

    pub fn parse_assignment_expression(&mut self) -> NodeType {
        let mut left = self.parse_object_expression();

        if self.first().token_type == TokenType::Equals {
            while [TokenType::Equals].contains(&self.first().token_type) {
                let _ = self.eat();
                let right = self.parse_object_expression();
                left = NodeType::AssignmentExpression {
                    identifier: Box::new(left),
                    value: Box::new(right),
                };
            }
        } else {
            while [TokenType::OpenBrackets].contains(&self.first().token_type) {
                let _ = self.eat();
                let right = self.parse_additive_expression();
                self.expect_eat(
                    &TokenType::CloseBrackets,
                    "Expected close brackets for setter.",
                );
                left = NodeType::AssignmentExpression {
                    identifier: Box::new(left),
                    value: Box::new(right),
                };
            }
        }

        left
    }

    pub fn parse_object_expression(&mut self) -> NodeType {
        if self.first().token_type != TokenType::OpenCurly {
            return self.parse_additive_expression();
        }

        let mut properties = HashMap::new();
        let _ = self.eat();

        while !self.is_eof() && self.first().token_type != TokenType::CloseCurly {
            let key = self
                .expect_eat(&TokenType::Identifier, "Expected object literal key.")
                .value;

            if [TokenType::Comma, TokenType::CloseCurly].contains(&self.first().token_type) {
                if self.first().token_type != TokenType::CloseCurly {
                    let _ = self.eat();
                }

                properties.insert(key, None);
                continue;
            }

            let _ = self.expect_eat(&TokenType::Colon, "Object missing colon after identifier.");

            properties.insert(key, Some(self.parse_expression()));

            if self.first().token_type != TokenType::CloseCurly {
                let _ = self.expect_eat(&TokenType::Comma, "Object missing comma after property");
            }
        }

        let _ = self.expect_eat(&TokenType::CloseCurly, "Object missing closing brace.");

        NodeType::MapLiteral(properties)
    }

    pub fn parse_power_expression(&mut self) -> NodeType {
        let mut left = self.parse_primary_expression();

        while ["^"].contains(&self.first().value.trim()) {
            let operator = self.eat().value.trim().chars().nth(0).unwrap();
            let right = self.parse_primary_expression();

            left = NodeType::BinaryExpression {
                left: Box::new(left),
                right: Box::new(right),
                operator: BinaryOperator::from_symbol(operator).unwrap(),
            };
        }

        left
    }

    pub fn parse_multiplicative_expression(&mut self) -> NodeType {
        let mut left = self.parse_power_expression();

        while ["/", "*", "%"].contains(&self.first().value.trim()) {
            let operator = self.eat().value.trim().chars().nth(0).unwrap();
            let right = self.parse_power_expression();

            left = NodeType::BinaryExpression {
                left: Box::new(left),
                right: Box::new(right),
                operator: BinaryOperator::from_symbol(operator).unwrap(),
            };
        }

        left
    }

    pub fn parse_additive_expression(&mut self) -> NodeType {
        let mut left = self.parse_multiplicative_expression();

        while ["+", "-"].contains(&self.first().value.trim()) {
            let operator = self.eat().value.trim().chars().nth(0).unwrap();
            let right = self.parse_multiplicative_expression();

            left = NodeType::BinaryExpression {
                left: Box::new(left),
                right: Box::new(right),
                operator: BinaryOperator::from_symbol(operator).unwrap(),
            };
        }

        left
    }

    pub fn parse_primary_expression(&mut self) -> NodeType {
        match self.first().token_type {
            TokenType::Identifier => NodeType::Identifier(self.eat().value),
            TokenType::Float => NodeType::FloatLiteral(self.eat().value.trim().parse().unwrap()),
            TokenType::Integer => {
                NodeType::IntegerLiteral(self.eat().value.trim().parse().unwrap())
            }
            TokenType::OpenBrackets => {
                self.eat();
                let value = self.parse_expression();
                self.expect_eat(
                    &TokenType::CloseBrackets,
                    "Unexpected token found inside parenthesis.",
                );
                value
            }
            _ => panic!("Unexpected Token : {:?}.", self.first()),
        }
    }
}
