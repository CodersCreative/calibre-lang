use crate::parser::Parser;
use std::collections::{HashMap};

use crate::{
    ast::{NodeType, binary::BinaryOperator},
    lexer::TokenType,
};

impl Parser {
    pub fn parse_primary_expression(&mut self) -> NodeType {
        match self.first().token_type {
            TokenType::Identifier => NodeType::Identifier(self.eat().value),
            TokenType::Float => NodeType::FloatLiteral(self.eat().value.trim().parse().unwrap()),
            TokenType::Integer => {
                NodeType::IntegerLiteral(self.eat().value.trim().parse().unwrap())
            }
            TokenType::String => {
                let val = self.eat().value;
                if val.len() == 1 {
                    NodeType::CharLiteral(val.chars().nth(0).unwrap())
                } else {
                    NodeType::StringLiteral(val.to_string())
                }
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
    pub fn parse_object_expression(&mut self) -> NodeType {
        if self.first().token_type != TokenType::OpenCurly {
            return self.parse_boolean_expression();
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
    pub fn parse_assignment_expression(&mut self) -> NodeType {
        let mut left = self.parse_list_expression();

        if let TokenType::UnaryAssign(op) = self.first().token_type.clone() {
            let operator = self.eat();

            if [
                BinaryOperator::Power,
                BinaryOperator::Divide,
                BinaryOperator::Multiply,
            ]
            .contains(&op)
            {
                panic!(
                    "This notation cannot be used with operator {:?} as it would not alter the value.",
                    operator.value.chars().nth(0).unwrap()
                );
            }

            left = NodeType::AssignmentExpression {
                identifier: Box::new(left.clone()),
                value: Box::new(NodeType::BinaryExpression {
                    left: Box::new(left),
                    right: Box::new(NodeType::IntegerLiteral(1)),
                    operator: op,
                }),
            };
        } else if let TokenType::BinaryAssign(op) = self.first().token_type.clone() {
            let operator = self.eat();
            left = NodeType::AssignmentExpression {
                identifier: Box::new(left.clone()),
                value: Box::new(NodeType::BinaryExpression {
                    left: Box::new(left),
                    right: Box::new(self.parse_list_expression()),
                    operator: op,
                }),
            };
        } else if [TokenType::Equals].contains(&self.first().token_type) {
            let _ = self.eat();
            let right = self.parse_list_expression();
            left = NodeType::AssignmentExpression {
                identifier: Box::new(left),
                value: Box::new(right),
            };
        }

        left
    }
}
