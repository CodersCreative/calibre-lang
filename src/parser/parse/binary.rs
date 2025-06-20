use crate::parser::{Parser, ParserError};

use crate::{
    ast::{NodeType, binary::BinaryOperator},
    lexer::TokenType,
};

impl Parser {
    pub fn parse_range_expression(&mut self) -> Result<NodeType, ParserError> {
        let mut left = self.parse_additive_expression()?;

        if let TokenType::Range = self.first().token_type.clone() {
            let _ = self.eat();

            let inclusive = if self.first().token_type == TokenType::Equals {
                let _ = self.eat();
                true
            } else {
                false
            };

            let right = self.parse_additive_expression()?;

            left = NodeType::RangeDeclaration {
                from: Box::new(left),
                to: Box::new(right),
                inclusive,
            }
        }

        Ok(left)
    }

    pub fn parse_additive_expression(&mut self) -> Result<NodeType, ParserError> {
        let mut left = self.parse_multiplicative_expression()?;

        if let TokenType::BinaryOperator(op) = self.first().token_type.clone() {
            if [BinaryOperator::Add, BinaryOperator::Subtract].contains(&op) {
                let _ = self.eat();
                let right = self.parse_multiplicative_expression()?;

                left = NodeType::BinaryExpression {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator: op,
                };
            }
        }

        Ok(left)
    }

    pub fn parse_multiplicative_expression(&mut self) -> Result<NodeType, ParserError> {
        let mut left = self.parse_power_expression()?;
        if let TokenType::BinaryOperator(op) = self.first().token_type.clone() {
            if [
                BinaryOperator::Multiply,
                BinaryOperator::Divide,
                BinaryOperator::Modulus,
            ]
            .contains(&op)
            {
                let _ = self.eat();
                let right = self.parse_power_expression()?;

                left = NodeType::BinaryExpression {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator: op,
                };
            }
        }

        Ok(left)
    }

    pub fn parse_power_expression(&mut self) -> Result<NodeType, ParserError> {
        let mut left = self.parse_call_member_expression()?;

        if let TokenType::BinaryOperator(op) = self.first().token_type.clone() {
            if [BinaryOperator::Power].contains(&op) {
                let _ = self.eat();
                let right = self.parse_call_member_expression()?;

                left = NodeType::BinaryExpression {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator: op,
                };
            }
        }

        Ok(left)
    }

    pub fn parse_comparison_expression(&mut self) -> Result<NodeType, ParserError> {
        let mut left = self.parse_range_expression()?;
        if let TokenType::Comparison(comparison) = self.first().token_type.clone() {
            let _ = self.eat();
            let right = self.parse_range_expression()?;

            left = NodeType::ComparisonExpression {
                left: Box::new(left),
                right: Box::new(right),
                operator: comparison,
            };
        }

        Ok(left)
    }

    pub fn parse_boolean_expression(&mut self) -> Result<NodeType, ParserError> {
        let mut left = self.parse_comparison_expression()?;
        if let TokenType::Boolean(comparison) = self.first().token_type.clone() {
            let _ = self.eat();
            let right = self.parse_comparison_expression()?;

            left = NodeType::BooleanExpression {
                left: Box::new(left),
                right: Box::new(right),
                operator: comparison,
            };
        }

        Ok(left)
    }
}
