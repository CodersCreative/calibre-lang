use crate::parser::{Parser, ParserError};

use crate::{
    ast::{NodeType, binary::BinaryOperator},
    lexer::TokenType,
};

impl Parser {
    pub fn parse_in_expression(&mut self) -> Result<NodeType, ParserError> {
        let mut left = self.parse_range_expression()?;

        if let TokenType::In = self.first().token_type.clone() {
            let _ = self.eat();

            let right = self.parse_range_expression()?;

            left = NodeType::InDeclaration {
                identifier: Box::new(left),
                expression: Box::new(right),
            }
        }

        Ok(left)
    }

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

        while let TokenType::BinaryOperator(op) = self.first().token_type.clone() {
            if [BinaryOperator::Add, BinaryOperator::Sub].contains(&op) {
                let _ = self.eat();
                let right = self.parse_multiplicative_expression()?;

                left = NodeType::BinaryExpression {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator: op,
                };
            } else {
                break;
            }
        }

        Ok(left)
    }

    pub fn parse_multiplicative_expression(&mut self) -> Result<NodeType, ParserError> {
        let mut left = self.parse_power_expression()?;
        while let TokenType::BinaryOperator(op) = self.first().token_type.clone() {
            if [
                BinaryOperator::Mul,
                BinaryOperator::Div,
                BinaryOperator::Mod,
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
            } else {
                break;
            }
        }

        Ok(left)
    }

    pub fn parse_power_expression(&mut self) -> Result<NodeType, ParserError> {
        let mut left = self.parse_call_member_expression()?;

        while let TokenType::BinaryOperator(BinaryOperator::Pow) = self.first().token_type.clone()
        {
            let _ = self.eat();
            let right = self.parse_call_member_expression()?;

            left = NodeType::BinaryExpression {
                left: Box::new(left),
                right: Box::new(right),
                operator: BinaryOperator::Pow,
            };
        }

        Ok(left)
    }

    pub fn parse_comparison_expression(&mut self) -> Result<NodeType, ParserError> {
        let mut left = self.parse_in_expression()?;
        while let TokenType::Comparison(comparison) = self.first().token_type.clone() {
            let _ = self.eat();
            let right = self.parse_in_expression()?;

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
        while let TokenType::Boolean(comparison) = self.first().token_type.clone() {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::comparison::{BooleanOperation, Comparison};
    use crate::lexer::{Token, TokenType, tokenize};
    use crate::ast::{NodeType, binary::BinaryOperator};
    use crate::parser::Parser;

    fn parser_with_tokens(tokens: Vec<Token>) -> Parser {
        Parser { tokens }
    }

    #[test]
    fn test_parse_range_expression_exclusive() {
        let tokens = tokenize(String::from("1..10")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_range_expression().unwrap();
        match node {
            NodeType::RangeDeclaration { inclusive, .. } => assert!(!inclusive),
            _ => panic!("Expected RangeDeclaration"),
        }
    }

    #[test]
    fn test_parse_range_expression_inclusive() {
        let tokens = tokenize(String::from("1..=10")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_range_expression().unwrap();
        match node {
            NodeType::RangeDeclaration { inclusive, .. } => assert!(inclusive),
            _ => panic!("Expected RangeDeclaration"),
        }
    }

    #[test]
    fn test_parse_additive_expression() {
        let tokens = tokenize(String::from("1 + 2 - 3")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_additive_expression().unwrap();
        match node {
            NodeType::BinaryExpression { operator, .. } => {
                assert_eq!(operator, BinaryOperator::Sub);
            }
            _ => panic!("Expected BinaryExpression"),
        }
    }

    #[test]
    fn test_parse_multiplicative_expression() {
        let tokens = tokenize(String::from("2 * 3 / 4")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_multiplicative_expression().unwrap();
        match node {
            NodeType::BinaryExpression { operator, .. } => {
                assert_eq!(operator, BinaryOperator::Div);
            }
            _ => panic!("Expected BinaryExpression"),
        }
    }

    #[test]
    fn test_parse_power_expression() {
        let tokens = tokenize(String::from("2 ^ 3")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_power_expression().unwrap();
        match node {
            NodeType::BinaryExpression { operator, .. } => {
                assert_eq!(operator, BinaryOperator::Pow);
            }
            _ => panic!("Expected BinaryExpression"),
        }
    }

    #[test]
    fn test_parse_comparison_expression() {
        let tokens = tokenize(String::from("1 < 2 > 3")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_comparison_expression().unwrap();
        match node {
            NodeType::ComparisonExpression { operator, .. } => {
                assert_eq!(operator, Comparison::Lesser);
            }
            _ => panic!("Expected ComparisonExpression"),
        }
    }

    #[test]
    fn test_parse_boolean_expression() {
        let tokens = tokenize(String::from("true && false || !true")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_boolean_expression().unwrap();
        match node {
            NodeType::BooleanExpression { operator, .. } => {
                assert_eq!(operator, BooleanOperation::Or);
            }
            _ => panic!("Expected BooleanExpression"),
        }
    }
}
