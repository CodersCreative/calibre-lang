use crate::{Parser, ParserError, SyntaxErr};
use crate::{
    ast::{NodeType, binary::BinaryOperator},
    lexer::TokenType,
};

impl Parser {
    pub fn parse_pipe_expression(&mut self) -> Result<NodeType, ParserError> {
        let mut left = self.parse_object_expression()?;

        while let TokenType::Pipe = self.first().token_type.clone() {
            let _ = self.eat();
            left = NodeType::PipeExpression { left : Box::new(left), right : Box::new(self.parse_statement()?) };
        }
        Ok(left)
    }

    pub fn parse_as_expression(&mut self) -> Result<NodeType, ParserError> {
        let mut left = self.parse_call_member_expression()?;

        while let TokenType::As = self.first().token_type.clone() {
            let _ = self.eat();

            left = NodeType::AsExpression {
                value: Box::new(left),
                typ: self.parse_type()?.unwrap(),
            };
        }

        Ok(left)
    }

    pub fn parse_is_expression(&mut self) -> Result<NodeType, ParserError> {
        let mut left = self.parse_bitwise_expression()?;

        while let TokenType::Is = self.first().token_type.clone() {
            let _ = self.eat();

            left = NodeType::IsDeclaration {
                value: Box::new(left),
                data_type: self.parse_type()?.unwrap(),
            }
        }

        Ok(left)
    }

    pub fn parse_in_expression(&mut self) -> Result<NodeType, ParserError> {
        let mut left = self.parse_is_expression()?;

        while let TokenType::In = self.first().token_type.clone() {
            let _ = self.eat();

            left = NodeType::InDeclaration {
                identifier: Box::new(left),
                expression: Box::new(self.parse_statement()?),
            }
        }

        Ok(left)
    }

    pub fn parse_range_expression(&mut self) -> Result<NodeType, ParserError> {
        let mut left = self.parse_boolean_expression()?;

        if let TokenType::Range = self.first().token_type.clone() {
            let _ = self.eat();

            let inclusive = if self.first().token_type == TokenType::Equals {
                let _ = self.eat();
                true
            } else {
                false
            };

            left = NodeType::RangeDeclaration {
                from: Box::new(left),
                to: Box::new(self.parse_boolean_expression()?),
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

                left = NodeType::BinaryExpression {
                    left: Box::new(left),
                    right: Box::new(self.parse_multiplicative_expression()?),
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

                left = NodeType::BinaryExpression {
                    left: Box::new(left),
                    right: Box::new(self.parse_power_expression()?),
                    operator: op,
                };
            } else {
                break;
            }
        }

        Ok(left)
    }

    pub fn parse_bitwise_expression(&mut self) -> Result<NodeType, ParserError> {
        let mut left = self.parse_shift_expression()?;
        while [
            TokenType::Ref,
            TokenType::Or,
            TokenType::BinaryOperator(BinaryOperator::BitXor),
        ]
        .contains(&self.first().token_type)
        {
            let op = self.eat();

            left = NodeType::BinaryExpression {
                left: Box::new(left),
                right: Box::new(self.parse_shift_expression()?),
                operator: match op.token_type {
                    TokenType::Ref => BinaryOperator::BitAnd,
                    TokenType::Or => BinaryOperator::BitOr,
                    TokenType::BinaryOperator(x) => x,
                    _ => return Err(self.get_err(SyntaxErr::UnexpectedToken)),
                },
            };
        }

        Ok(left)
    }
    pub fn parse_shift_expression(&mut self) -> Result<NodeType, ParserError> {
        let mut left = self.parse_additive_expression()?;
        while let TokenType::BinaryOperator(op) = self.first().token_type.clone() {
            if [BinaryOperator::Shl, BinaryOperator::Shr].contains(&op) {
                let _ = self.eat();

                left = NodeType::BinaryExpression {
                    left: Box::new(left),
                    right: Box::new(self.parse_additive_expression()?),
                    operator: op,
                };
            } else {
                break;
            }
        }

        Ok(left)
    }

    pub fn parse_power_expression(&mut self) -> Result<NodeType, ParserError> {
        let mut left = self.parse_as_expression()?;

        while let TokenType::BinaryOperator(BinaryOperator::Pow) = self.first().token_type.clone() {
            let _ = self.eat();

            left = NodeType::BinaryExpression {
                left: Box::new(left),
                right: Box::new(self.parse_as_expression()?),
                operator: BinaryOperator::Pow,
            };
        }

        Ok(left)
    }

    pub fn parse_comparison_expression(&mut self) -> Result<NodeType, ParserError> {
        let mut left = self.parse_in_expression()?;
        while let TokenType::Comparison(comparison) = self.first().token_type.clone() {
            let _ = self.eat();

            left = NodeType::ComparisonExpression {
                left: Box::new(left),
                right: Box::new(self.parse_in_expression()?),
                operator: comparison,
            };
        }

        Ok(left)
    }

    pub fn parse_boolean_expression(&mut self) -> Result<NodeType, ParserError> {
        let mut left = self.parse_comparison_expression()?;
        while let TokenType::Boolean(comparison) = self.first().token_type.clone() {
            let _ = self.eat();

            left = NodeType::BooleanExpression {
                left: Box::new(left),
                right: Box::new(self.parse_comparison_expression()?),
                operator: comparison,
            };
        }

        Ok(left)
    }
}

#[cfg(test)]
mod tests {
    use crate::Parser;
    use crate::ast::comparison::{BooleanOperation, Comparison};
    use crate::ast::{NodeType, binary::BinaryOperator};
    use crate::lexer::{Token, tokenize};

    fn parser_with_tokens(tokens: Vec<Token>) -> Parser {
        Parser { tokens }
    }

    #[test]
    fn test_parse_range_expression_exclusive() {
        let tokens = tokenize(String::from("1 .. 10")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_range_expression().unwrap();
        match node {
            NodeType::RangeDeclaration { inclusive, .. } => assert!(!inclusive),
            _ => panic!("Expected RangeDeclaration"),
        }
    }

    #[test]
    fn test_parse_range_expression_inclusive() {
        let tokens = tokenize(String::from("1 ..= 10")).unwrap();
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
        let tokens = tokenize(String::from("2 ** 3")).unwrap();
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
                assert_eq!(operator, Comparison::Greater);
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
