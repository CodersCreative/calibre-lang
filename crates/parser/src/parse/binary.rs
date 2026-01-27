use crate::ast::comparison::ComparisonOperator;
use crate::ast::{Node, PipeSegment};
use crate::lexer::Span;
use crate::{Parser, SyntaxErr};
use crate::{
    ast::{NodeType, binary::BinaryOperator},
    lexer::TokenType,
};

impl Parser {
    pub fn parse_pipe_expression(&mut self) -> Node {
        let mut left = vec![PipeSegment::Unnamed(self.parse_try_expression())];

        while TokenType::Pipe == self.first().token_type
            || self.first().token_type == TokenType::OrColon
        {
            let name = if TokenType::Pipe == self.eat().token_type {
                None
            } else {
                let name = self.expect_potential_dollar_ident();
                let _ = self.expect_eat(
                    &TokenType::Comparison(ComparisonOperator::Greater),
                    SyntaxErr::ExpectedChar('>'),
                );
                Some(name)
            };

            let value = self.parse_statement();
            if let NodeType::PipeExpression(mut x) = value.node_type {
                if let (Some(first), Some(name)) = (x.first_mut(), name) {
                    *first = PipeSegment::Named {
                        identifier: name,
                        node: first.get_node().clone(),
                    }
                }
                left.append(&mut x);
            } else {
                left.push(if let Some(name) = name {
                    PipeSegment::Named {
                        identifier: name,
                        node: value,
                    }
                } else {
                    PipeSegment::Unnamed(value)
                });
            }
        }

        if left.len() > 1 {
            Node::new(
                Span::new_from_spans(*left[0].span(), *left.last().unwrap().span()),
                NodeType::PipeExpression(left),
            )
        } else {
            left.remove(0).into()
        }
    }

    pub fn parse_as_expression(&mut self) -> Node {
        let mut left = self.parse_call_member_expression();

        while let TokenType::As = self.first().token_type.clone() {
            left = Node::new(
                self.eat().span,
                NodeType::AsExpression {
                    value: Box::new(left),
                    data_type: self.expect_potential_new_type(),
                },
            );
        }

        left
    }

    pub fn parse_in_expression(&mut self) -> Node {
        let mut left = self.parse_bitwise_expression();

        while let TokenType::In = self.first().token_type.clone() {
            left = Node::new(
                self.eat().span,
                NodeType::InDeclaration {
                    identifier: Box::new(left),
                    value: Box::new(self.parse_statement()),
                },
            )
        }

        left
    }

    pub fn parse_range_expression(&mut self) -> Node {
        let mut left = self.parse_boolean_expression();

        if let TokenType::Range = self.first().token_type.clone() {
            let token = self.eat();

            let inclusive = if self.first().token_type == TokenType::Equals {
                let _ = self.eat();
                true
            } else {
                false
            };

            left = Node::new(
                token.span,
                NodeType::RangeDeclaration {
                    from: Box::new(left),
                    to: Box::new(self.parse_boolean_expression()),
                    inclusive,
                },
            )
        }

        left
    }

    pub fn parse_additive_expression(&mut self) -> Node {
        let mut left = self.parse_multiplicative_expression();

        while let TokenType::BinaryOperator(op) = self.first().token_type.clone() {
            if [BinaryOperator::Add, BinaryOperator::Sub].contains(&op) {
                left = Node::new(
                    self.eat().span,
                    NodeType::BinaryExpression {
                        left: Box::new(left),
                        right: Box::new(self.parse_multiplicative_expression()),
                        operator: op,
                    },
                );
            } else {
                break;
            }
        }

        left
    }

    pub fn parse_multiplicative_expression(&mut self) -> Node {
        let mut left = self.parse_power_expression();
        while let TokenType::BinaryOperator(op) = self.first().token_type.clone() {
            if [
                BinaryOperator::Mul,
                BinaryOperator::Div,
                BinaryOperator::Mod,
            ]
            .contains(&op)
            {
                left = Node::new(
                    self.eat().span,
                    NodeType::BinaryExpression {
                        left: Box::new(left),
                        right: Box::new(self.parse_power_expression()),
                        operator: op,
                    },
                );
            } else {
                break;
            }
        }

        left
    }

    pub fn parse_bitwise_expression(&mut self) -> Node {
        let mut left = self.parse_shift_expression();
        while [
            TokenType::Ref,
            TokenType::Or,
            TokenType::BinaryOperator(BinaryOperator::BitXor),
        ]
        .contains(&self.first().token_type)
        {
            let op = self.eat();

            left = Node::new(
                op.span,
                NodeType::BinaryExpression {
                    left: Box::new(left),
                    right: Box::new(self.parse_shift_expression()),
                    operator: match op.token_type {
                        TokenType::Ref => BinaryOperator::BitAnd,
                        TokenType::Or => BinaryOperator::BitOr,
                        TokenType::BinaryOperator(x) => x,
                        _ => {
                            self.add_err(SyntaxErr::UnexpectedToken);
                            BinaryOperator::BitAnd
                        }
                    },
                },
            );
        }

        left
    }
    pub fn parse_shift_expression(&mut self) -> Node {
        let mut left = self.parse_additive_expression();
        while [
            (
                TokenType::Comparison(crate::ast::comparison::ComparisonOperator::Greater),
                TokenType::Comparison(crate::ast::comparison::ComparisonOperator::Greater),
            ),
            (
                TokenType::Comparison(crate::ast::comparison::ComparisonOperator::Lesser),
                TokenType::Comparison(crate::ast::comparison::ComparisonOperator::Lesser),
            ),
        ]
        .contains(&(
            self.first().token_type.clone(),
            self.second().token_type.clone(),
        )) {
            let token = self.eat();
            let _ = self.eat();

            left = Node::new(
                token.span,
                NodeType::BinaryExpression {
                    left: Box::new(left),
                    right: Box::new(self.parse_additive_expression()),
                    operator: match token.token_type {
                        TokenType::Comparison(
                            crate::ast::comparison::ComparisonOperator::Greater,
                        ) => BinaryOperator::Shr,
                        _ => BinaryOperator::Shl,
                    },
                },
            );
        }

        left
    }

    pub fn parse_power_expression(&mut self) -> Node {
        let mut left = self.parse_as_expression();

        while let TokenType::BinaryOperator(BinaryOperator::Pow) = self.first().token_type.clone() {
            left = Node::new(
                self.eat().span,
                NodeType::BinaryExpression {
                    left: Box::new(left),
                    right: Box::new(self.parse_as_expression()),
                    operator: BinaryOperator::Pow,
                },
            );
        }

        left
    }

    pub fn parse_comparison_expression(&mut self) -> Node {
        let mut left = self.parse_in_expression();
        while let TokenType::Comparison(mut comparison) = self.first().token_type.clone() {
            let token = self.eat();

            if self.first().token_type == TokenType::Equals {
                match comparison {
                    crate::ast::comparison::ComparisonOperator::Greater => {
                        let _ = self.eat();
                        comparison = crate::ast::comparison::ComparisonOperator::GreaterEqual;
                    }
                    crate::ast::comparison::ComparisonOperator::Lesser => {
                        let _ = self.eat();
                        comparison = crate::ast::comparison::ComparisonOperator::LesserEqual;
                    }
                    _ => {}
                }
            }

            left = Node::new(
                token.span,
                NodeType::ComparisonExpression {
                    left: Box::new(left),
                    right: Box::new(self.parse_in_expression()),
                    operator: comparison,
                },
            );
        }

        left
    }

    pub fn parse_boolean_expression(&mut self) -> Node {
        let mut left = self.parse_comparison_expression();
        while let TokenType::Boolean(operator) = self.first().token_type.clone() {
            left = Node::new(
                self.eat().span,
                NodeType::BooleanExpression {
                    left: Box::new(left),
                    right: Box::new(self.parse_comparison_expression()),
                    operator,
                },
            );
        }

        left
    }
}

#[cfg(test)]
mod tests {
    use crate::Parser;
    use crate::ast::comparison::{BooleanOperator, ComparisonOperator};
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
                assert_eq!(operator, ComparisonOperator::Greater);
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
                assert_eq!(operator, BooleanOperator::Or);
            }
            _ => panic!("Expected BooleanExpression"),
        }
    }
}
