use crate::{
    lexer::LexerError,
    parser::{Parser, ParserError, SyntaxErr},
    runtime::values::helper::{ObjectType, StopValue},
};
use std::collections::HashMap;

use crate::{
    ast::{NodeType, binary::BinaryOperator},
    lexer::TokenType,
};

impl Parser {
    pub fn parse_primary_expression(&mut self) -> Result<NodeType, ParserError> {
        Ok(match &self.first().token_type {
            TokenType::Identifier => NodeType::Identifier(self.eat().value),
            TokenType::Float => NodeType::FloatLiteral(self.eat().value.trim().parse().unwrap()),
            TokenType::Integer => {
                NodeType::IntegerLiteral(self.eat().value.trim().parse().unwrap())
            }
            TokenType::Stop(x) => match x {
                StopValue::Return => self.parse_return_declaration()?,
                StopValue::Break => {
                    self.eat();
                    NodeType::Break
                }
                StopValue::Continue => {
                    self.eat();
                    NodeType::Continue
                }
            },
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
                let value = self.parse_expression()?;
                let _ = self.expect_eat(
                    &TokenType::CloseBrackets,
                    SyntaxErr::ExpectedClosingBracket(TokenType::CloseBrackets),
                )?;
                value
            }
            TokenType::BinaryOperator(x) if x == &BinaryOperator::Subtract => {
                self.eat();
                NodeType::NotExpression {
                    value: Box::new(self.parse_expression()?),
                }
            }
            TokenType::Not => {
                self.eat();
                NodeType::NotExpression {
                    value: Box::new(self.parse_expression()?),
                }
            }
            _ => return Err(self.get_err(SyntaxErr::UnexpectedToken)),
        })
    }

    pub fn parse_potential_key_value(
        &mut self,
    ) -> Result<ObjectType<Option<NodeType>>, ParserError> {
        let _ = self.expect_eat(
            &TokenType::OpenCurly,
            SyntaxErr::ExpectedOpeningBracket(TokenType::OpenCurly),
        );
        let mut is_tuple = true;
        let mut tuple = Vec::new();
        let mut properties = HashMap::new();

        while !self.is_eof() && self.first().token_type != TokenType::CloseCurly {
            let key = self.first().value.clone();
            tuple.push(Some(self.parse_expression()?));

            if [TokenType::Comma, TokenType::CloseCurly].contains(&self.first().token_type) {
                if self.first().token_type != TokenType::CloseCurly {
                    let _ = self.eat();
                }

                properties.insert(key, None);
                continue;
            }

            is_tuple = false;

            let _ = self.expect_eat(&TokenType::Colon, SyntaxErr::ExpectedChar(':'))?;

            properties.insert(key, Some(self.parse_expression()?));

            if self.first().token_type != TokenType::CloseCurly {
                let _ = self.expect_eat(&TokenType::Comma, SyntaxErr::ExpectedChar(','))?;
            }
        }

        let _ = self.expect_eat(
            &TokenType::CloseCurly,
            SyntaxErr::ExpectedClosingBracket(TokenType::CloseCurly),
        )?;

        if is_tuple {
            Ok(ObjectType::Tuple(tuple))
        } else {
            Ok(ObjectType::Map(properties))
        }
    }

    pub fn parse_object_expression(&mut self) -> Result<NodeType, ParserError> {
        if self.first().token_type != TokenType::OpenCurly {
            return self.parse_boolean_expression();
        }

        Ok(NodeType::StructLiteral(self.parse_potential_key_value()?))
    }

    pub fn parse_assignment_expression(&mut self) -> Result<NodeType, ParserError> {
        let mut left = self.parse_tuple_expression()?;

        if let TokenType::UnaryAssign(op) = self.first().token_type.clone() {
            let _ = self.eat();
            if [
                BinaryOperator::Power,
                BinaryOperator::Divide,
                BinaryOperator::Multiply,
            ]
            .contains(&op)
            {
                return Err(ParserError::Lexer(LexerError::BinaryOperatorShortHand));
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
            let _ = self.eat();
            left = NodeType::AssignmentExpression {
                identifier: Box::new(left.clone()),
                value: Box::new(NodeType::BinaryExpression {
                    left: Box::new(left),
                    right: Box::new(self.parse_tuple_expression()?),
                    operator: op,
                }),
            };
        } else if [TokenType::Equals].contains(&self.first().token_type) {
            let _ = self.eat();
            let right = self.parse_tuple_expression()?;
            left = NodeType::AssignmentExpression {
                identifier: Box::new(left),
                value: Box::new(right),
            };
        }

        Ok(left)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{Token, TokenType, tokenize};
    use crate::ast::NodeType;
    use crate::runtime::values::helper::{ObjectType, StopValue};
    use crate::parser::Parser;

    fn parser_with_tokens(tokens: Vec<Token>) -> Parser {
        Parser { tokens }
    }

    #[test]
    fn test_parse_primary_expression_identifier() {
        let tokens = tokenize(String::from("foo")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_primary_expression().unwrap();
        match node {
            NodeType::Identifier(ref s) if s == "foo" => {}
            _ => panic!("Expected Identifier"),
        }
    }

    #[test]
    fn test_parse_primary_expression_integer() {
        let tokens = tokenize(String::from("42")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_primary_expression().unwrap();
        match node {
            NodeType::IntegerLiteral(42) => {}
            _ => panic!("Expected IntegerLiteral"),
        }
    }

    #[test]
    fn test_parse_primary_expression_float() {
        let tokens = tokenize(String::from("3.14")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_primary_expression().unwrap();
        match node {
            NodeType::FloatLiteral(f) if (f - 3.14).abs() < 1e-6 => {}
            _ => panic!("Expected FloatLiteral"),
        }
    }

    #[test]
    fn test_parse_primary_expression_string() {
        let tokens = tokenize(String::from("\"hello\"")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_primary_expression().unwrap();
        match node {
            NodeType::StringLiteral(ref s) if s == "hello" => {}
            _ => panic!("Expected StringLiteral"),
        }
    }

    #[test]
    fn test_parse_primary_expression_char() {
        let tokens = tokenize(String::from("\"a\"")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_primary_expression().unwrap();
        match node {
            NodeType::CharLiteral('a') => {}
            _ => panic!("Expected CharLiteral"),
        }
    }

    #[test]
    fn test_parse_primary_expression_parentheses() {
        let tokens = tokenize(String::from("(42)")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_primary_expression().unwrap();
        match node {
            NodeType::IntegerLiteral(42) => {}
            _ => panic!("Expected IntegerLiteral inside parentheses"),
        }
    }

    #[test]
    fn test_parse_primary_expression_not() {
        let tokens = tokenize(String::from("!foo")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_primary_expression().unwrap();
        match node {
            NodeType::NotExpression { .. } => {}
            _ => panic!("Expected NotExpression"),
        }
    }

    #[test]
    fn test_parse_potential_key_value_tuple() {
        let tokens = tokenize(String::from("{1, 2}")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let obj = parser.parse_potential_key_value().unwrap();
        match obj {
            ObjectType::Tuple(v) => assert_eq!(v.len(), 2),
            _ => panic!("Expected Tuple"),
        }
    }

    #[test]
    fn test_parse_potential_key_value_map() {
        let tokens = tokenize(String::from("{a: 1, b: 2}")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let obj = parser.parse_potential_key_value().unwrap();
        match obj {
            ObjectType::Map(m) => {
                assert_eq!(m.len(), 2);
                assert!(m.contains_key("a"));
                assert!(m.contains_key("b"));
            }
            _ => panic!("Expected Map"),
        }
    }

    #[test]
    fn test_parse_object_expression_struct_literal() {
        let tokens = tokenize(String::from("{a: 1, b: 2}")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_object_expression().unwrap();
        match node {
            NodeType::StructLiteral(ObjectType::Map(m)) => {
                assert_eq!(m.len(), 2);
            }
            _ => panic!("Expected StructLiteral"),
        }
    }

    #[test]
    fn test_parse_assignment_expression_equals() {
        let tokens = tokenize(String::from("x = 5")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_assignment_expression().unwrap();
        match node {
            NodeType::AssignmentExpression { .. } => {}
            _ => panic!("Expected AssignmentExpression"),
        }
    }

    #[test]
    fn test_parse_assignment_expression_unary_assign() {
        let tokens = tokenize(String::from("x++")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_assignment_expression().unwrap();
        match node {
            NodeType::AssignmentExpression { .. } => {}
            _ => panic!("Expected AssignmentExpression"),
        }
    }
}
