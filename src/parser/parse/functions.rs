use rand::seq::IndexedRandom;

use crate::lexer::Bracket;
use crate::parser::{Parser, ParserError, SyntaxErr};

use crate::{ast::NodeType, lexer::TokenType};

impl Parser {
    pub fn parse_call_member_expression(&mut self) -> Result<NodeType, ParserError> {
        let member = self.parse_member_expression()?;

        if self.first().token_type == TokenType::Open(Bracket::Paren) {
            return self.parse_call_expression(member);
        }

        Ok(member)
    }

    pub fn parse_call_expression(&mut self, caller: NodeType) -> Result<NodeType, ParserError> {
        let mut expression = NodeType::CallExpression(
            Box::new(caller),
            self.parse_arguments(
                TokenType::Open(Bracket::Paren),
                TokenType::Close(Bracket::Paren),
            )?,
        );

        if self.first().token_type == TokenType::Open(Bracket::Paren) {
            expression = self.parse_call_expression(expression)?;
        }

        Ok(expression)
    }

    pub fn parse_member_expression(&mut self) -> Result<NodeType, ParserError> {
        let mut path = vec![(self.parse_primary_expression()?, false)];

        while self.first().token_type == TokenType::FullStop
            || self.first().token_type == TokenType::Open(Bracket::Square)
        {
            path.push(if self.eat().token_type == TokenType::FullStop {
                let prop = self.parse_call_member_expression()?;
                (prop, false)
            } else {
                let prop = self.parse_statement()?;

                self.expect_eat(
                    &TokenType::Close(Bracket::Square),
                    SyntaxErr::ExpectedClosingBracket(Bracket::Square),
                )?;

                (prop, true)
            });
        }

        if path.len() == 2 && !path[1].1 {
            if let NodeType::Identifier(identifier) = &path[0].0 {
                match &path[1].0 {
                    NodeType::Identifier(value) => {
                        if self.first().token_type == TokenType::Open(Bracket::Curly) {
                            let data = self.parse_potential_key_value()?;
                            return Ok(NodeType::EnumExpression {
                                identifier: identifier.to_string(),
                                value: value.to_string(),
                                data: Some(data),
                            });
                        }
                    }
                    _ => {}
                }
            }
        }

        if path.len() <= 1 {
            Ok(path.remove(0).0)
        } else {
            Ok(NodeType::MemberExpression { path: path })
        }
    }

    pub fn parse_arguments(
        &mut self,
        open_token: TokenType,
        close_token: TokenType,
    ) -> Result<Vec<(NodeType, Option<NodeType>)>, ParserError> {
        let _ = self.expect_eat(&open_token, SyntaxErr::ExpectedToken(open_token.clone()))?;

        let args = if self.first().token_type == close_token {
            Vec::new()
        } else {
            self.parse_arguments_list()?
        };

        let _ = self.expect_eat(&close_token, SyntaxErr::ExpectedToken(close_token.clone()))?;

        Ok(args)
    }

    pub fn parse_arguments_list(
        &mut self,
    ) -> Result<Vec<(NodeType, Option<NodeType>)>, ParserError> {
        let mut defaulted = false;

        let mut args = vec![{
            if defaulted || self.nth(1).token_type == TokenType::Equals {
                (self.parse_primary_expression()?, {
                    let _ = self.expect_eat(&TokenType::Equals, SyntaxErr::ExpectedChar('='))?;
                    defaulted = true;
                    Some(self.parse_statement()?)
                })
            } else {
                (self.parse_statement()?, None)
            }
        }];

        while self.first().token_type == TokenType::Comma {
            let _ = self.eat();

            args.push({
                if defaulted || self.nth(1).token_type == TokenType::Equals {
                    (self.parse_primary_expression()?, {
                        let _ =
                            self.expect_eat(&TokenType::Equals, SyntaxErr::ExpectedChar('='))?;
                        defaulted = true;
                        Some(self.parse_statement()?)
                    })
                } else {
                    (self.parse_statement()?, None)
                }
            });

            if [
                TokenType::Close(Bracket::Curly),
                TokenType::Close(Bracket::Paren),
            ]
            .contains(&self.first().token_type)
            {
                break;
            }
        }

        Ok(args)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::NodeType;
    use crate::lexer::{Token, TokenType, tokenize};

    fn parser_with_tokens(tokens: Vec<Token>) -> Parser {
        Parser { tokens }
    }

    #[test]
    fn test_parse_call_member_expression_simple_call() {
        let tokens = tokenize(String::from("foo()")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let result = parser.parse_call_member_expression();
        assert!(result.is_ok());
        match result.unwrap() {
            NodeType::CallExpression(_, _) => {}
            _ => panic!("Expected CallExpression"),
        }
    }
    #[test]
    fn test_parse_call_expression_member_nested() {
        let tokens = tokenize(String::from("foo.bar()")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let first_call = parser.parse_call_member_expression().unwrap();
        match first_call {
            NodeType::MemberExpression { property, .. } => match *property {
                NodeType::CallExpression(_, _) | NodeType::Identifier(_) => {}
                _ => panic!("Expected nested CallExpression or Identifier"),
            },
            _ => panic!("Expected CallExpression"),
        }
    }

    #[test]
    fn test_parse_call_expression_nested() {
        let tokens = tokenize(String::from("foo()()")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let first_call = parser.parse_call_member_expression().unwrap();
        match first_call {
            NodeType::CallExpression(inner, _) => match *inner {
                NodeType::CallExpression(_, _) | NodeType::Identifier(_) => {}
                _ => panic!("Expected nested CallExpression or Identifier"),
            },
            _ => panic!("Expected CallExpression"),
        }
    }

    #[test]
    fn test_parse_member_expression_dot_access() {
        let tokens = tokenize(String::from("foo.bar")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let result = parser.parse_member_expression();
        assert!(result.is_ok());
        match result.unwrap() {
            NodeType::MemberExpression { .. } => {}
            _ => panic!("Expected MemberExpression"),
        }
    }

    #[test]
    fn test_parse_member_expression_computed_access() {
        let tokens = tokenize(String::from("foo[bar]")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let result = parser.parse_member_expression();
        assert!(result.is_ok());
        match result.unwrap() {
            NodeType::MemberExpression {
                is_computed: true, ..
            } => {}
            _ => panic!("Expected computed MemberExpression"),
        }
    }

    #[test]
    fn test_parse_arguments_empty() {
        let tokens = tokenize(String::from("()")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let result = parser.parse_arguments(
            TokenType::Open(Bracket::Paren),
            TokenType::Close(Bracket::Paren),
        );
        assert!(result.is_ok());
        let args = result.unwrap();
        assert_eq!(args.len(), 0);
    }

    #[test]
    fn test_parse_arguments_with_values() {
        let tokens = tokenize(String::from("(a, b)")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let result = parser.parse_arguments(
            TokenType::Open(Bracket::Paren),
            TokenType::Close(Bracket::Paren),
        );
        assert!(result.is_ok());
        let args = result.unwrap();
        assert_eq!(args.len(), 2);
    }

    #[test]
    fn test_parse_arguments_with_default() {
        let tokens = tokenize(String::from("(a, b = 1)")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let result = parser.parse_arguments(
            TokenType::Open(Bracket::Paren),
            TokenType::Close(Bracket::Paren),
        );
        assert!(result.is_ok());
        let args = result.unwrap();
        assert_eq!(args.len(), 2);
        assert!(args[0].1.is_none());
        assert!(args[1].1.is_some());
    }
}
