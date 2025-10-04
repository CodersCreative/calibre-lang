use crate::{
    Parser, ParserError, SyntaxErr,
    ast::{Node, ObjectType, RefMutability},
    lexer::{Bracket, LexerError, Span, StopValue},
};
use crate::{
    ast::{NodeType, binary::BinaryOperator},
    lexer::TokenType,
};
use std::collections::HashMap;

impl Parser {
    pub fn parse_primary_expression(&mut self) -> Result<Node, ParserError> {
        Ok(match &self.first().token_type {
            TokenType::Identifier => {
                let val = self.eat();
                Node::new(NodeType::Identifier(val.value), val.span)
            }
            TokenType::Float => {
                let val = self.eat();
                Node::new(
                    NodeType::FloatLiteral(val.value.trim().parse().unwrap()),
                    val.span,
                )
            }
            TokenType::Integer => {
                let val = self.eat();
                Node::new(
                    NodeType::IntLiteral(val.value.trim().parse().unwrap()),
                    val.span,
                )
            }
            TokenType::Stop(x) => match x {
                StopValue::Return => self.parse_return_declaration()?,
                StopValue::Break => {
                    let val = self.eat();
                    Node::new(NodeType::Break, val.span)
                }
                StopValue::Continue => {
                    let val = self.eat();
                    Node::new(NodeType::Continue, val.span)
                }
            },
            TokenType::String => {
                let val = self.eat();
                if val.value.len() == 1 {
                    Node::new(
                        NodeType::CharLiteral(val.value.chars().nth(0).unwrap()),
                        val.span,
                    )
                } else {
                    Node::new(NodeType::StringLiteral(val.value.to_string()), val.span)
                }
            }
            TokenType::Open(Bracket::Paren) => self.parse_tuple_expression()?,
            TokenType::Open(Bracket::Square) => self.parse_list_expression()?,
            TokenType::BinaryOperator(x) if x == &BinaryOperator::Sub => {
                let open = self.eat();
                let val = self.parse_statement()?;
                let span = Span::new_from_spans(open.span, val.span);
                Node::new(
                    NodeType::NotExpression {
                        value: Box::new(val),
                    },
                    span,
                )
            }
            TokenType::Not => {
                let open = self.eat();
                let val = self.parse_statement()?;
                let span = Span::new_from_spans(open.span, val.span);
                Node::new(
                    NodeType::NotExpression {
                        value: Box::new(val),
                    },
                    span,
                )
            }
            TokenType::Try => self.parse_try_expression()?,
            TokenType::Func => self.parse_function_declaration()?,
            x if RefMutability::from(x.clone()) != RefMutability::Value => {
                let open = self.eat();
                let close =
                    self.expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedIdentifier)?;
                Node::new(
                    NodeType::RefStatement {
                        mutability: RefMutability::from(open.token_type),
                        value: close.value,
                    },
                    Span::new_from_spans(open.span, close.span),
                )
            }
            _ => return Err(self.get_err(SyntaxErr::UnexpectedToken)),
        })
    }

    pub fn parse_potential_key_value(&mut self) -> Result<ObjectType<Option<Node>>, ParserError> {
        match self.first().token_type {
            TokenType::Open(Bracket::Curly) => {
                let _ = self.eat();
                let mut properties = HashMap::new();
                while !self.is_eof() && self.first().token_type != TokenType::Close(Bracket::Curly)
                {
                    let key = self
                        .expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedIdentifier)?
                        .value;

                    if [TokenType::Comma, TokenType::Close(Bracket::Curly)]
                        .contains(&self.first().token_type)
                    {
                        if self.first().token_type != TokenType::Close(Bracket::Curly) {
                            let _ = self.eat();
                        }

                        properties.insert(key, None);
                        continue;
                    }

                    let _ = self.expect_eat(&TokenType::Colon, SyntaxErr::ExpectedChar(':'))?;

                    properties.insert(key, Some(self.parse_statement()?));

                    if self.first().token_type != TokenType::Close(Bracket::Curly) {
                        let _ = self.expect_eat(&TokenType::Comma, SyntaxErr::ExpectedChar(','))?;
                    }
                }
                let _ = self.expect_eat(
                    &TokenType::Close(Bracket::Curly),
                    SyntaxErr::ExpectedClosingBracket(Bracket::Curly),
                )?;
                Ok(ObjectType::Map(properties))
            }
            _ => {
                let _ = self.expect_eat(
                    &TokenType::Open(Bracket::Paren),
                    SyntaxErr::ExpectedOpeningBracket(Bracket::Paren),
                );
                let mut tuple = Vec::new();
                while !self.is_eof() && self.first().token_type != TokenType::Close(Bracket::Paren)
                {
                    tuple.push(Some(self.parse_statement()?));
                    if self.first().token_type != TokenType::Close(Bracket::Paren) {
                        let _ = self.expect_eat(&TokenType::Comma, SyntaxErr::ExpectedChar(','))?;
                    }
                }
                let _ = self.expect_eat(
                    &TokenType::Close(Bracket::Paren),
                    SyntaxErr::ExpectedClosingBracket(Bracket::Paren),
                )?;
                Ok(ObjectType::Tuple(tuple))
            }
        }
    }

    pub fn parse_object_expression(&mut self) -> Result<Node, ParserError> {
        if self.first().token_type != TokenType::Open(Bracket::Curly) {
            return self.parse_try_expression();
        }

        let open = self.first().clone();

        Ok(Node::new(
            NodeType::StructLiteral(self.parse_potential_key_value()?),
            open.span,
        ))
    }

    pub fn parse_assignment_expression(&mut self) -> Result<Node, ParserError> {
        let mut left: Node = self.parse_pipe_expression()?;

        if let TokenType::BinaryAssign(op) = self.first().token_type.clone() {
            let open = self.eat();
            left = Node::new(
                NodeType::AssignmentExpression {
                    identifier: Box::new(left.clone()),
                    value: Box::new(Node::new(
                        NodeType::BinaryExpression {
                            left: Box::new(left),
                            right: Box::new(self.parse_statement()?),
                            operator: op,
                        },
                        open.span.clone(),
                    )),
                },
                open.span,
            );
        } else if [TokenType::Equals].contains(&self.first().token_type) {
            let open = self.eat();
            let right = self.parse_statement()?;
            left = Node::new(
                NodeType::AssignmentExpression {
                    identifier: Box::new(left),
                    value: Box::new(right),
                },
                open.span,
            );
        }

        Ok(left)
    }
}

#[cfg(test)]
mod tests {
    use crate::Parser;
    use crate::ast::NodeType;
    use crate::ast::ObjectType;
    use crate::lexer::{Token, tokenize};

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
    fn test_parse_primary_expression_int() {
        let tokens = tokenize(String::from("42")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_primary_expression().unwrap();
        match node {
            NodeType::IntLiteral(42) => {}
            _ => panic!("Expected IntLiteral"),
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
            NodeType::IntLiteral(42) => {}
            _ => panic!("Expected IntLiteral inside parentheses"),
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
        let tokens = tokenize(String::from("{a, b}")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let obj = parser.parse_potential_key_value().unwrap();
        match obj {
            ObjectType::Map(v) => assert_eq!(v.len(), 2),
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
