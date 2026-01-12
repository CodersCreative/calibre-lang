use crate::{
    Parser, SyntaxErr,
    ast::{Node, ObjectType, ParserText, RefMutability},
    lexer::{Bracket, Span, StopValue},
};
use crate::{
    ast::{NodeType, binary::BinaryOperator},
    lexer::TokenType,
};
use std::collections::HashMap;

impl Parser {
    pub fn parse_primary_expression(&mut self) -> Node {
        match &self.first().token_type {
            TokenType::If => self.parse_if_statement(),
            TokenType::Match => self.parse_match_declaration(),
            TokenType::List => self.parse_list_iter_expression(),
            TokenType::Identifier => {
                let val = self.eat();
                Node::new(NodeType::Identifier(val.clone().into()), val.span)
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
                StopValue::Return => self.parse_return_declaration(),
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

                Node::new(NodeType::StringLiteral(val.clone().into()), val.span)
            }
            TokenType::Char => {
                let val = self.eat();

                Node::new(
                    NodeType::CharLiteral(val.value.chars().nth(0).unwrap()),
                    val.span,
                )
            }
            TokenType::FatArrow => self.parse_scope_declaration(false),
            TokenType::Open(Bracket::Paren) => self.parse_paren_expression(),
            TokenType::BinaryOperator(BinaryOperator::Sub) => {
                let open = self.eat();
                let val = self.parse_statement();
                let span = Span::new_from_spans(open.span, val.span);
                Node::new(
                    NodeType::NegExpression {
                        value: Box::new(val),
                    },
                    span,
                )
            }
            TokenType::Not => {
                let open = self.eat();
                let val = self.parse_statement();
                let span = Span::new_from_spans(open.span, val.span);
                Node::new(
                    NodeType::NotExpression {
                        value: Box::new(val),
                    },
                    span,
                )
            }
            TokenType::Debug => {
                let open = self.eat();
                let val = self.parse_statement();
                let span = Span::new_from_spans(open.span, val.span);
                Node::new(
                    NodeType::DebugExpression {
                        value: Box::new(val),
                    },
                    span,
                )
            }
            TokenType::Try => self.parse_try_expression(),
            TokenType::Func => self.parse_function_declaration(),
            TokenType::BinaryOperator(BinaryOperator::Mul) => {
                let open = self.eat();
                let close = self.parse_purely_member();
                Node::new(
                    NodeType::DerefStatement {
                        value: Box::new(close.clone()),
                    },
                    Span::new_from_spans(open.span, close.span),
                )
            }
            TokenType::BinaryOperator(BinaryOperator::Pow) => {
                let open = self.eat();
                let close = self.parse_purely_member();
                Node::new(
                    NodeType::DerefStatement {
                        value: Box::new(Node::new(
                            NodeType::DerefStatement {
                                value: Box::new(close.clone()),
                            },
                            Span::new_from_spans(open.span, close.span),
                        )),
                    },
                    Span::new_from_spans(open.span, close.span),
                )
            }

            TokenType::Boolean(crate::ast::comparison::BooleanOperation::And) => {
                let open = self.eat();
                let close = self.parse_purely_member();
                Node::new(
                    NodeType::RefStatement {
                        mutability: RefMutability::Ref,
                        value: Box::new(Node::new(
                            NodeType::RefStatement {
                                mutability: RefMutability::Ref,
                                value: Box::new(close.clone()),
                            },
                            Span::new_from_spans(open.span, close.span),
                        )),
                    },
                    Span::new_from_spans(open.span, close.span),
                )
            }
            x if RefMutability::from(x.clone()) != RefMutability::Value => {
                let open = self.eat();
                let close = self.parse_purely_member();
                Node::new(
                    NodeType::RefStatement {
                        mutability: RefMutability::from(open.token_type),
                        value: Box::new(close.clone()),
                    },
                    Span::new_from_spans(open.span, close.span),
                )
            }
            TokenType::EOL => Node::new(NodeType::EmptyLine, self.eat().span),
            _ => {
                self.add_err(SyntaxErr::UnexpectedToken);
                Node::new(NodeType::EmptyLine, self.eat().span)
            }
        }
    }

    pub fn parse_potential_key_value(&mut self) -> ObjectType<Node> {
        match self.first().token_type {
            TokenType::Open(Bracket::Curly) => {
                let _ = self.eat();
                let mut properties = HashMap::new();
                while !self.is_eof() && self.first().token_type != TokenType::Close(Bracket::Curly)
                {
                    let key =
                        self.expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedIdentifier);

                    if [TokenType::Comma, TokenType::Close(Bracket::Curly)]
                        .contains(&self.first().token_type)
                    {
                        if self.first().token_type != TokenType::Close(Bracket::Curly) {
                            let _ = self.eat();
                        }

                        properties.insert(
                            key.value.clone(),
                            Node::new(
                                NodeType::Identifier(ParserText::new(key.value, key.span)),
                                key.span,
                            ),
                        );
                        continue;
                    }

                    let _ = self.expect_eat(&TokenType::Colon, SyntaxErr::ExpectedChar(':'));

                    properties.insert(key.value, self.parse_statement());

                    if self.first().token_type != TokenType::Close(Bracket::Curly) {
                        let _ = self.expect_eat(&TokenType::Comma, SyntaxErr::ExpectedChar(','));
                    }
                }
                let _ = self.expect_eat(
                    &TokenType::Close(Bracket::Curly),
                    SyntaxErr::ExpectedClosingBracket(Bracket::Curly),
                );
                ObjectType::Map(properties)
            }
            _ => {
                let _ = self.expect_eat(
                    &TokenType::Open(Bracket::Paren),
                    SyntaxErr::ExpectedOpeningBracket(Bracket::Paren),
                );
                let mut tuple = Vec::new();
                while !self.is_eof() && self.first().token_type != TokenType::Close(Bracket::Paren)
                {
                    tuple.push(self.parse_statement());
                    if self.first().token_type != TokenType::Close(Bracket::Paren) {
                        let _ = self.expect_eat(&TokenType::Comma, SyntaxErr::ExpectedChar(','));
                    }
                }
                let _ = self.expect_eat(
                    &TokenType::Close(Bracket::Paren),
                    SyntaxErr::ExpectedClosingBracket(Bracket::Paren),
                );
                ObjectType::Tuple(tuple)
            }
        }
    }

    /*pub fn parse_object_expression(&mut self) -> Node {
        if self.first().token_type != TokenType::Open(Bracket::Curly) {
            return self.parse_try_expression();
        }

        let open = self.first().clone();

        Node::new(
            NodeType::StructLiteral(self.parse_potential_key_value()),
            open.span,
        )
    }*/

    pub fn parse_assignment_expression(&mut self) -> Node {
        let mut left: Node = self.parse_pipe_expression();

        if let TokenType::BooleanAssign(op) = self.first().token_type.clone() {
            let open = self.eat();
            left = Node::new(
                NodeType::AssignmentExpression {
                    identifier: Box::new(left.clone()),
                    value: Box::new(Node::new(
                        NodeType::BooleanExpression {
                            left: Box::new(left),
                            right: Box::new(self.parse_statement()),
                            operator: op,
                        },
                        open.span.clone(),
                    )),
                },
                open.span,
            );
        } else if let TokenType::BinaryAssign(op) = self.first().token_type.clone() {
            let open = self.eat();
            left = Node::new(
                NodeType::AssignmentExpression {
                    identifier: Box::new(left.clone()),
                    value: Box::new(Node::new(
                        NodeType::BinaryExpression {
                            left: Box::new(left),
                            right: Box::new(self.parse_statement()),
                            operator: op,
                        },
                        open.span.clone(),
                    )),
                },
                open.span,
            );
        } else if [TokenType::Equals].contains(&self.first().token_type) {
            let open = self.eat();
            let right = self.parse_statement();
            left = Node::new(
                NodeType::AssignmentExpression {
                    identifier: Box::new(left),
                    value: Box::new(right),
                },
                open.span,
            );
        }

        left
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
