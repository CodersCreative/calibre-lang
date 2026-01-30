use crate::{
    Parser, SyntaxErr,
    ast::{
        Node, ObjectType, PotentialDollarIdentifier, PotentialGenericTypeIdentifier, RefMutability,
        comparison::BooleanOperator,
    },
    lexer::{Bracket, Span, StopValue},
};
use crate::{
    ast::{NodeType, binary::BinaryOperator},
    lexer::TokenType,
};

impl Parser {
    pub fn parse_primary_expression(&mut self) -> Node {
        let node = match &self.first().token_type {
            TokenType::If => self.parse_if_statement(),
            TokenType::Match => self.parse_match_statement(),
            TokenType::List | TokenType::Open(Bracket::Square) => self.parse_list_iter_expression(),
            TokenType::Identifier => {
                let val = self.eat();
                let constraints = self.parse_generic_types();
                Node::new(
                    val.span,
                    NodeType::Identifier(if constraints.is_empty() {
                        PotentialDollarIdentifier::Identifier(val.into()).into()
                    } else {
                        PotentialGenericTypeIdentifier::Generic {
                            identifier: PotentialDollarIdentifier::Identifier(val.into()),
                            generic_types: constraints,
                        }
                    }),
                )
            }
            TokenType::Float => {
                let val = self.eat();
                Node::new(
                    val.span,
                    NodeType::FloatLiteral(val.value.trim().parse().unwrap()),
                )
            }
            TokenType::Null => Node::new(self.eat().span, NodeType::Null),
            TokenType::Integer => {
                let val = self.eat();
                Node::new(
                    val.span,
                    NodeType::IntLiteral(val.value.trim().parse().unwrap()),
                )
            }
            TokenType::Stop(x) => match x {
                StopValue::Until => {
                    let open = self.eat();
                    let condition = self.parse_statement();
                    Node::new(
                        Span::new_from_spans(open.span, condition.span),
                        NodeType::Until {
                            condition: Box::new(condition),
                        },
                    )
                }
                StopValue::Return => self.parse_return_declaration(),
                StopValue::Break => {
                    let val = self.eat();
                    Node::new(val.span, NodeType::Break)
                }
                StopValue::Continue => {
                    let val = self.eat();
                    Node::new(val.span, NodeType::Continue)
                }
            },
            TokenType::String => {
                let val = self.eat();

                Node::new(val.span, NodeType::StringLiteral(val.into()))
            }
            TokenType::Char => {
                let val = self.eat();

                Node::new(
                    val.span,
                    NodeType::CharLiteral(val.value.chars().nth(0).unwrap()),
                )
            }
            TokenType::Drop => {
                let open = self.eat();
                let ident = self.expect_potential_dollar_ident();

                Node::new(
                    Span::new_from_spans(open.span, *ident.span()),
                    NodeType::Drop(ident),
                )
            }
            TokenType::Move => {
                let open = self.eat();
                let ident = self.expect_potential_dollar_ident();

                Node::new(
                    Span::new_from_spans(open.span, *ident.span()),
                    NodeType::Move(ident),
                )
            }
            TokenType::Defer => {
                let open = self.eat();

                let function = if self.first().token_type == TokenType::Stop(StopValue::Return) {
                    let _ = self.eat();
                    true
                } else {
                    false
                };

                let ident = self.parse_statement();

                Node::new(
                    Span::new_from_spans(open.span, ident.span),
                    NodeType::Defer {
                        value: Box::new(ident),
                        function,
                    },
                )
            }
            TokenType::FatArrow => self.parse_scope_declaration(false),
            TokenType::Open(Bracket::Paren) => self.parse_paren_expression(),
            TokenType::BinaryOperator(BinaryOperator::Sub) => {
                let open = self.eat();
                let val = self.parse_statement();
                Node::new(
                    Span::new_from_spans(open.span, val.span),
                    NodeType::NegExpression {
                        value: Box::new(val),
                    },
                )
            }
            TokenType::Dollar => {
                let val = self.expect_potential_dollar_ident();
                let constraints = self.parse_generic_types();

                Node::new(
                    *val.span(),
                    NodeType::Identifier(if constraints.is_empty() {
                        val.into()
                    } else {
                        PotentialGenericTypeIdentifier::Generic {
                            identifier: val.into(),
                            generic_types: constraints,
                        }
                    }),
                )
            }
            TokenType::Not => {
                let open = self.eat();
                let val = self.parse_statement();

                Node::new(
                    Span::new_from_spans(open.span, val.span),
                    NodeType::NotExpression {
                        value: Box::new(val),
                    },
                )
            }
            TokenType::Debug => {
                let open = self.eat();
                let val = self.parse_statement();

                Node::new(
                    Span::new_from_spans(open.span, val.span),
                    NodeType::DebugExpression {
                        value: Box::new(val),
                    },
                )
            }
            TokenType::Try => self.parse_try_expression(),
            TokenType::Func => self.parse_function_declaration(),
            TokenType::BinaryOperator(BinaryOperator::Mul) => {
                let open = self.eat();
                let close = self.parse_purely_member();
                Node::new(
                    Span::new_from_spans(open.span, close.span),
                    NodeType::DerefStatement {
                        value: Box::new(close),
                    },
                )
            }
            TokenType::Type => {
                let open = self.eat();
                let _ = self.expect_eat(&TokenType::Colon, SyntaxErr::ExpectedChar(':'));
                let data_type = self.expect_potential_new_type();
                Node {
                    span: Span::new_from_spans(open.span, *data_type.span()),
                    node_type: NodeType::DataType { data_type },
                }
            }
            TokenType::BinaryOperator(BinaryOperator::Pow) => {
                let open = self.eat();
                let close = self.parse_purely_member();
                Node::new(
                    Span::new_from_spans(open.span, close.span),
                    NodeType::DerefStatement {
                        value: Box::new(Node::new(
                            Span::new_from_spans(open.span, close.span),
                            NodeType::DerefStatement {
                                value: Box::new(close),
                            },
                        )),
                    },
                )
            }

            TokenType::Boolean(BooleanOperator::And) => {
                let open = self.eat();
                let close = self.parse_purely_member();
                Node::new(
                    Span::new_from_spans(open.span, close.span),
                    NodeType::RefStatement {
                        mutability: RefMutability::Ref,
                        value: Box::new(Node::new(
                            Span::new_from_spans(open.span, close.span),
                            NodeType::RefStatement {
                                mutability: RefMutability::Ref,
                                value: Box::new(close),
                            },
                        )),
                    },
                )
            }
            x if RefMutability::from(x.clone()) != RefMutability::Value => {
                let open = self.eat();
                let close = self.parse_purely_member();
                Node::new(
                    Span::new_from_spans(open.span, close.span),
                    NodeType::RefStatement {
                        mutability: RefMutability::from(open.token_type),
                        value: Box::new(close),
                    },
                )
            }
            TokenType::EOL => Node::new(self.eat().span, NodeType::EmptyLine),
            _ => {
                self.add_err(SyntaxErr::UnexpectedToken);
                Node::new(self.eat().span, NodeType::EmptyLine)
            }
        };

        self.parse_potential_member(node)
    }

    pub fn expect_potential_generic_type_ident(&mut self) -> PotentialGenericTypeIdentifier {
        let ident = self.expect_potential_dollar_ident();
        let constraints = self.parse_generic_types();

        if constraints.is_empty() {
            PotentialGenericTypeIdentifier::Identifier(ident)
        } else {
            PotentialGenericTypeIdentifier::Generic {
                identifier: ident,
                generic_types: constraints,
            }
        }
    }

    pub fn parse_potential_generic_type_ident(&mut self) -> Option<PotentialGenericTypeIdentifier> {
        let ident = self.parse_potential_dollar_ident()?;
        let constraints = self.parse_generic_types();

        if constraints.is_empty() {
            Some(PotentialGenericTypeIdentifier::Identifier(ident))
        } else {
            Some(PotentialGenericTypeIdentifier::Generic {
                identifier: ident,
                generic_types: constraints,
            })
        }
    }

    pub fn expect_potential_dollar_ident(&mut self) -> PotentialDollarIdentifier {
        if self.first().token_type == TokenType::Dollar {
            let open = self.eat();
            if self.first().token_type == TokenType::Identifier {
                let ident = self.expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedIdentifier);
                PotentialDollarIdentifier::DollarIdentifier(ident.into())
            } else {
                PotentialDollarIdentifier::Identifier(open.into())
            }
        } else {
            let ident = self.expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedIdentifier);
            PotentialDollarIdentifier::Identifier(ident.into())
        }
    }

    pub fn parse_potential_dollar_ident(&mut self) -> Option<PotentialDollarIdentifier> {
        if self.first().token_type == TokenType::Dollar {
            let open = self.eat();
            if self.first().token_type == TokenType::Identifier {
                let ident = self.expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedIdentifier);
                Some(PotentialDollarIdentifier::DollarIdentifier(ident.into()))
            } else {
                Some(PotentialDollarIdentifier::Identifier(open.into()))
            }
        } else if self.first().token_type == TokenType::Identifier {
            let ident = self.expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedIdentifier);
            Some(PotentialDollarIdentifier::Identifier(ident.into()))
        } else {
            None
        }
    }

    pub fn parse_potential_key_value(&mut self) -> ObjectType<Node> {
        match self.first().token_type {
            TokenType::Open(Bracket::Curly) => {
                let _ = self.eat();
                let mut properties = Vec::new();
                while !self.is_eof() && self.first().token_type != TokenType::Close(Bracket::Curly)
                {
                    let key = self.expect_potential_dollar_ident();

                    if [TokenType::Comma, TokenType::Close(Bracket::Curly)]
                        .contains(&self.first().token_type)
                    {
                        if self.first().token_type != TokenType::Close(Bracket::Curly) {
                            let _ = self.eat();
                        }

                        properties.push((
                            key.to_string(),
                            Node::new(*key.span(), NodeType::Identifier(key.into())),
                        ));
                        continue;
                    }

                    let _ = self.expect_eat(&TokenType::Colon, SyntaxErr::ExpectedChar(':'));

                    properties.push((key.to_string(), self.parse_statement()));

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

    pub fn parse_assignment_expression(&mut self) -> Node {
        let mut left: Node = self.parse_pipe_expression();

        if let TokenType::BooleanAssign(op) = self.first().token_type.clone() {
            let open = self.eat();
            left = Node::new(
                open.span,
                NodeType::AssignmentExpression {
                    identifier: Box::new(left.clone()),
                    value: Box::new(Node::new(
                        open.span,
                        NodeType::BooleanExpression {
                            left: Box::new(left),
                            right: Box::new(self.parse_statement()),
                            operator: op,
                        },
                    )),
                },
            );
        } else if let TokenType::BinaryAssign(op) = self.first().token_type.clone() {
            let open = self.eat();
            left = Node::new(
                open.span,
                NodeType::AssignmentExpression {
                    identifier: Box::new(left.clone()),
                    value: Box::new(Node::new(
                        open.span,
                        NodeType::BinaryExpression {
                            left: Box::new(left),
                            right: Box::new(self.parse_statement()),
                            operator: op,
                        },
                    )),
                },
            );
        } else if [TokenType::Equals].contains(&self.first().token_type) {
            let open = self.eat();
            let right = self.parse_statement();
            left = Node::new(
                open.span,
                NodeType::AssignmentExpression {
                    identifier: Box::new(left),
                    value: Box::new(right),
                },
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
