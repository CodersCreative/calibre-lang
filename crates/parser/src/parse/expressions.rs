use crate::{
    Parser, SyntaxErr,
    ast::{
        CallArg, DestructurePattern, Node, ObjectType, ParserText, PotentialDollarIdentifier,
        PotentialGenericTypeIdentifier, RefMutability, VarType, comparison::BooleanOperator,
    },
    lexer::{Bracket, Span, StopValue},
};
use crate::{
    ast::{NodeType, binary::BinaryOperator},
    lexer::TokenType,
};

impl Parser {
    fn has_token_after_comma(&self, target: TokenType) -> bool {
        let mut depth_paren = 0;
        let mut depth_square = 0;
        let mut depth_curly = 0;
        let mut i = 0;

        while let Some(tok) = self.nth(i) {
            i += 1;
            match tok.token_type {
                TokenType::Open(Bracket::Paren) => depth_paren += 1,
                TokenType::Open(Bracket::Square) => depth_square += 1,
                TokenType::Open(Bracket::Curly) => depth_curly += 1,
                TokenType::Close(Bracket::Paren) => {
                    if depth_paren == 0 {
                        return false;
                    }
                    depth_paren -= 1;
                }
                TokenType::Close(Bracket::Square) => {
                    if depth_square == 0 {
                        return false;
                    }
                    depth_square -= 1;
                }
                TokenType::Close(Bracket::Curly) => {
                    if depth_curly == 0 {
                        return false;
                    }
                    depth_curly -= 1;
                }
                TokenType::EOL | TokenType::EOF => return false,
                TokenType::Dollar
                    if target == TokenType::Equals
                        && depth_paren == 0
                        && depth_square == 0
                        && depth_curly == 0 =>
                {
                    return false;
                }
                ref t
                    if *t == target
                        && depth_paren == 0
                        && depth_square == 0
                        && depth_curly == 0 =>
                {
                    return true;
                }
                TokenType::FatArrow | TokenType::Arrow | TokenType::Equals
                    if depth_paren == 0 && depth_square == 0 && depth_curly == 0 =>
                {
                    if tok.token_type != target {
                        return false;
                    }
                }
                _ => {}
            }
        }

        false
    }

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
                let parsed = match val.value.trim().parse() {
                    Ok(num) => num,
                    Err(_) => {
                        self.add_err_at(SyntaxErr::InvalidLiteral(val.value.clone()), val.span);
                        0.0
                    }
                };
                Node::new(val.span, NodeType::FloatLiteral(parsed))
            }
            TokenType::Null => Node::new(self.eat().span, NodeType::Null),
            TokenType::Integer => {
                let val = self.eat();
                let parsed = match val.value.trim().parse() {
                    Ok(num) => num,
                    Err(_) => {
                        self.add_err_at(SyntaxErr::InvalidLiteral(val.value.clone()), val.span);
                        0
                    }
                };
                Node::new(val.span, NodeType::IntLiteral(parsed.to_string()))
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
                    let label = if self.first().token_type == TokenType::At {
                        let _ = self.eat();
                        Some(self.expect_potential_dollar_ident())
                    } else {
                        None
                    };
                    let value = match self.first().token_type {
                        TokenType::EOL
                        | TokenType::EOF
                        | TokenType::Comma
                        | TokenType::Close(_)
                        | TokenType::Open(Bracket::Curly) => None,
                        _ => Some(Box::new(self.parse_statement())),
                    };
                    Node::new(val.span, NodeType::Break { label, value })
                }
                StopValue::Continue => {
                    let val = self.eat();
                    let label = if self.first().token_type == TokenType::At {
                        let _ = self.eat();
                        Some(self.expect_potential_dollar_ident())
                    } else {
                        None
                    };
                    Node::new(val.span, NodeType::Continue { label })
                }
            },
            TokenType::String => {
                let val = self.eat();

                Node::new(val.span, NodeType::StringLiteral(val.into()))
            }
            TokenType::Char => {
                let val = self.eat();
                let mut chars = val.value.chars();
                let ch = chars.next().unwrap_or_else(|| {
                    self.add_err_at(SyntaxErr::InvalidLiteral(val.value.clone()), val.span);
                    '\0'
                });
                Node::new(val.span, NodeType::CharLiteral(ch))
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
                let value = self.parse_pipe_expression();
                Node::new(
                    Span::new_from_spans(open.span, value.span),
                    NodeType::MoveExpression {
                        value: Box::new(value),
                    },
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
            TokenType::For => self.parse_loop_declaration(),
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

                    let value = self.parse_statement();
                    properties.push((key.to_string(), value));

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

        if matches!(
            left.node_type,
            NodeType::Identifier(ref id) if id.to_string() == "comp"
        ) && self.first().token_type == TokenType::Comma
            && self.has_token_after_comma(TokenType::FatArrow)
        {
            while self.first().token_type == TokenType::Comma {
                let _ = self.eat();
                let _ = self.parse_pipe_expression();
            }

            if self.first().token_type == TokenType::FatArrow {
                let _ = self.eat();
                return self.parse_statement();
            }
        }

        if matches!(left.node_type, NodeType::Identifier(_))
            && self.first().token_type == TokenType::Comma
            && self.has_token_after_comma(TokenType::Equals)
        {
            let open = left.span;
            let mut bindings = vec![left.clone()];
            while self.first().token_type == TokenType::Comma {
                let _ = self.eat();
                let nxt = self.parse_pipe_expression();
                bindings.push(nxt.clone());
            }

            if self.first().token_type == TokenType::Equals {
                let _ = self.eat();
                let mut values = vec![self.parse_statement()];
                while self.first().token_type == TokenType::Comma {
                    let _ = self.eat();
                    values.push(self.parse_statement());
                }

                let value = if values.len() == 1 {
                    values
                        .pop()
                        .unwrap_or_else(|| Node::new(self.first().span, NodeType::Null))
                } else if let (Some(first), Some(last)) = (values.first(), values.last()) {
                    let span = Span::new_from_spans(first.span, last.span);
                    Node::new(span, NodeType::TupleLiteral { values })
                } else {
                    Node::new(self.first().span, NodeType::Null)
                };

                let mut bindings_out = Vec::new();
                for bind in bindings.into_iter() {
                    let NodeType::Identifier(ident) = bind.node_type else {
                        bindings_out.push(None);
                        continue;
                    };
                    bindings_out.push(Some((VarType::Immutable, ident.get_ident().clone())));
                }

                return Node::new(
                    open,
                    NodeType::DestructureAssignment {
                        pattern: DestructurePattern::Tuple(bindings_out),
                        value: Box::new(value),
                    },
                );
            }
        }

        if let NodeType::CallExpression {
            caller,
            args,
            string_fn: None,
            generic_types,
            reverse_args,
        } = &left.node_type
        {
            if generic_types.is_empty() && reverse_args.is_empty() {
                if let NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(id)) =
                    &caller.node_type
                {
                    if id.to_string() == "tuple" && self.first().token_type == TokenType::Equals {
                        let open = left.span;
                        let _ = self.eat();
                        let mut values = vec![self.parse_statement()];
                        while self.first().token_type == TokenType::Comma {
                            let _ = self.eat();
                            values.push(self.parse_statement());
                        }

                        let value = if values.len() == 1 {
                            values
                                .pop()
                                .unwrap_or_else(|| Node::new(self.first().span, NodeType::Null))
                        } else if let (Some(first), Some(last)) = (values.first(), values.last()) {
                            let span = Span::new_from_spans(first.span, last.span);
                            Node::new(
                                span,
                                NodeType::CallExpression {
                                    string_fn: None,
                                    generic_types: Vec::new(),
                                    caller: Box::new(Node::new(
                                        span,
                                        NodeType::Identifier(
                                            PotentialGenericTypeIdentifier::Identifier(
                                                ParserText::from(String::from("tuple")).into(),
                                            ),
                                        ),
                                    )),
                                    args: values.into_iter().map(CallArg::Value).collect(),
                                    reverse_args: Vec::new(),
                                },
                            )
                        } else {
                            Node::new(self.first().span, NodeType::Null)
                        };

                        let mut bindings_out = Vec::new();
                        for bind in args.iter() {
                            let CallArg::Value(Node {
                                node_type: NodeType::Identifier(ident),
                                ..
                            }) = bind
                            else {
                                bindings_out.push(None);
                                continue;
                            };
                            bindings_out
                                .push(Some((VarType::Immutable, ident.get_ident().clone())));
                        }

                        return Node::new(
                            open,
                            NodeType::DestructureAssignment {
                                pattern: DestructurePattern::Tuple(bindings_out),
                                value: Box::new(value),
                            },
                        );
                    }
                }
            }
        }

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
