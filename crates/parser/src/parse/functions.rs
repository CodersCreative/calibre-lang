use crate::ast::{CallArg, Node, ParserDataType, ParserInnerType, PotentialNewType};
use crate::lexer::{Bracket, Span, Tokenizer};
use crate::{Parser, SyntaxErr};
use crate::{ast::NodeType, lexer::TokenType};

fn parse_splits(input: &str) -> (Vec<String>, Vec<String>) {
    let mut normal_parts = Vec::new();
    let mut extracted_parts = Vec::new();

    let mut current_buffer = String::new();
    let mut chars = input.chars().peekable();

    while let Some(c) = chars.next() {
        match c {
            '{' => {
                if chars.peek() == Some(&'{') {
                    current_buffer.push('{');
                    chars.next();
                } else {
                    normal_parts.push(current_buffer.clone());
                    current_buffer.clear();

                    let mut extracted = String::new();
                    while let Some(&next_c) = chars.peek() {
                        if next_c == '}' {
                            chars.next();
                            break;
                        }
                        extracted.push(chars.next().unwrap());
                    }
                    extracted_parts.push(extracted);
                }
            }
            '}' => {
                if chars.peek() == Some(&'}') {
                    current_buffer.push('}');
                    chars.next();
                } else {
                    current_buffer.push('}');
                }
            }
            _ => current_buffer.push(c),
        }
    }

    normal_parts.push(current_buffer);

    (normal_parts, extracted_parts)
}

impl Parser {
    pub fn parse_call_member_expression(&mut self) -> Node {
        let member = self.parse_scope_member_expression();

        if self.is_first_potential_call() {
            return self.parse_call_expression(member);
        }

        member
    }

    pub fn parse_call_expression(&mut self, caller: Node) -> Node {
        let generic_types = self.parse_generic_types();

        let mut string_fn = None;

        let args = if self.first().token_type == TokenType::String {
            let val = self.eat();
            string_fn = Some(val.clone().into());
            let mut tokenizer = Tokenizer::new(false);
            let (texts, args) = parse_splits(&val.value);

            let mut parsed_args = vec![CallArg::Value(Node {
                node_type: NodeType::ListLiteral(
                    PotentialNewType::DataType(ParserDataType::from(ParserInnerType::Str)),
                    {
                        let mut txts = Vec::new();

                        for txt in texts {
                            txts.push(Node::new_from_type(NodeType::StringLiteral(txt.into())));
                        }

                        txts
                    },
                ),
                span: Span::default(),
            })];

            let args = args
                .into_iter()
                .map(|x| tokenizer.tokenize(x).unwrap())
                .collect::<Vec<_>>();

            let mut original = Vec::new();
            original.append(&mut self.tokens);

            self.tokens.clear();

            for arg in args {
                self.tokens = arg;
                parsed_args.push(CallArg::Value(self.parse_statement()));
            }

            self.tokens = original;

            parsed_args
        } else {
            self.parse_arguments(
                TokenType::Open(Bracket::Paren),
                TokenType::Close(Bracket::Paren),
            )
        };

        let mut expression = Node::new(
            caller.span,
            NodeType::CallExpression {
                string_fn,
                caller: Box::new(caller),
                generic_types,
                args,
            },
        );

        if self.first().token_type == TokenType::Open(Bracket::Paren) {
            expression = self.parse_call_expression(expression);
        }

        expression
    }

    pub fn parse_purely_member(&mut self) -> Node {
        let first = self.expect_potential_generic_type_ident();

        let mut path = vec![(
            Node {
                node_type: NodeType::Identifier(first.clone()),
                span: *first.span(),
            },
            false,
        )];

        while self.first().token_type == TokenType::FullStop
            || self.first().token_type == TokenType::Open(Bracket::Square)
        {
            if self.eat().token_type == TokenType::FullStop {
                let first = self.expect_potential_dollar_ident();
                path.push((
                    Node {
                        node_type: NodeType::Identifier(first.clone().into()),
                        span: *first.span(),
                    },
                    false,
                ));
            } else {
                path.push((self.parse_statement(), true));

                self.expect_eat(
                    &TokenType::Close(Bracket::Square),
                    SyntaxErr::ExpectedClosingBracket(Bracket::Square),
                );
            }
        }

        if path.len() <= 1 {
            path.remove(0).0
        } else {
            let first = Span::new_from_spans(path[0].0.span, path.last().unwrap().0.span);
            Node::new(first, NodeType::MemberExpression { path: path })
        }
    }

    pub fn parse_scope_member_expression(&mut self) -> Node {
        let mut path: Vec<Node> = vec![self.parse_member_expression(None)];

        if let NodeType::Identifier(_) = path[0].node_type {
            while self.first().token_type == TokenType::DoubleColon {
                let _ = self.eat();
                path.push(self.parse_member_expression(None));
            }
        }

        if path.len() <= 1 {
            path.remove(0)
        } else {
            let first = Span::new_from_spans(path[0].span, path.last().unwrap().span);
            Node::new(first, NodeType::ScopeMemberExpression { path: path })
        }
    }

    pub fn parse_potential_member(&mut self, node: Node) -> Node {
        if !self.is_eof()
            && self.first().token_type == TokenType::FullStop
            && !self.first().value.contains(" ")
        {
            self.parse_member_expression(Some(node))
        } else if !self.is_eof() && self.is_first_potential_call() {
            self.parse_call_expression(node)
        } else {
            node
        }
    }

    pub fn parse_member_expression(&mut self, start: Option<Node>) -> Node {
        let mut path: Vec<(Node, bool)> = if let Some(start) = start {
            vec![(start, false)]
        } else {
            vec![(self.parse_primary_expression(), false)]
        };

        while self.first().token_type == TokenType::FullStop
            || self.first().token_type == TokenType::Open(Bracket::Square)
        {
            path.push(if self.eat().token_type == TokenType::FullStop {
                let mut prop = self.parse_primary_expression();

                if self.is_first_potential_call() {
                    prop = self.parse_call_expression(prop);
                }

                (prop, false)
            } else {
                let prop = self.parse_statement();

                let _ = self.expect_eat(
                    &TokenType::Close(Bracket::Square),
                    SyntaxErr::ExpectedClosingBracket(Bracket::Square),
                );

                (prop, true)
            });
        }

        if path.len() == 2 && !path[1].1 {
            if let NodeType::Identifier(identifier) = &path[0].0.node_type {
                match &path[1].0.node_type {
                    NodeType::Identifier(value) => {
                        if self.first().token_type == TokenType::Colon {
                            let _ = self.eat();
                            let data = self.parse_statement();
                            return Node::new(
                                Span::new_from_spans(path[0].0.span, path[1].0.span),
                                NodeType::EnumExpression {
                                    identifier: identifier.clone(),
                                    value: value.clone().into(),
                                    data: Some(Box::new(data)),
                                },
                            );
                        }
                    }
                    _ => {}
                }
            }
        }

        if path.len() == 1 {
            if let NodeType::Identifier(identifier) = &path[0].0.node_type {
                if self.first().token_type == TokenType::Open(Bracket::Curly) {
                    let data = self.parse_potential_key_value();
                    return Node::new(
                        path[0].0.span,
                        NodeType::StructLiteral {
                            identifier: identifier.clone(),
                            value: data,
                        },
                    );
                }
            }
        }

        if path.len() <= 1 {
            path.remove(0).0
        } else {
            let first = Span::new_from_spans(path[0].0.span, path.last().unwrap().0.span);
            Node::new(first, NodeType::MemberExpression { path: path })
        }
    }

    pub fn parse_arguments(
        &mut self,
        open_token: TokenType,
        close_token: TokenType,
    ) -> Vec<CallArg> {
        let _ = self.expect_eat(&open_token, SyntaxErr::ExpectedToken(open_token.clone()));

        let args = if self.first().token_type == close_token {
            Vec::new()
        } else {
            self.parse_arguments_list()
        };

        let _ = self.expect_eat(&close_token, SyntaxErr::ExpectedToken(close_token.clone()));

        args
    }

    pub fn parse_arguments_list(&mut self) -> Vec<CallArg> {
        let mut defaulted = false;

        macro_rules! parse_arg {
            () => {{
                if let Some(equals) = self.nth(1) {
                    if defaulted || equals.token_type == TokenType::Equals {
                        CallArg::Named(self.expect_potential_dollar_ident(), {
                            let _ =
                                self.expect_eat(&TokenType::Equals, SyntaxErr::ExpectedChar('='));
                            defaulted = true;
                            self.parse_statement()
                        })
                    } else {
                        CallArg::Value(self.parse_statement())
                    }
                } else {
                    if defaulted {
                        self.add_err(SyntaxErr::ExpectedChar('='))
                    }

                    CallArg::Value(self.parse_statement())
                }
            }};
        }

        let mut args = vec![parse_arg!()];

        while self.first().token_type == TokenType::Comma {
            let _ = self.eat();

            args.push(parse_arg!());

            if [
                TokenType::Close(Bracket::Curly),
                TokenType::Close(Bracket::Paren),
            ]
            .contains(&self.first().token_type)
            {
                break;
            }
        }

        args
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
            NodeType::MemberExpression { path } => {
                for (node, _) in path {
                    match node {
                        NodeType::CallExpression(_, _) | NodeType::Identifier(_) => {}
                        _ => panic!("Expected nested CallExpression or Identifier"),
                    }
                }
            }
            _ => panic!("Expected MemberExpression"),
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
            NodeType::MemberExpression { path } => {
                assert!(path[1].1)
            }
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
