use crate::{
    Parser, SyntaxErr,
    ast::{
        FunctionHeader, GenericType, GenericTypes, IfComparisonType, LoopType, MatchArmType,
        NamedScope, Node, ParserDataType, ParserInnerType, ParserText, PotentialDollarIdentifier,
        PotentialNewType, TryCatch, VarType, binary::BinaryOperator,
        comparison::ComparisonOperator,
    },
    lexer::{Bracket, Span, StopValue},
};
use crate::{ast::NodeType, lexer::TokenType};

impl Parser {
    pub fn parse_statement(&mut self) -> Node {
        let node = match &self.first().token_type {
            TokenType::Let | TokenType::Const => self.parse_variable_declaration(),
            TokenType::Trait => self.parse_if_statement(),
            TokenType::Impl => self.parse_impl_declaration(),
            TokenType::Import => self.parse_import_declaration(),
            TokenType::Type => self.parse_type_decaration(),
            TokenType::For => self.parse_loop_declaration(),
            _ => self.parse_assignment_expression(),
        };

        self.parse_potential_member(node)
    }

    pub fn expect_named_scope(&mut self) -> NamedScope {
        let _ = self.expect_eat(&TokenType::At, SyntaxErr::ExpectedChar('@'));

        let name = self.expect_potential_dollar_ident();

        let _ = self.expect_eat(
            &TokenType::Open(Bracket::Square),
            SyntaxErr::ExpectedOpeningBracket(Bracket::Square),
        );

        let mut args = Vec::new();

        while self.first().token_type != TokenType::Close(Bracket::Square) {
            let _ = self.expect_eat(&TokenType::Dollar, SyntaxErr::ExpectedChar('$'));
            let ident = self.expect_potential_dollar_ident();
            let _ = self.expect_eat(&TokenType::Equals, SyntaxErr::ExpectedChar('='));
            let value = self.parse_statement();

            if self.first().token_type == TokenType::Comma {
                let _ = self.eat();
            }

            args.push((ident, value));
        }
        let _ = self.expect_eat(
            &TokenType::Close(Bracket::Square),
            SyntaxErr::ExpectedClosingBracket(Bracket::Square),
        );
        NamedScope { name, args }
    }

    pub fn parse_scope_alias(&mut self) -> Node {
        let open = self.expect_eat(&TokenType::At, SyntaxErr::ExpectedChar('@'));
        let identifier = self.expect_potential_dollar_ident();
        let close = *identifier.span();

        let _ = self.expect_eat(
            &TokenType::FatArrow,
            SyntaxErr::ExpectedKeyword(String::from("=>")),
        );

        let value = self.expect_named_scope();

        let create_new_scope = if self.first().token_type == TokenType::Open(Bracket::Curly) {
            let _ = self.expect_eat(
                &TokenType::Open(Bracket::Curly),
                SyntaxErr::ExpectedOpeningBracket(Bracket::Curly),
            );

            let create_new_scope = if self.first().token_type == TokenType::Open(Bracket::Curly) {
                let _ = self.eat();
                false
            } else {
                true
            };

            let _ = self.expect_eat(
                &TokenType::Close(Bracket::Curly),
                SyntaxErr::ExpectedClosingBracket(Bracket::Curly),
            );

            if !create_new_scope {
                self.expect_eat(
                    &TokenType::Close(Bracket::Curly),
                    SyntaxErr::ExpectedClosingBracket(Bracket::Curly),
                );
            }

            Some(create_new_scope)
        } else {
            None
        };

        Node {
            node_type: NodeType::ScopeAlias {
                identifier,
                value,
                create_new_scope,
            },
            span: Span::new_from_spans(open.span, close),
        }
    }

    pub fn parse_scope_declaration(&mut self, define: bool) -> Node {
        let open = self.expect_eat(
            &TokenType::FatArrow,
            SyntaxErr::ExpectedKeyword(String::from("=>")),
        );

        let named = if self.first().token_type == TokenType::At {
            Some(self.expect_named_scope())
        } else {
            None
        };

        if self.first().token_type == TokenType::EOL {
            let close = self.eat();

            Node::new(
                Span::new_from_spans(open.span, close.span),
                NodeType::ScopeDeclaration {
                    body: None,
                    is_temp: true,
                    create_new_scope: None,
                    define,
                    named,
                },
            )
        } else if self.first().token_type == TokenType::Open(Bracket::Curly) {
            let open = self.expect_eat(
                &TokenType::Open(Bracket::Curly),
                SyntaxErr::ExpectedOpeningBracket(Bracket::Curly),
            );

            let create_new_scope = if self.first().token_type == TokenType::Open(Bracket::Curly) {
                let _ = self.eat();
                false
            } else {
                true
            };

            let mut body: Vec<Node> = Vec::new();

            while ![TokenType::EOF, TokenType::Close(Bracket::Curly)]
                .contains(&self.first().token_type)
            {
                body.push(self.parse_statement());
                let _ = self.parse_delimited();
            }

            let body: Vec<Node> = body
                .into_iter()
                .filter(|x| x.node_type != NodeType::EmptyLine)
                .collect();

            let close = self.expect_eat(
                &TokenType::Close(Bracket::Curly),
                SyntaxErr::ExpectedClosingBracket(Bracket::Curly),
            );

            let close = if !create_new_scope {
                self.expect_eat(
                    &TokenType::Close(Bracket::Curly),
                    SyntaxErr::ExpectedClosingBracket(Bracket::Curly),
                )
            } else {
                close
            };

            Node::new(
                Span::new_from_spans(open.span, close.span),
                NodeType::ScopeDeclaration {
                    body: Some(body),
                    is_temp: true,
                    create_new_scope: Some(create_new_scope),
                    define,
                    named,
                },
            )
        } else {
            let body = vec![self.parse_statement()];
            let close = body.last().unwrap().span.clone();

            Node::new(
                Span::new_from_spans(open.span, close),
                NodeType::ScopeDeclaration {
                    body: Some(body),
                    is_temp: true,
                    create_new_scope: Some(false),
                    define,
                    named,
                },
            )
        }
    }

    pub fn parse_variable_declaration(&mut self) -> Node {
        let var_type = match self.eat().token_type {
            TokenType::Const => VarType::Constant,
            TokenType::Let => {
                if let TokenType::Mut = self.first().token_type {
                    let _ = self.eat();
                    VarType::Mutable
                } else {
                    if self.first().token_type == TokenType::FatArrow {
                        return self.parse_scope_declaration(true);
                    } else if self.first().token_type == TokenType::At {
                        return self.parse_scope_alias();
                    }
                    VarType::Immutable
                }
            }
            _ => {
                self.add_err(SyntaxErr::UnexpectedToken);
                VarType::Constant
            }
        };

        let identifier = self.expect_potential_dollar_ident();

        let data_type = match self.first().token_type {
            TokenType::Colon => {
                let _ = self.eat();
                self.expect_potential_new_type()
            }
            _ => PotentialNewType::DataType(ParserDataType::from(ParserInnerType::Auto(None))),
        };

        let _ = self.expect_eat(&TokenType::Equals, SyntaxErr::ExpectedChar('='));

        let value = self.parse_statement();

        Node::new(
            *identifier.span(),
            NodeType::VariableDeclaration {
                var_type,
                identifier: identifier.into(),
                data_type,
                value: Box::new(value),
            },
        )
    }

    pub fn is_first_potential_call(&self) -> bool {
        match self.first().token_type {
            TokenType::Open(Bracket::Paren) | TokenType::ColonAngled | TokenType::String => true,
            _ => false,
        }
    }

    pub fn parse_generic_types(&mut self) -> Vec<PotentialNewType> {
        if self.first().token_type != TokenType::ColonAngled {
            return Vec::new();
        }

        let _ = self.expect_eat(
            &TokenType::ColonAngled,
            SyntaxErr::ExpectedKeyword(":<".to_string()),
        );

        let mut types = Vec::new();

        while self.first().token_type != TokenType::Comparison(ComparisonOperator::Greater) {
            types.push(self.expect_potential_new_type());

            if self.first().token_type != TokenType::Comparison(ComparisonOperator::Greater) {
                let _ = self.expect_eat(&TokenType::Comma, SyntaxErr::ExpectedChar(','));
            }
        }

        let _ = self.expect_eat(
            &TokenType::Comparison(ComparisonOperator::Greater),
            SyntaxErr::ExpectedChar('>'),
        );

        types
    }

    pub fn parse_generic_types_with_constraints(&mut self) -> GenericTypes {
        if self.first().token_type != TokenType::Comparison(ComparisonOperator::Lesser) {
            return GenericTypes::default();
        }
        let _ = self.expect_eat(
            &TokenType::Comparison(ComparisonOperator::Lesser),
            SyntaxErr::ExpectedChar('<'),
        );

        let mut types = Vec::new();

        while self.first().token_type != TokenType::Comparison(ComparisonOperator::Greater) {
            let identifier = self.parse_potential_dollar_ident().unwrap();
            let mut constraints = Vec::new();

            if self.first().token_type == TokenType::Colon {
                let _ = self.eat();

                constraints.push(self.expect_potential_dollar_ident());

                while self.first().token_type == TokenType::BinaryOperator(BinaryOperator::Add) {
                    constraints.push(self.expect_potential_dollar_ident());
                }
            }

            if self.first().token_type != TokenType::Comparison(ComparisonOperator::Greater) {
                let _ = self.expect_eat(&TokenType::Comma, SyntaxErr::ExpectedChar(','));
            }

            types.push(GenericType {
                identifier,
                trait_constraints: constraints,
            });
        }

        let _ = self.expect_eat(
            &TokenType::Comparison(ComparisonOperator::Greater),
            SyntaxErr::ExpectedChar('>'),
        );

        GenericTypes(types)
    }

    pub fn parse_impl_declaration(&mut self) -> Node {
        let _ = self.expect_eat(
            &TokenType::Impl,
            SyntaxErr::ExpectedKeyword(String::from("impl")),
        );

        let identifier = self.expect_potential_dollar_ident();

        let mut variables = Vec::new();

        let _ = self.expect_eat(
            &TokenType::Open(Bracket::Curly),
            SyntaxErr::ExpectedOpeningBracket(Bracket::Curly),
        );

        while self.first().token_type != TokenType::Close(Bracket::Curly) {
            let decl = self.parse_variable_declaration();

            let _ = self.parse_delimited();

            match &decl.node_type {
                NodeType::VariableDeclaration {
                    var_type: VarType::Constant,
                    ..
                } => {
                    variables.push(decl);
                }
                _ => self.add_err(SyntaxErr::ExpectedFunctions),
            }
        }

        let _ = self.expect_eat(
            &TokenType::Close(Bracket::Curly),
            SyntaxErr::ExpectedClosingBracket(Bracket::Curly),
        );

        Node::new(
            *identifier.span(),
            NodeType::ImplDeclaration {
                identifier: identifier.into(),
                variables,
            },
        )
    }

    pub fn parse_try_expression(&mut self) -> Node {
        if self.first().token_type == TokenType::Try {
            let open = self.expect_eat(
                &TokenType::Try,
                SyntaxErr::ExpectedKeyword(String::from("try")),
            );

            let val = self.parse_statement();

            let mut span = Span::new_from_spans(open.span, val.span);

            let catch = if self.first().token_type == TokenType::Colon
                || self.first().token_type == TokenType::FatArrow
            {
                let name = if self.first().token_type == TokenType::Colon {
                    let _ = self.eat();

                    Some(self.expect_potential_dollar_ident())
                } else {
                    None
                };

                let block = self.parse_scope_declaration(false);

                span = Span::new_from_spans(span, block.span);

                Some(TryCatch {
                    name,
                    body: Box::new(block),
                })
            } else {
                None
            };

            Node::new(
                span,
                NodeType::Try {
                    value: Box::new(val),
                    catch,
                },
            )
        } else {
            self.parse_range_expression()
        }
    }

    pub fn parse_import_declaration(&mut self) -> Node {
        let open = self.expect_eat(
            &TokenType::Import,
            SyntaxErr::ExpectedKeyword(String::from("import")),
        );

        let get_module = |this: &mut Parser| -> Vec<PotentialDollarIdentifier> {
            let mut module = vec![this.expect_potential_dollar_ident()];

            while this.first().token_type == TokenType::Colon {
                module.push(this.expect_potential_dollar_ident());
            }

            module
        };

        if [
            TokenType::Open(Bracket::Paren),
            TokenType::BinaryOperator(BinaryOperator::Mul),
        ]
        .contains(&self.first().token_type)
        {
            let values = if TokenType::Open(Bracket::Paren) == self.first().token_type {
                let _ = self.expect_eat(
                    &TokenType::Open(Bracket::Paren),
                    SyntaxErr::ExpectedOpeningBracket(Bracket::Paren),
                );

                let mut values = Vec::new();

                while self.first().token_type != TokenType::Close(Bracket::Paren) {
                    values.push(self.expect_potential_dollar_ident());
                    if self.first().token_type == TokenType::Comma {
                        let _ = self.eat();
                    }
                }

                let _ = self.expect_eat(
                    &TokenType::Close(Bracket::Paren),
                    SyntaxErr::ExpectedClosingBracket(Bracket::Paren),
                );
                values
            } else if self.first().token_type == TokenType::BinaryOperator(BinaryOperator::Mul) {
                let value = self.eat();
                vec![PotentialDollarIdentifier::Identifier(ParserText::from(
                    value,
                ))]
            } else {
                vec![self.expect_potential_dollar_ident()]
            };

            let _ = self.expect_eat(
                &TokenType::From,
                SyntaxErr::ExpectedKeyword(String::from("from")),
            );

            let module = get_module(self);
            return Node::new(
                open.span,
                NodeType::ImportStatement {
                    module,
                    alias: None,
                    values,
                },
            );
        }

        let module = get_module(self);

        let alias = if self.first().token_type == TokenType::As {
            let _ = self.eat();
            Some(self.expect_potential_dollar_ident())
        } else {
            None
        };

        Node::new(
            open.span,
            NodeType::ImportStatement {
                module,
                alias,
                values: Vec::new(),
            },
        )
    }

    pub fn parse_return_declaration(&mut self) -> Node {
        let open = self.expect_eat(
            &TokenType::Stop(StopValue::Return),
            SyntaxErr::ExpectedKeyword(String::from("return")),
        );

        let mut span = open.span;

        let val = if self.first().token_type == TokenType::EOL {
            None
        } else {
            let val = self.parse_statement();
            span = Span::new_from_spans(span, val.span);
            Some(Box::new(val))
        };
        Node::new(span, NodeType::Return { value: val })
    }

    pub fn parse_match_arm_start(&mut self, parse_name: bool) -> MatchArmType {
        match self.first().token_type {
            TokenType::FullStop => {
                let _ = self.eat();

                let variant = self.expect_potential_dollar_ident();

                let name = if self.first().token_type == TokenType::Colon && parse_name {
                    let _ = self.eat();
                    let typ = self.parse_match_var_type();
                    let name = self.expect_potential_dollar_ident();
                    (typ, Some(name))
                } else {
                    (VarType::Immutable, None)
                };

                MatchArmType::Enum {
                    value: variant,
                    var_type: name.0,
                    name: name.1,
                }
            }
            TokenType::Let | TokenType::Const => {
                let var_type = match self.eat().token_type {
                    TokenType::Const => VarType::Constant,
                    TokenType::Let => {
                        if let TokenType::Mut = self.first().token_type {
                            let _ = self.eat();
                            VarType::Mutable
                        } else {
                            VarType::Immutable
                        }
                    }
                    _ => {
                        self.add_err(SyntaxErr::UnexpectedToken);
                        VarType::Constant
                    }
                };

                let name = self.expect_potential_dollar_ident();

                MatchArmType::Let { var_type, name }
            }
            _ if &self.first().value == "_" => {
                let all = self.eat();

                MatchArmType::Wildcard(all.span)
            }
            _ => MatchArmType::Value(self.parse_statement()),
        }
    }

    pub fn parse_match_patterns(&mut self) -> Vec<(MatchArmType, Vec<Node>, Box<Node>)> {
        let mut patterns = Vec::new();

        while self.first().token_type != TokenType::Close(Bracket::Curly) {
            let mut values = vec![self.parse_match_arm_start(false)];
            let mut conditions = Vec::new();

            while self.first().token_type == TokenType::Or {
                let _ = self.eat();
                values.push(self.parse_match_arm_start(false));
            }

            if self.first().token_type == TokenType::Colon {
                let _ = self.eat();
                let _typ = self.parse_match_var_type();
                let n = self.expect_potential_dollar_ident();
                for val in values.iter_mut() {
                    match val {
                        MatchArmType::Enum {
                            value: _,
                            var_type: _typ,
                            name,
                        } => *name = Some(n.clone()),
                        _ => self.add_err(SyntaxErr::ExpectedIdentifier),
                    }
                }
            }

            while self.first().token_type == TokenType::If {
                let _ = self.eat();
                conditions.push(self.parse_statement());
            }

            let body = Box::new(self.parse_scope_declaration(false));

            for value in values {
                patterns.push((value, conditions.clone(), body.clone()));
            }

            if self.first().token_type == TokenType::Comma {
                let _ = self.eat();
            }
        }
        patterns
    }

    pub fn parse_match_statement(&mut self) -> Node {
        let open = self.expect_eat(
            &TokenType::Match,
            SyntaxErr::ExpectedKeyword(String::from("match")),
        );

        let value = if self.first().token_type == TokenType::Open(Bracket::Curly) {
            None
        } else {
            Some(Box::new(self.parse_statement()))
        };

        let _ = self.expect_eat(
            &TokenType::Open(Bracket::Curly),
            SyntaxErr::ExpectedOpeningBracket(Bracket::Curly),
        );

        let patterns = self.parse_match_patterns();

        let close = self.expect_eat(
            &TokenType::Close(Bracket::Curly),
            SyntaxErr::ExpectedClosingBracket(Bracket::Curly),
        );

        Node::new(
            Span::new_from_spans(open.span, close.span),
            NodeType::MatchStatement {
                value,
                body: patterns,
            },
        )
    }

    pub fn parse_fnmatch_declaration(&mut self) -> Node {
        let open = self.expect_eat(
            &TokenType::Match,
            SyntaxErr::ExpectedKeyword(String::from("match")),
        );

        let is_async = self.first().token_type == TokenType::Async;

        if is_async {
            let _ = self.eat();
        };

        let generic_types = self.parse_generic_types_with_constraints();

        let typ = if self.first().token_type != TokenType::Open(Bracket::Curly) {
            self.parse_potential_new_type()
        } else {
            None
        };

        let return_type = if self.first().token_type == TokenType::Open(Bracket::Curly) {
            ParserDataType::from(ParserInnerType::Null).into()
        } else {
            let _ = self.expect_eat(
                &TokenType::Arrow,
                SyntaxErr::ExpectedKeyword(String::from("->")),
            );
            self.parse_potential_new_type()
                .unwrap_or(ParserDataType::from(ParserInnerType::Null).into())
        };

        let _ = self.expect_eat(
            &TokenType::Open(Bracket::Curly),
            SyntaxErr::ExpectedOpeningBracket(Bracket::Curly),
        );

        let patterns = self.parse_match_patterns();

        let close = self.expect_eat(
            &TokenType::Close(Bracket::Curly),
            SyntaxErr::ExpectedClosingBracket(Bracket::Curly),
        );

        let func = Node::new(
            Span::new_from_spans(open.span, close.span),
            NodeType::FnMatchDeclaration {
                header: FunctionHeader {
                    generics: generic_types,
                    parameters: vec![(
                        ParserText::new(Span::default(), String::from("input_value")).into(),
                        typ.unwrap_or(
                            ParserDataType::new(Span::default(), ParserInnerType::Auto(None))
                                .into(),
                        ),
                    )],
                    return_type,
                    is_async,
                },
                body: patterns,
            },
        );

        if self.first().token_type == TokenType::Open(Bracket::Paren) {
            self.parse_call_expression(func)
        } else {
            func
        }
    }

    pub fn get_loop_type(&mut self) -> LoopType {
        if self.first().token_type == TokenType::Let {
            let expr = self.parse_let_pattern();

            return LoopType::Let {
                value: expr.0,
                pattern: (expr.1, expr.2),
            };
        }

        if let Some(in_token) = self.nth(1) {
            if self.first().token_type == TokenType::Identifier
                && in_token.token_type == TokenType::In
            {
                let identifier = self.expect_potential_dollar_ident();

                let _ = self.eat();
                return LoopType::For(identifier, self.parse_statement());
            }
        }

        let task = self.parse_statement();
        LoopType::While(task)
    }

    pub fn parse_loop_declaration(&mut self) -> Node {
        let open = self.expect_eat(
            &TokenType::For,
            SyntaxErr::ExpectedKeyword(String::from("for")),
        );

        let typ = if self.first().token_type == TokenType::FatArrow {
            LoopType::Loop
        } else {
            self.get_loop_type()
        };
        let block = self.parse_scope_declaration(false);

        let until = if self.first().token_type == TokenType::Stop(StopValue::Until) {
            let _ = self.eat();
            Some(Box::new(self.parse_statement()))
        } else {
            None
        };

        Node::new(
            Span::new_from_spans(open.span, block.span),
            NodeType::LoopDeclaration {
                loop_type: Box::new(typ),
                body: Box::new(block),
                until,
            },
        )
    }

    pub fn parse_function_declaration(&mut self) -> Node {
        let open = self.expect_eat(
            &TokenType::Func,
            SyntaxErr::ExpectedKeyword(String::from("fn")),
        );

        if self.first().token_type == TokenType::Match {
            return self.parse_fnmatch_declaration();
        }

        let is_async = self.first().token_type == TokenType::Async;

        if is_async {
            let _ = self.eat();
        }

        let generic_types = self.parse_generic_types_with_constraints();

        let parameters = self.parse_key_type_list_ordered_with_ref(
            TokenType::Open(Bracket::Paren),
            TokenType::Close(Bracket::Paren),
        );

        let return_type = if self.first().token_type == TokenType::FatArrow {
            ParserDataType::from(ParserInnerType::Auto(None)).into()
        } else {
            let _ = self.expect_eat(
                &TokenType::Arrow,
                SyntaxErr::ExpectedKeyword(String::from("->")),
            );
            self.parse_potential_new_type()
                .unwrap_or(ParserDataType::from(ParserInnerType::Null).into())
        };

        let block = self.parse_scope_declaration(false);

        let func = Node::new(
            Span::new_from_spans(open.span, block.span),
            NodeType::FunctionDeclaration {
                header: FunctionHeader {
                    generics: generic_types,
                    parameters,
                    return_type,
                    is_async,
                },
                body: Box::new(block),
            },
        );

        if self.first().token_type == TokenType::Open(Bracket::Paren) {
            self.parse_call_expression(func)
        } else {
            func
        }
    }

    pub fn parse_match_var_type(&mut self) -> VarType {
        let typ = match &self.first().token_type {
            TokenType::Mut => VarType::Mutable,
            TokenType::Const => VarType::Constant,
            TokenType::Let => VarType::Immutable,
            _ => return VarType::Immutable,
        };

        let _ = self.eat();

        typ
    }

    pub fn parse_let_pattern(&mut self) -> (Node, Vec<MatchArmType>, Vec<Node>) {
        let _ = self.expect_eat(&TokenType::Let, SyntaxErr::ExpectedToken(TokenType::Let));

        let mut values = vec![self.parse_match_arm_start(false)];
        let mut conditions = Vec::new();

        while self.first().token_type == TokenType::Or {
            let _ = self.eat();
            values.push(self.parse_match_arm_start(false));
        }

        if self.first().token_type == TokenType::Colon {
            let _ = self.eat();
            let _typ = self.parse_match_var_type();
            let n = self.expect_potential_dollar_ident();
            for val in values.iter_mut() {
                match val {
                    MatchArmType::Enum {
                        value: _,
                        var_type: _typ,
                        name,
                    } => *name = Some(n.clone()),
                    _ => self.add_err(SyntaxErr::ExpectedIdentifier),
                }
            }
        }

        while self.first().token_type == TokenType::If {
            let _ = self.eat();
            conditions.push(self.parse_statement());
        }

        let _ = self.expect_eat(
            &TokenType::LeftArrow,
            SyntaxErr::ExpectedKeyword(String::from("<-")),
        );

        let value = self.parse_statement();

        (value, values, conditions)
    }

    pub fn parse_if_statement(&mut self) -> Node {
        let open = self.expect_eat(
            &TokenType::If,
            SyntaxErr::ExpectedKeyword(String::from("if")),
        );

        let comparison = if self.first().token_type == TokenType::Let {
            let expr = self.parse_let_pattern();

            IfComparisonType::IfLet {
                value: expr.0,
                pattern: (expr.1, expr.2),
            }
        } else {
            IfComparisonType::If(self.parse_statement())
        };

        let then = Box::new(self.parse_scope_declaration(false));

        let otherwise = if self.first().token_type == TokenType::Else {
            let _ = self.eat();
            if self.first().token_type == TokenType::If {
                Some(Box::new(self.parse_if_statement()))
            } else {
                Some(Box::new(self.parse_scope_declaration(false)))
            }
        } else {
            None
        };

        Node::new(
            Span::new_from_spans(
                open.span,
                match otherwise.clone() {
                    Some(x) => x.span,
                    None => then.span,
                },
            ),
            NodeType::IfStatement {
                comparison: Box::new(comparison),
                then,
                otherwise,
            },
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::Parser;
    use crate::ast::NodeType;
    use crate::ast::VarType;
    use crate::lexer::{Token, tokenize};

    fn parser_with_tokens(tokens: Vec<Token>) -> Parser {
        Parser { tokens }
    }

    #[test]
    fn test_parse_variable_declaration_let() {
        let tokens = tokenize(String::from("let x = 42")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_variable_declaration().unwrap();
        match node {
            NodeType::VariableDeclaration {
                var_type,
                identifier,
                ..
            } => {
                assert_eq!(identifier, "x");
                assert!(matches!(var_type, VarType::Immutable));
            }
            _ => panic!("Expected VariableDeclaration"),
        }
    }

    #[test]
    fn test_parse_variable_declaration_let_mut() {
        let tokens = tokenize(String::from("let mut x = 42")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_variable_declaration().unwrap();
        match node {
            NodeType::VariableDeclaration {
                var_type,
                identifier,
                ..
            } => {
                assert_eq!(identifier, "x");
                assert!(matches!(var_type, VarType::Mutable));
            }
            _ => panic!("Expected VariableDeclaration"),
        }
    }

    #[test]
    fn test_parse_variable_declaration_const() {
        let tokens = tokenize(String::from("const y = 3.14")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_variable_declaration().unwrap();
        match node {
            NodeType::VariableDeclaration {
                var_type,
                identifier,
                ..
            } => {
                assert_eq!(identifier, "y");
                assert!(matches!(var_type, VarType::Constant));
            }
            _ => panic!("Expected VariableDeclaration"),
        }
    }

    #[test]
    fn test_parse_function_declaration_simple() {
        let tokens = tokenize(String::from("fn(x: int, y: int) => return x + y ")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_function_declaration().unwrap();
        match node {
            NodeType::FunctionDeclaration { parameters, .. } => {
                assert_eq!(parameters.len(), 2);
            }
            _ => panic!("Expected FunctionDeclaration"),
        }
    }

    #[test]
    fn test_parse_loop_declaration_for() {
        let tokens = tokenize(String::from("for i in xs => x = x + 1")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_loop_declaration().unwrap();
        match node {
            NodeType::LoopDeclaration { .. } => {}
            _ => panic!("Expected LoopDeclaration"),
        }
    }

    #[test]
    fn test_parse_return_declaration() {
        let tokens = tokenize(String::from("return 123")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_return_declaration().unwrap();
        match node {
            NodeType::Return { .. } => {}
            _ => panic!("Expected Return"),
        }
    }
}
