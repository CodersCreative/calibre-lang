use crate::{
    Parser, SyntaxErr,
    ast::{
        DestructurePattern, FunctionHeader, GenericType, GenericTypes, IfComparisonType, LoopType,
        MatchArmType, NamedScope, Node, ParserDataType, ParserInnerType, ParserText,
        PotentialDollarIdentifier, PotentialFfiDataType, PotentialGenericTypeIdentifier,
        PotentialNewType, TryCatch, TypeDefType, VarType, binary::BinaryOperator,
        comparison::ComparisonOperator,
    },
    lexer::{Bracket, Span, StopValue},
};
use crate::{ast::NodeType, lexer::TokenType};

impl Parser {
    pub fn parse_statement(&mut self) -> Node {
        let node = match &self.first().token_type {
            TokenType::Let | TokenType::Const => self.parse_variable_declaration(),
            TokenType::Trait => self.parse_trait_declaration(),
            TokenType::Impl => self.parse_impl_declaration(),
            TokenType::Import => self.parse_import_declaration(),
            TokenType::Type => self.parse_type_decaration(),
            TokenType::Spawn => self.parse_spawn_statement(),
            TokenType::Use => self.parse_use_declaration(),
            TokenType::Select => self.parse_select_declaration(),
            TokenType::Extern => self.parse_extern_function_declaration(),
            _ => self.parse_assignment_expression(),
        };

        self.parse_potential_member(node)
    }

    pub fn parse_spawn_statement(&mut self) -> Node {
        let open = self.expect_eat(
            &TokenType::Spawn,
            SyntaxErr::ExpectedKeyword(String::from("spawn")),
        );

        if self.first().token_type == TokenType::Open(Bracket::Curly) {
            let _ = self.eat();
            let mut items = Vec::new();
            while self.first().token_type != TokenType::Close(Bracket::Curly) && !self.is_eof() {
                let item = if self.first().token_type == TokenType::FatArrow {
                    self.parse_scope_declaration(false)
                } else if self.first().token_type == TokenType::Func {
                    let node = self.parse_function_declaration();
                    self.parse_potential_member(node)
                } else if self.first().token_type == TokenType::For {
                    self.parse_loop_declaration()
                } else if self.first().token_type == TokenType::Spawn {
                    self.parse_spawn_statement()
                } else {
                    self.parse_call_member_expression()
                };
                if self.first().token_type == TokenType::Comma {
                    let _ = self.eat();
                } else {
                    let _ = self.parse_delimited();
                }
                items.push(item);
            }
            let close = self.expect_eat(
                &TokenType::Close(Bracket::Curly),
                SyntaxErr::ExpectedClosingBracket(Bracket::Curly),
            );
            return Node::new(
                Span::new_from_spans(open.span, close.span),
                NodeType::SpawnBlock { items },
            );
        }

        let value = if self.first().token_type == TokenType::FatArrow {
            self.parse_scope_declaration(false)
        } else if self.first().token_type == TokenType::Func {
            let node = self.parse_function_declaration();
            self.parse_potential_member(node)
        } else if self.first().token_type == TokenType::For {
            self.parse_loop_declaration()
        } else {
            self.parse_call_member_expression()
        };

        Node::new(
            Span::new_from_spans(open.span, value.span),
            NodeType::Spawn {
                value: Box::new(value),
            },
        )
    }

    pub fn parse_use_declaration(&mut self) -> Node {
        let open = self.expect_eat(
            &TokenType::Use,
            SyntaxErr::ExpectedKeyword(String::from("use")),
        );

        if self.first().token_type == TokenType::Spawn {
            let value = self.parse_spawn_statement();
            let value_span = value.span;
            let wg_name = format!(
                "__use_spawn_wg_{}_{}",
                open.span.from.line, open.span.from.col
            );
            let wg_ident: PotentialDollarIdentifier = ParserText::from(wg_name.clone()).into();

            let wg_decl = Node::new(
                Span::new_from_spans(open.span, value.span),
                NodeType::VariableDeclaration {
                    var_type: VarType::Immutable,
                    identifier: wg_ident.clone(),
                    data_type: PotentialNewType::DataType(ParserDataType::new(
                        open.span,
                        ParserInnerType::Auto(None),
                    )),
                    value: Box::new(value),
                },
            );

            let wg_ident_node = Node::new(
                open.span,
                NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                    wg_ident.clone().into(),
                )),
            );

            let wait_call = Node::new(
                Span::new_from_spans(open.span, open.span),
                NodeType::CallExpression {
                    caller: Box::new(Node::new(
                        open.span,
                        NodeType::MemberExpression {
                            path: vec![
                                (wg_ident_node, false),
                                (
                                    Node::new(
                                        open.span,
                                        NodeType::Identifier(
                                            PotentialGenericTypeIdentifier::Identifier(
                                                ParserText::from(String::from("wait")).into(),
                                            ),
                                        ),
                                    ),
                                    false,
                                ),
                            ],
                        },
                    )),
                    generic_types: Vec::new(),
                    args: Vec::new(),
                    reverse_args: Vec::new(),
                    string_fn: None,
                },
            );

            return Node::new(
                Span::new_from_spans(open.span, value_span),
                NodeType::ScopeDeclaration {
                    body: Some(vec![wg_decl, wait_call]),
                    named: None,
                    is_temp: false,
                    create_new_scope: Some(false),
                    define: false,
                },
            );
        }

        let mut identifiers = Vec::new();
        if self.first().token_type != TokenType::LeftArrow {
            loop {
                identifiers.push(self.expect_potential_dollar_ident());
                if self.first().token_type == TokenType::Comma {
                    let _ = self.eat();
                } else {
                    break;
                }
            }
        }

        let _ = self.expect_eat(
            &TokenType::LeftArrow,
            SyntaxErr::ExpectedKeyword(String::from("<-")),
        );
        let value = self.parse_assignment_expression();

        Node::new(
            Span::new_from_spans(open.span, value.span),
            NodeType::Use {
                identifiers,
                value: Box::new(value),
            },
        )
    }

    pub fn parse_select_declaration(&mut self) -> Node {
        let open = self.expect_eat(
            &TokenType::Select,
            SyntaxErr::ExpectedKeyword(String::from("select")),
        );

        let _ = self.expect_eat(
            &TokenType::Open(Bracket::Curly),
            SyntaxErr::ExpectedOpeningBracket(Bracket::Curly),
        );

        let mut arms = Vec::new();

        while self.first().token_type != TokenType::Close(Bracket::Curly) && !self.is_eof() {
            let (kind, left, right) =
                if self.first().token_type == TokenType::Identifier && self.first().value == "_" {
                    let _ = self.eat();
                    (crate::ast::SelectArmKind::Default, None, None)
                } else {
                    let left = self.parse_statement();
                    if self.first().token_type == TokenType::LeftArrow {
                        let _ = self.eat();
                        let right = self.parse_statement();
                        (crate::ast::SelectArmKind::Recv, Some(left), Some(right))
                    } else if self.first().token_type == TokenType::Arrow {
                        let _ = self.eat();
                        let right = self.parse_statement();
                        (crate::ast::SelectArmKind::Send, Some(left), Some(right))
                    } else {
                        self.add_err(SyntaxErr::ExpectedToken(TokenType::LeftArrow));
                        (crate::ast::SelectArmKind::Default, None, None)
                    }
                };

            let body = if self.first().token_type == TokenType::FatArrow {
                self.parse_scope_declaration(false)
            } else {
                self.add_err(SyntaxErr::ExpectedToken(TokenType::FatArrow));
                self.parse_statement()
            };
            if self.first().token_type == TokenType::Comma {
                let _ = self.eat();
            } else {
                let _ = self.parse_delimited();
            }

            arms.push(crate::ast::SelectArm {
                kind,
                left,
                right,
                body,
            });
        }

        let close = self.expect_eat(
            &TokenType::Close(Bracket::Curly),
            SyntaxErr::ExpectedClosingBracket(Bracket::Curly),
        );

        Node::new(
            Span::new_from_spans(open.span, close.span),
            NodeType::SelectStatement { arms },
        )
    }

    pub fn parse_extern_function_declaration(&mut self) -> Node {
        let open = self.expect_eat(
            &TokenType::Extern,
            SyntaxErr::ExpectedKeyword(String::from("extern")),
        );

        let abi = if self.first().token_type == TokenType::String {
            self.eat().value
        } else {
            String::from("c")
        };

        let _ = self.expect_eat(
            &TokenType::Const,
            SyntaxErr::ExpectedKeyword(String::from("const")),
        );

        let identifier = self.expect_potential_dollar_ident();

        let _ = self.expect_eat(&TokenType::Equals, SyntaxErr::ExpectedChar('='));

        let _ = self.expect_eat(
            &TokenType::Func,
            SyntaxErr::ExpectedKeyword(String::from("fn")),
        );

        let _ = self.expect_eat(
            &TokenType::Open(Bracket::Paren),
            SyntaxErr::ExpectedOpeningBracket(Bracket::Paren),
        );

        let mut parameters: Vec<PotentialFfiDataType> = Vec::new();

        while self.first().token_type != TokenType::Close(Bracket::Paren) {
            let ty = self.expect_potential_ffi_type();

            if self.first().token_type == TokenType::Comma {
                let _ = self.eat();
            }

            parameters.push(ty);
        }

        let _ = self.expect_eat(
            &TokenType::Close(Bracket::Paren),
            SyntaxErr::ExpectedClosingBracket(Bracket::Paren),
        );

        let return_type = if self.first().token_type == TokenType::Arrow {
            let arrow = self.eat();
            self.parse_potential_ffi_type()
                .unwrap_or(PotentialFfiDataType::Normal(
                    ParserDataType::new(arrow.span, ParserInnerType::Null).into(),
                ))
        } else {
            PotentialFfiDataType::Normal(
                ParserDataType::new(open.span, ParserInnerType::Null).into(),
            )
        };

        let mut library = String::new();
        let mut symbol = None;

        if self.first().token_type == TokenType::From {
            let _ = self.eat();
            library = self
                .expect_eat(
                    &TokenType::String,
                    SyntaxErr::ExpectedToken(TokenType::String),
                )
                .value;
        }

        if self.first().token_type == TokenType::As {
            let _ = self.eat();
            symbol = Some(
                self.expect_eat(
                    &TokenType::String,
                    SyntaxErr::ExpectedToken(TokenType::String),
                )
                .value,
            );
        }

        if !library.is_empty() {
            if let Some(base) = self.source_path.as_ref().and_then(|p| p.parent()) {
                let candidates = [
                    base.join(&library),
                    base.join(library.trim_start().trim_start_matches("./")),
                ];

                for candidate in candidates {
                    if candidate.exists() {
                        library = candidate.to_string_lossy().to_string();
                        break;
                    }
                }
            }
        } else {
            self.add_err(SyntaxErr::ExpectedToken(TokenType::String));
        }

        Node::new(
            Span::new_from_spans(open.span, *identifier.span()),
            NodeType::ExternFunctionDeclaration {
                abi,
                identifier,
                parameters,
                return_type,
                library,
                symbol,
            },
        )
    }

    pub fn expect_named_scope(&mut self) -> NamedScope {
        let _ = self.expect_eat(&TokenType::At, SyntaxErr::ExpectedChar('@'));

        let name = self.expect_potential_dollar_ident();

        let mut args = Vec::new();

        if self.first().token_type == TokenType::Open(Bracket::Square) {
            let _ = self.expect_eat(
                &TokenType::Open(Bracket::Square),
                SyntaxErr::ExpectedOpeningBracket(Bracket::Square),
            );

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
        }
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
            let close = body.last().map(|node| node.span).unwrap_or(open.span);

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
                    if self.second().token_type == TokenType::Identifier
                        && self.nth(2).map(|t| t.token_type.clone()) == Some(TokenType::Comma)
                    {
                        VarType::Immutable
                    } else {
                        let _ = self.eat();
                        VarType::Mutable
                    }
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

        if self.first().token_type == TokenType::Open(Bracket::Curly) {
            let open = self.eat();
            let mut fields: Vec<(String, VarType, PotentialDollarIdentifier)> = Vec::new();

            while self.first().token_type != TokenType::Close(Bracket::Curly) && !self.is_eof() {
                let field = self.expect_potential_dollar_ident();
                let mut bind_type = var_type.clone();
                let mut bind_name = field.clone();
                if self.first().token_type == TokenType::Colon {
                    let _ = self.eat();
                    if self.first().token_type == TokenType::Mut {
                        let _ = self.eat();
                        bind_type = VarType::Mutable;
                    }
                    bind_name = self.expect_potential_dollar_ident();
                }
                fields.push((field.to_string(), bind_type, bind_name));

                if self.first().token_type == TokenType::Comma {
                    let _ = self.eat();
                }
            }
            let _ = self.expect_eat(
                &TokenType::Close(Bracket::Curly),
                SyntaxErr::ExpectedClosingBracket(Bracket::Curly),
            );

            let _ = self.expect_eat(&TokenType::Equals, SyntaxErr::ExpectedChar('='));
            let value = self.parse_destructure_value();
            return Node::new(
                open.span,
                NodeType::DestructureDeclaration {
                    var_type,
                    pattern: DestructurePattern::Struct(fields),
                    value: Box::new(value),
                },
            );
        }

        if self.first().token_type == TokenType::Open(Bracket::Paren)
            || (self.first().token_type == TokenType::Identifier
                && self.second().token_type == TokenType::Comma)
            || self.first().token_type == TokenType::Mut
        {
            let open = if self.first().token_type == TokenType::Open(Bracket::Paren) {
                Some(self.eat())
            } else {
                None
            };
            let bindings = self.parse_tuple_bindings(var_type.clone());
            if open.is_some() {
                let _ = self.expect_eat(
                    &TokenType::Close(Bracket::Paren),
                    SyntaxErr::ExpectedClosingBracket(Bracket::Paren),
                );
            }
            let _ = self.expect_eat(&TokenType::Equals, SyntaxErr::ExpectedChar('='));
            let value = self.parse_destructure_value();

            return Node::new(
                open.map(|t| t.span).unwrap_or(self.first().span),
                NodeType::DestructureDeclaration {
                    var_type,
                    pattern: DestructurePattern::Tuple(bindings),
                    value: Box::new(value),
                },
            );
        }

        let identifier = self.expect_potential_dollar_ident();

        let data_type = match self.first().token_type {
            TokenType::Colon => {
                let _ = self.eat();
                self.expect_potential_new_type()
            }
            _ => PotentialNewType::DataType(ParserDataType::new(
                *identifier.span(),
                ParserInnerType::Auto(None),
            )),
        };

        let _ = self.expect_eat(&TokenType::Equals, SyntaxErr::ExpectedChar('='));

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

    fn parse_tuple_bindings(
        &mut self,
        default_type: VarType,
    ) -> Vec<Option<(VarType, PotentialDollarIdentifier)>> {
        let mut bindings = Vec::new();
        loop {
            if self.first().token_type == TokenType::Range {
                let _ = self.eat();
                bindings.push(None);
            } else if self.first().token_type == TokenType::FullStop
                && self.second().token_type == TokenType::FullStop
            {
                let _ = self.eat();
                let _ = self.eat();
                bindings.push(None);
            } else {
                let mut bind_type = default_type.clone();
                if self.first().token_type == TokenType::Mut {
                    let _ = self.eat();
                    bind_type = VarType::Mutable;
                }
                let name = self.expect_potential_dollar_ident();
                bindings.push(Some((bind_type, name)));
            }

            if self.first().token_type == TokenType::Comma {
                let _ = self.eat();
                continue;
            }
            break;
        }
        bindings
    }

    fn parse_destructure_value(&mut self) -> Node {
        let mut values = vec![self.parse_statement()];
        while self.first().token_type == TokenType::Comma {
            let _ = self.eat();
            values.push(self.parse_statement());
        }
        if values.len() == 1 {
            values
                .pop()
                .unwrap_or_else(|| Node::new(self.first().span, NodeType::Null))
        } else {
            if let (Some(first), Some(last)) = (values.first(), values.last()) {
                let span = Span::new_from_spans(first.span, last.span);
                Node::new(span, NodeType::TupleLiteral { values })
            } else {
                Node::new(self.first().span, NodeType::Null)
            }
        }
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
            let identifier = self.expect_potential_dollar_ident();
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

        let generics = self.parse_generic_types_with_constraints();

        let target_type = self.expect_potential_new_type();
        if self.first().token_type == TokenType::Overload {
            let overloads = self.parse_overloads();
            let identifier = match &target_type {
                PotentialNewType::DataType(ParserDataType { data_type, span }) => match data_type {
                    ParserInnerType::Struct(name) => PotentialGenericTypeIdentifier::Identifier(
                        PotentialDollarIdentifier::Identifier(ParserText {
                            text: name.clone(),
                            span: *span,
                        }),
                    ),
                    ParserInnerType::StructWithGenerics {
                        identifier,
                        generic_types,
                    } => PotentialGenericTypeIdentifier::Generic {
                        identifier: PotentialDollarIdentifier::Identifier(ParserText {
                            text: identifier.clone(),
                            span: *span,
                        }),
                        generic_types: generic_types
                            .iter()
                            .map(|t| PotentialNewType::DataType(t.clone()))
                            .collect(),
                    },
                    _ => PotentialGenericTypeIdentifier::Identifier(
                        PotentialDollarIdentifier::Identifier(ParserText {
                            text: data_type.to_string(),
                            span: *span,
                        }),
                    ),
                },
                PotentialNewType::NewType { identifier, .. } => {
                    PotentialGenericTypeIdentifier::Identifier(identifier.clone())
                }
            };

            let auto_inner = PotentialNewType::DataType(ParserDataType::new(
                *target_type.span(),
                ParserInnerType::Auto(None),
            ));
            return Node::new(
                *target_type.span(),
                NodeType::TypeDeclaration {
                    identifier,
                    object: TypeDefType::NewType(Box::new(auto_inner)),
                    overloads,
                },
            );
        }
        let target = if self.first().token_type == TokenType::For {
            let _ = self.eat();
            Some(self.expect_potential_new_type())
        } else {
            None
        };

        let mut variables = Vec::new();

        let _ = self.expect_eat(
            &TokenType::Open(Bracket::Curly),
            SyntaxErr::ExpectedOpeningBracket(Bracket::Curly),
        );

        while self.first().token_type != TokenType::Close(Bracket::Curly) {
            let decl = if self.first().token_type == TokenType::Type {
                self.parse_type_decaration()
            } else {
                self.parse_variable_declaration()
            };

            let _ = self.parse_delimited();

            match &decl.node_type {
                NodeType::VariableDeclaration {
                    var_type: VarType::Constant,
                    ..
                }
                | NodeType::TypeDeclaration { .. } => {
                    variables.push(decl);
                }
                _ => self.add_err(SyntaxErr::ExpectedFunctions),
            }
        }

        let _ = self.expect_eat(
            &TokenType::Close(Bracket::Curly),
            SyntaxErr::ExpectedClosingBracket(Bracket::Curly),
        );

        if let Some(target) = target {
            let trait_ident = match target_type {
                PotentialNewType::DataType(ParserDataType {
                    data_type: ParserInnerType::Struct(name),
                    span,
                }) => PotentialGenericTypeIdentifier::Identifier(
                    PotentialDollarIdentifier::Identifier(ParserText { text: name, span }),
                ),
                PotentialNewType::DataType(ParserDataType {
                    data_type: ParserInnerType::List(inner),
                    span,
                }) => PotentialGenericTypeIdentifier::Generic {
                    identifier: PotentialDollarIdentifier::Identifier(ParserText {
                        text: "list".to_string(),
                        span,
                    }),
                    generic_types: vec![PotentialNewType::DataType(*inner)],
                },
                PotentialNewType::DataType(ParserDataType {
                    data_type: ParserInnerType::StructWithGenerics { identifier, .. },
                    span,
                }) => PotentialGenericTypeIdentifier::Identifier(
                    PotentialDollarIdentifier::Identifier(ParserText {
                        text: identifier,
                        span,
                    }),
                ),
                _ => {
                    self.add_err(SyntaxErr::ExpectedIdentifier);
                    PotentialGenericTypeIdentifier::Identifier(
                        PotentialDollarIdentifier::Identifier(ParserText::from(String::from(
                            "Invalid",
                        ))),
                    )
                }
            };

            Node::new(
                *target.span(),
                NodeType::ImplTraitDeclaration {
                    generics,
                    trait_ident,
                    target,
                    variables,
                },
            )
        } else {
            Node::new(
                *target_type.span(),
                NodeType::ImplDeclaration {
                    generics,
                    target: target_type,
                    variables,
                },
            )
        }
    }

    pub fn parse_trait_declaration(&mut self) -> Node {
        let open = self.expect_eat(
            &TokenType::Trait,
            SyntaxErr::ExpectedKeyword(String::from("trait")),
        );

        let identifier = self.expect_potential_generic_type_ident();
        let mut implied_traits = Vec::new();

        if self.first().token_type == TokenType::Colon {
            let _ = self.eat();
            implied_traits.push(self.expect_potential_dollar_ident());

            while self.first().token_type == TokenType::BinaryOperator(BinaryOperator::Add) {
                let _ = self.eat();
                implied_traits.push(self.expect_potential_dollar_ident());
            }
        }

        let _ = self.expect_eat(
            &TokenType::Open(Bracket::Curly),
            SyntaxErr::ExpectedOpeningBracket(Bracket::Curly),
        );

        let mut members = Vec::new();

        while self.first().token_type != TokenType::Close(Bracket::Curly) && !self.is_eof() {
            let kind = if self.first().token_type == TokenType::Type {
                let _ = self.eat();
                crate::ast::TraitMemberKind::Type
            } else {
                let _ = self.expect_eat(
                    &TokenType::Const,
                    SyntaxErr::ExpectedKeyword(String::from("const")),
                );
                crate::ast::TraitMemberKind::Const
            };

            let member_ident = self.expect_potential_dollar_ident();

            let mut data_type = PotentialNewType::DataType(ParserDataType::new(
                *member_ident.span(),
                ParserInnerType::Auto(None),
            ));

            if self.first().token_type == TokenType::Colon {
                let _ = self.eat();
                data_type = self.expect_potential_new_type();
            } else if matches!(kind, crate::ast::TraitMemberKind::Type)
                && self.first().token_type == TokenType::Equals
            {
                let _ = self.eat();
                data_type = self.expect_potential_new_type();
            }

            let mut value = None;
            if matches!(kind, crate::ast::TraitMemberKind::Const)
                && self.first().token_type == TokenType::Equals
            {
                let _ = self.eat();
                value = Some(Box::new(self.parse_statement()));
            }

            let _ = self.parse_delimited();

            members.push(crate::ast::TraitMember {
                kind,
                identifier: member_ident,
                data_type,
                value,
            });
        }

        let _ = self.expect_eat(
            &TokenType::Close(Bracket::Curly),
            SyntaxErr::ExpectedClosingBracket(Bracket::Curly),
        );

        Node::new(
            Span::new_from_spans(open.span, *identifier.span()),
            NodeType::TraitDeclaration {
                identifier,
                implied_traits,
                members,
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
            let mut module = vec![match this.first().token_type {
                TokenType::List | TokenType::Range => {
                    let tok = this.eat();
                    PotentialDollarIdentifier::Identifier(ParserText::from(tok))
                }
                _ => this.expect_potential_dollar_ident(),
            }];

            while matches!(
                this.first().token_type,
                TokenType::Colon | TokenType::DoubleColon
            ) {
                let _ = this.eat();
                module.push(match this.first().token_type {
                    TokenType::List | TokenType::Range => {
                        let tok = this.eat();
                        PotentialDollarIdentifier::Identifier(ParserText::from(tok))
                    }
                    _ => this.expect_potential_dollar_ident(),
                });
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
                    destructure: None,
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

            let mut destructure_info: Option<(PotentialDollarIdentifier, DestructurePattern)> =
                None;
            if self.first().token_type == TokenType::Colon {
                let _ = self.eat();
                let _typ = self.parse_match_var_type();
                let mut bindings = Vec::new();
                let mut enum_name: Option<PotentialDollarIdentifier> = None;

                if self.first().token_type == TokenType::Open(Bracket::Curly) {
                    let open = self.eat();
                    let mut fields: Vec<(String, VarType, PotentialDollarIdentifier)> = Vec::new();
                    while self.first().token_type != TokenType::Close(Bracket::Curly)
                        && !self.is_eof()
                    {
                        let field = self.expect_potential_dollar_ident();
                        let mut bind_type = _typ.clone();
                        let mut bind_name = field.clone();
                        if self.first().token_type == TokenType::Colon {
                            let _ = self.eat();
                            if self.first().token_type == TokenType::Mut {
                                let _ = self.eat();
                                bind_type = VarType::Mutable;
                            }
                            bind_name = self.expect_potential_dollar_ident();
                        }
                        fields.push((field.to_string(), bind_type, bind_name));

                        if self.first().token_type == TokenType::Comma {
                            let _ = self.eat();
                        }
                    }
                    let _ = self.expect_eat(
                        &TokenType::Close(Bracket::Curly),
                        SyntaxErr::ExpectedClosingBracket(Bracket::Curly),
                    );

                    let tmp_name = PotentialDollarIdentifier::Identifier(ParserText::from(
                        format!("__match_tmp_{}_{}", open.span.from.line, open.span.from.col),
                    ));
                    enum_name = Some(tmp_name.clone());
                    destructure_info = Some((tmp_name, DestructurePattern::Struct(fields)));
                } else if self.first().token_type == TokenType::Open(Bracket::Paren) {
                    let open = self.eat();
                    let tuple_bindings = self.parse_tuple_bindings(_typ.clone());
                    let _ = self.expect_eat(
                        &TokenType::Close(Bracket::Paren),
                        SyntaxErr::ExpectedClosingBracket(Bracket::Paren),
                    );

                    if tuple_bindings.len() == 1 && tuple_bindings[0].is_some() {
                        if let Some((_, name)) = tuple_bindings[0].clone() {
                            enum_name = Some(name);
                        }
                    } else {
                        let tmp_name = PotentialDollarIdentifier::Identifier(ParserText::from(
                            format!("__match_tmp_{}_{}", open.span.from.line, open.span.from.col),
                        ));
                        enum_name = Some(tmp_name.clone());
                        destructure_info =
                            Some((tmp_name, DestructurePattern::Tuple(tuple_bindings)));
                    }
                } else {
                    let mut bind_type = _typ.clone();
                    if self.first().token_type == TokenType::Mut {
                        let _ = self.eat();
                        bind_type = VarType::Mutable;
                    }
                    let n = self.expect_potential_dollar_ident();
                    bindings.push((bind_type, n.clone()));

                    while self.first().token_type == TokenType::Comma {
                        let _ = self.eat();
                        let mut bind_type = _typ.clone();
                        if self.first().token_type == TokenType::Mut {
                            let _ = self.eat();
                            bind_type = VarType::Mutable;
                        }
                        let name = self.expect_potential_dollar_ident();
                        bindings.push((bind_type, name));
                    }

                    if bindings.len() == 1 {
                        enum_name = Some(n);
                    } else {
                        let tmp_name = PotentialDollarIdentifier::Identifier(ParserText::from(
                            format!("__match_tmp_{}_{}", n.span().from.line, n.span().from.col),
                        ));
                        enum_name = Some(tmp_name.clone());
                        let tuple_bindings = bindings
                            .iter()
                            .cloned()
                            .map(|(v, n)| Some((v, n)))
                            .collect();
                        destructure_info =
                            Some((tmp_name, DestructurePattern::Tuple(tuple_bindings)));
                    }
                }

                for val in values.iter_mut() {
                    match val {
                        MatchArmType::Enum {
                            var_type,
                            name,
                            destructure,
                            ..
                        } => {
                            *var_type = _typ.clone();
                            *name = enum_name.clone();
                            if let Some((_, pattern)) = destructure_info.as_ref() {
                                *destructure = Some(pattern.clone());
                            }
                        }
                        MatchArmType::Value(Node {
                            node_type: NodeType::Identifier(id),
                            ..
                        }) => {
                            *val = MatchArmType::Enum {
                                value: id.clone().into(),
                                var_type: _typ.clone(),
                                name: enum_name.clone(),
                                destructure: destructure_info.as_ref().map(|x| x.1.clone()),
                            };
                        }
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

        let generic_types = self.parse_generic_types_with_constraints();

        let typ = if self.first().token_type != TokenType::Open(Bracket::Curly) {
            self.parse_potential_new_type()
        } else {
            None
        };

        let return_type = if self.first().token_type == TokenType::Open(Bracket::Curly) {
            ParserDataType::new(open.span, ParserInnerType::Null).into()
        } else {
            let arrow = self.expect_eat(
                &TokenType::Arrow,
                SyntaxErr::ExpectedKeyword(String::from("->")),
            );
            self.parse_potential_new_type()
                .unwrap_or(ParserDataType::new(arrow.span, ParserInnerType::Null).into())
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
                    param_destructures: Vec::new(),
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
        let mut block = self.parse_scope_declaration(false);
        let mut label = None;

        if let NodeType::ScopeDeclaration { named, body, .. } = &mut block.node_type {
            if let Some(named) = named
                && named.args.is_empty()
                && body.is_some()
            {
                label = Some(named.name.clone());
            }
            *named = None;
        }

        let until = if self.first().token_type == TokenType::Stop(StopValue::Until) {
            let _ = self.eat();
            Some(Box::new(self.parse_statement()))
        } else {
            None
        };

        let else_body = if self.first().token_type == TokenType::Else {
            let _ = self.eat();
            Some(Box::new(self.parse_scope_declaration(false)))
        } else {
            None
        };

        Node::new(
            Span::new_from_spans(open.span, block.span),
            NodeType::LoopDeclaration {
                loop_type: Box::new(typ),
                body: Box::new(block),
                until,
                label,
                else_body,
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

        let generic_types = self.parse_generic_types_with_constraints();

        let (parameters, destructures) = self.parse_function_params_with_destructure();

        let return_type = if self.first().token_type == TokenType::FatArrow {
            ParserDataType::new(open.span, ParserInnerType::Auto(None)).into()
        } else {
            let arrow = self.expect_eat(
                &TokenType::Arrow,
                SyntaxErr::ExpectedKeyword(String::from("->")),
            );
            self.parse_potential_new_type()
                .unwrap_or(ParserDataType::new(arrow.span, ParserInnerType::Null).into())
        };

        let block = self.parse_scope_declaration(false);

        let func = Node::new(
            Span::new_from_spans(open.span, block.span),
            NodeType::FunctionDeclaration {
                header: FunctionHeader {
                    generics: generic_types,
                    parameters,
                    return_type,
                    param_destructures: destructures,
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

    fn parse_function_params_with_destructure(
        &mut self,
    ) -> (
        Vec<(PotentialDollarIdentifier, PotentialNewType)>,
        Vec<(PotentialDollarIdentifier, DestructurePattern)>,
    ) {
        let mut params = Vec::new();
        let mut destructures = Vec::new();
        let _ = self.expect_eat(
            &TokenType::Open(Bracket::Paren),
            SyntaxErr::ExpectedToken(TokenType::Open(Bracket::Paren)),
        );

        while !self.is_eof() && self.first().token_type != TokenType::Close(Bracket::Paren) {
            if self.first().token_type == TokenType::Open(Bracket::Paren) {
                let open = self.eat();
                let bindings = self.parse_tuple_bindings(VarType::Immutable);
                let _ = self.expect_eat(
                    &TokenType::Close(Bracket::Paren),
                    SyntaxErr::ExpectedClosingBracket(Bracket::Paren),
                );
                let typ = if self.first().token_type == TokenType::Colon {
                    let _ = self.eat();
                    self.expect_potential_new_type()
                } else {
                    ParserDataType::new(open.span.clone(), ParserInnerType::Auto(None)).into()
                };
                let tmp_name =
                    format!("__param_tmp_{}_{}", open.span.from.line, open.span.from.col);
                let tmp_ident =
                    PotentialDollarIdentifier::Identifier(ParserText::from(tmp_name.clone()));
                params.push((tmp_ident.clone(), typ));
                destructures.push((tmp_ident, DestructurePattern::Tuple(bindings)));
            } else {
                let mut keys = Vec::new();
                while self.first().token_type == TokenType::Identifier {
                    keys.push(self.expect_potential_dollar_ident());
                }

                let typ = if self.first().token_type == TokenType::Colon {
                    let _ = self.expect_eat(&TokenType::Colon, SyntaxErr::ExpectedChar(':'));
                    self.expect_potential_new_type()
                } else {
                    ParserDataType::new(self.first().span.clone(), ParserInnerType::Auto(None))
                        .into()
                };

                for key in keys {
                    params.push((key, typ.clone()));
                }
            }

            if self.first().token_type != TokenType::Close(Bracket::Paren) {
                let _ = self.expect_eat(&TokenType::Comma, SyntaxErr::ExpectedChar(','));
            }
        }

        let _ = self.expect_eat(
            &TokenType::Close(Bracket::Paren),
            SyntaxErr::ExpectedToken(TokenType::Close(Bracket::Paren)),
        );

        (params, destructures)
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
                        destructure: _,
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
            NodeType::FunctionDeclaration { header, .. } => {
                assert_eq!(header.parameters.len(), 2);
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
