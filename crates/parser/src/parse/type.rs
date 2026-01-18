use crate::{
    Parser, SyntaxErr,
    ast::{Node, NodeType, ObjectType, Overload, ParserDataType, ParserInnerType, TypeDefType},
    lexer::{Bracket, Span, TokenType},
};

impl Parser {
    pub fn parse_type_def_type(&mut self) -> TypeDefType {
        match &self.first().token_type {
            TokenType::Enum => self.parse_enum_declaration(),
            TokenType::Struct => self.parse_struct_declaration(),
            _ => {
                if let Some(t) = self.parse_type() {
                    TypeDefType::NewType(Box::new(t.into()))
                } else {
                    self.add_err(SyntaxErr::ExpectedType);
                    TypeDefType::NewType(Box::new(
                        crate::ast::ParserDataType::new(
                            crate::ast::ParserInnerType::Dynamic,
                            self.first().span,
                        )
                        .into(),
                    ))
                }
            }
        }
    }

    pub fn parse_overloads(&mut self) -> Vec<Overload> {
        if self.first().token_type != TokenType::Overload {
            return Vec::new();
        }

        let _ = self.expect_eat(
            &TokenType::Overload,
            SyntaxErr::ExpectedKeyword(String::from("@overload")),
        );

        let _ = self.expect_eat(
            &TokenType::Open(Bracket::Curly),
            SyntaxErr::ExpectedOpeningBracket(Bracket::Curly),
        );

        let mut overloads = Vec::new();

        while self.first().token_type == TokenType::Func {
            let open = self.expect_eat(
                &TokenType::Func,
                SyntaxErr::ExpectedKeyword(String::from("fn")),
            );

            let is_async = self.first().token_type == TokenType::Async;

            if is_async {
                let _ = self.eat();
            }

            let operator = self.expect_eat(
                &TokenType::String,
                SyntaxErr::ExpectedToken(TokenType::String),
            );

            let parameters = self.parse_key_type_list_ordered_with_ref(
                TokenType::Open(Bracket::Paren),
                TokenType::Close(Bracket::Paren),
            );

            let return_type = if self.first().token_type == TokenType::FatArrow {
                ParserDataType::from(ParserInnerType::Null).into()
            } else {
                let _ = self.expect_eat(
                    &TokenType::Arrow,
                    SyntaxErr::ExpectedKeyword(String::from("->")),
                );
                self.parse_potential_new_type()
                    .unwrap_or(ParserDataType::from(ParserInnerType::Null).into())
            };

            let block = self.parse_scope_declaration(false);

            overloads.push(Overload {
                operator: operator.into(),
                parameters: parameters.into_iter().map(|x| (x.0, x.1)).collect(),
                return_type,
                body: Box::new(block),
                is_async,
            });
        }

        let _ = self.expect_eat(
            &TokenType::Close(Bracket::Curly),
            SyntaxErr::ExpectedClosingBracket(Bracket::Curly),
        );

        overloads
    }

    pub fn parse_type_decaration(&mut self) -> Node {
        let open = self.expect_eat(
            &TokenType::Type,
            SyntaxErr::ExpectedKeyword(String::from("type")),
        );

        if self.first().token_type == TokenType::Colon {
            let _ = self.eat();
            let data_type = self.expect_potential_new_type();
            return Node {
                span: Span::new_from_spans(open.span, *data_type.span()),
                node_type: NodeType::DataType { data_type },
            };
        }

        let identifier = self.expect_potential_dollar_ident();

        let _ = self.expect_eat(&TokenType::Equals, SyntaxErr::ExpectedChar('='));

        let object = self.parse_type_def_type();

        let overloads = self.parse_overloads();

        Node::new(
            NodeType::TypeDeclaration {
                identifier: identifier.clone(),
                object,
                overloads,
            },
            *identifier.span(),
        )
    }

    pub fn parse_enum_declaration(&mut self) -> TypeDefType {
        let _ = self.expect_eat(
            &TokenType::Enum,
            SyntaxErr::ExpectedKeyword(String::from("enum")),
        );

        let _ = self.expect_eat(
            &TokenType::Open(Bracket::Curly),
            SyntaxErr::ExpectedOpeningBracket(Bracket::Curly),
        );

        let mut options = Vec::new();

        while let Some(option) = self.parse_potential_dollar_ident() {
            if self.first().token_type == TokenType::Colon {
                let _ = self.eat();
                options.push((option, Some(self.expect_potential_new_type())));
            } else {
                options.push((option, None));
            }

            if self.first().token_type == TokenType::Comma {
                let _ = self.eat();
            }
        }

        let _ = self.expect_eat(
            &TokenType::Close(Bracket::Curly),
            SyntaxErr::ExpectedClosingBracket(Bracket::Curly),
        );

        TypeDefType::Enum(options)
    }

    pub fn parse_struct_declaration(&mut self) -> TypeDefType {
        let _ = self.expect_eat(
            &TokenType::Struct,
            SyntaxErr::ExpectedKeyword(String::from("struct")),
        );

        TypeDefType::Struct(match self.first().token_type {
            TokenType::Open(Bracket::Curly) => ObjectType::Map(self.parse_key_type_list(
                TokenType::Open(Bracket::Curly),
                TokenType::Close(Bracket::Curly),
            )),
            _ => ObjectType::Tuple(self.parse_type_list(
                TokenType::Open(Bracket::Paren),
                TokenType::Close(Bracket::Paren),
            )),
        })
    }
}
