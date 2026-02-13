use crate::{
    Parser, SyntaxErr,
    ast::{
        FunctionHeader, GenericTypes, Node, NodeType, ObjectType, Overload, ParserDataType,
        ParserInnerType, TypeDefType,
    },
    lexer::{Bracket, Span, TokenType},
};

impl Parser {
    pub fn parse_type_def_type(&mut self) -> TypeDefType {
        match &self.first().token_type {
            TokenType::Enum => self.parse_enum_declaration(),
            TokenType::Struct => self.parse_struct_declaration(),
            _ => TypeDefType::NewType(Box::new(self.expect_type().into())),
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

        while self.first().token_type == TokenType::Const {
            let _ = self.expect_eat(
                &TokenType::Const,
                SyntaxErr::ExpectedKeyword(String::from("const")),
            );

            let operator = self.expect_eat(
                &TokenType::String,
                SyntaxErr::ExpectedToken(TokenType::String),
            );

            let _ = self.expect_eat(&TokenType::Equals, SyntaxErr::ExpectedChar('='));

            let _ = self.expect_eat(
                &TokenType::Func,
                SyntaxErr::ExpectedKeyword(String::from("fn")),
            );

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

            let _ = self.parse_delimited();

            overloads.push(Overload {
                header: FunctionHeader {
                    generics: GenericTypes::default(),
                    parameters,
                    return_type,
                    param_destructures: Vec::new(),
                },
                operator: operator.into(),
                body: Box::new(block),
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

        let identifier = self.expect_potential_generic_type_ident();

        let _ = self.expect_eat(&TokenType::Equals, SyntaxErr::ExpectedChar('='));

        let object = self.parse_type_def_type();

        let overloads = self.parse_overloads();
        Node::new(
            *identifier.span(),
            NodeType::TypeDeclaration {
                identifier,
                object,
                overloads,
            },
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
