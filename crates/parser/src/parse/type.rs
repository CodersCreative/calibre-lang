use crate::{
    Parser, SyntaxErr,
    ast::{Node, NodeType, ObjectType, TypeDefType},
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

        Node::new(
            NodeType::TypeDeclaration {
                identifier: identifier.clone(),
                object,
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
