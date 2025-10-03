use crate::{
    ast::{Node, NodeType, ObjectType, TypeDefType}, lexer::{Bracket, TokenType}, Parser, ParserError, SyntaxErr
};

impl Parser {
    pub fn parse_type_decaration(&mut self) -> Result<Node, ParserError> {
        let open = self.expect_eat(
            &TokenType::Type,
            SyntaxErr::ExpectedKeyword(String::from("type")),
        )?;

        let identifier = self
            .expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedIdentifier)?
            .value;

        let _ = self.expect_eat(&TokenType::Equals, SyntaxErr::ExpectedChar('='))?;

        let object = match &self.first().token_type {
            TokenType::Enum => self.parse_enum_declaration()?,
            TokenType::Struct => self.parse_struct_declaration()?,
            _ => {
                if let Some(t) = self.parse_type()? {
                    TypeDefType::NewType(t)
                } else {
                    return Err(self.get_err(SyntaxErr::ExpectedType));
                }
            }
        };

        Ok(Node::new(NodeType::TypeDeclaration { identifier, object }, open.line, open.col))
    }

    pub fn parse_enum_declaration(&mut self) -> Result<TypeDefType, ParserError> {
        let _ = self.expect_eat(
            &TokenType::Enum,
            SyntaxErr::ExpectedKeyword(String::from("enum")),
        )?;

        let _ = self.expect_eat(
            &TokenType::Open(Bracket::Curly),
            SyntaxErr::ExpectedOpeningBracket(Bracket::Curly),
        )?;

        let mut options = Vec::new();

        while self.first().token_type == TokenType::Identifier {
            let option = self.eat().value;

            if self.first().token_type == TokenType::Open(Bracket::Curly)
                || self.first().token_type == TokenType::Open(Bracket::Paren)
            {
                options.push((option, Some(self.parse_key_type_list_object_val()?)));
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

        Ok(TypeDefType::Enum(options))
    }

    pub fn parse_struct_declaration(&mut self) -> Result<TypeDefType, ParserError> {
        let _ = self.expect_eat(
            &TokenType::Struct,
            SyntaxErr::ExpectedKeyword(String::from("struct")),
        )?;

        Ok(TypeDefType::Struct(match self.first().token_type {
            TokenType::Open(Bracket::Curly) => ObjectType::Map(self.parse_key_type_list(
                TokenType::Open(Bracket::Curly),
                TokenType::Close(Bracket::Curly),
            )?),
            _ => ObjectType::Tuple(
                self.parse_type_list(
                    TokenType::Open(Bracket::Paren),
                    TokenType::Close(Bracket::Paren),
                )?
                .into_iter()
                .map(|x| x.0)
                .collect(),
            ),
        }))
    }
}
