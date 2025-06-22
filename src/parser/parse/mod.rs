pub mod binary;
pub mod declarations;
pub mod expressions;
pub mod functions;

use std::{collections::HashMap, str::FromStr};

use crate::{
    ast::{NodeType, RefMutability},
    lexer::{Token, TokenType},
    parser::{Parser, ParserError, SyntaxErr},
    runtime::values::{RuntimeType, helper::ObjectType},
};

impl Parser {
    fn first(&self) -> &Token {
        self.tokens.first().unwrap()
    }

    fn nth(&self, i: usize) -> &Token {
        self.tokens.get(i).unwrap()
    }

    fn eat(&mut self) -> Token {
        self.tokens.remove(0)
    }

    fn get_err(&self, err: SyntaxErr) -> ParserError {
        ParserError::Syntax(err, self.first().clone(), self.nth(1).clone())
    }

    fn expect_eat(&mut self, t: &TokenType, err: SyntaxErr) -> Result<Token, ParserError> {
        let value = self.eat();

        if &value.token_type != t {
            return Err(self.get_err(err));
        }

        Ok(value)
    }

    fn parse_key_type_list_ordered_with_ref(
        &mut self,
        open_token: TokenType,
        close_token: TokenType,
    ) -> Result<Vec<(String, RuntimeType, RefMutability, Option<NodeType>)>, ParserError> {
        let mut properties = Vec::new();
        let mut defaulted = false;
        let _ = self.expect_eat(
            &open_token,
            SyntaxErr::ExpectedOpeningBracket(open_token.clone()),
        );

        while !self.is_eof() && self.first().token_type != close_token {
            let key = self
                .expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedKey)?
                .value;

            let _ = self.expect_eat(&TokenType::Colon, SyntaxErr::ExpectedChar(':'))?;

            let ref_mutability = RefMutability::from(self.first().token_type.clone());

            if ref_mutability != RefMutability::Value {
                let _ = self.eat();
            }

            let typ = self.parse_type()?.expect("Expected data type.");

            let default = if defaulted || self.first().token_type == TokenType::Equals {
                let _ = self.expect_eat(&TokenType::Equals, SyntaxErr::ExpectedChar('='))?;
                defaulted = true;
                Some(self.parse_expression()?)
            } else {
                None
            };

            properties.push((key, typ, ref_mutability, default));

            if self.first().token_type != close_token {
                let _ = self.expect_eat(&TokenType::Comma, SyntaxErr::ExpectedChar(','))?;
            }
        }

        let _ = self.expect_eat(
            &close_token,
            SyntaxErr::ExpectedClosingBracket(close_token.clone()),
        )?;

        Ok(properties)
    }

    fn parse_key_type_list_ordered(
        &mut self,
        open_token: TokenType,
        close_token: TokenType,
    ) -> Result<Vec<(String, RuntimeType)>, ParserError> {
        let mut properties = Vec::new();
        let _ = self.expect_eat(
            &open_token,
            SyntaxErr::ExpectedOpeningBracket(open_token.clone()),
        );

        while !self.is_eof() && self.first().token_type != close_token {
            let key = self
                .expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedKey)?
                .value;

            let _ = self.expect_eat(&TokenType::Colon, SyntaxErr::ExpectedChar(':'))?;

            properties.push((key, self.parse_type()?.expect("Expected data type.")));

            if self.first().token_type != close_token {
                let _ = self.expect_eat(&TokenType::Comma, SyntaxErr::ExpectedChar(','))?;
            }
        }

        let _ = self.expect_eat(
            &close_token,
            SyntaxErr::ExpectedClosingBracket(close_token.clone()),
        )?;

        Ok(properties)
    }

    fn parse_key_type_list_object_val(
        &mut self,
        open_token: TokenType,
        close_token: TokenType,
    ) -> Result<ObjectType<RuntimeType>, ParserError> {
        let mut tuple = Vec::new();
        let mut properties = HashMap::new();

        let _ = self.expect_eat(
            &open_token,
            SyntaxErr::ExpectedOpeningBracket(open_token.clone()),
        );

        let is_tuple = self.nth(1).token_type != TokenType::Colon;

        while !self.is_eof() && self.first().token_type != close_token {
            if is_tuple {
                tuple.push(self.parse_type()?.expect("Expected data type."));
            } else {
                let key = self
                    .expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedKey)?
                    .value;

                let _ = self.expect_eat(&TokenType::Colon, SyntaxErr::ExpectedChar(':'))?;

                properties.insert(key, self.parse_type()?.expect("Expected data type."));
            }

            if self.first().token_type != close_token {
                let _ = self.expect_eat(&TokenType::Comma, SyntaxErr::ExpectedChar(','))?;
            }
        }

        let _ = self.expect_eat(
            &close_token,
            SyntaxErr::ExpectedClosingBracket(close_token.clone()),
        )?;

        if is_tuple {
            Ok(ObjectType::Tuple(tuple))
        } else {
            Ok(ObjectType::Map(properties))
        }
    }

    fn parse_key_type_list(
        &mut self,
        open_token: TokenType,
        close_token: TokenType,
    ) -> Result<HashMap<String, RuntimeType>, ParserError> {
        let mut properties = HashMap::new();
        let _ = self.expect_eat(
            &open_token,
            SyntaxErr::ExpectedOpeningBracket(open_token.clone()),
        );

        while !self.is_eof() && self.first().token_type != close_token {
            let key = self
                .expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedKey)?
                .value;

            let _ = self.expect_eat(&TokenType::Colon, SyntaxErr::ExpectedChar(':'))?;

            properties.insert(key, self.parse_type()?.expect("Expected data type."));

            if self.first().token_type != close_token {
                let _ = self.expect_eat(&TokenType::Comma, SyntaxErr::ExpectedChar(','))?;
            }
        }

        let _ = self.expect_eat(
            &close_token,
            SyntaxErr::ExpectedClosingBracket(close_token.clone()),
        )?;

        Ok(properties)
    }

    fn parse_type_list_with_ref(
        &mut self,
        open_token: TokenType,
        close_token: TokenType,
    ) -> Result<Vec<(RuntimeType, RefMutability)>, ParserError> {
        let mut properties = Vec::new();
        let _ = self.expect_eat(
            &open_token,
            SyntaxErr::ExpectedOpeningBracket(open_token.clone()),
        );

        while !self.is_eof() && self.first().token_type != close_token {
            let ref_mutability = RefMutability::from(self.first().token_type.clone());

            if ref_mutability != RefMutability::Value {
                let _ = self.eat();
            }

            properties.push((
                self.parse_type()?
                    .expect("Expected data type after identifier"),
                ref_mutability,
            ));

            if self.first().token_type != close_token {
                let _ = self.expect_eat(&TokenType::Comma, SyntaxErr::ExpectedChar(','))?;
            }
        }

        let _ = self.expect_eat(
            &close_token,
            SyntaxErr::ExpectedClosingBracket(close_token.clone()),
        )?;

        Ok(properties)
    }

    fn parse_type_list(
        &mut self,
        open_token: TokenType,
        close_token: TokenType,
    ) -> Result<Vec<RuntimeType>, ParserError> {
        let mut properties = Vec::new();
        let _ = self.expect_eat(
            &open_token,
            SyntaxErr::ExpectedOpeningBracket(open_token.clone()),
        );

        while !self.is_eof() && self.first().token_type != close_token {
            properties.push(
                self.parse_type()?
                    .expect("Expected data type after identifier"),
            );

            if self.first().token_type != close_token {
                let _ = self.expect_eat(&TokenType::Comma, SyntaxErr::ExpectedChar(','))?;
            }
        }

        let _ = self.expect_eat(
            &close_token,
            SyntaxErr::ExpectedClosingBracket(close_token.clone()),
        )?;

        Ok(properties)
    }

    pub fn parse_type(&mut self) -> Result<Option<RuntimeType>, ParserError> {
        let t = self.first().clone();

        if t.token_type == TokenType::OpenBrackets {
            Ok(Some(RuntimeType::Tuple(self.parse_type_list(
                TokenType::OpenBrackets,
                TokenType::CloseBrackets,
            )?)))
        } else if t.token_type == TokenType::Func || t.token_type == TokenType::Async {
            let _ = self.eat();
            let is_async = if t.token_type == TokenType::Async {
                let _ = self.expect_eat(
                    &TokenType::Func,
                    SyntaxErr::ExpectedKeyword(String::from("function")),
                )?;
                true
            } else {
                false
            };

            let args =
                self.parse_type_list_with_ref(TokenType::OpenBrackets, TokenType::CloseBrackets)?;
            let mut ret = None;

            if self.first().token_type == TokenType::Arrow {
                let _ = self.eat();
                ret = Some(self.parse_type()?.unwrap());
            }

            Ok(Some(RuntimeType::Function {
                return_type: Box::new(ret),
                parameters: args,
                is_async,
            }))
        } else if t.token_type == TokenType::List {
            let _ = self.eat();
            let t = if self.first().token_type == TokenType::OpenBrackets {
                let _ = self.eat();
                let t = Some(self.parse_type()?.expect("Expected data type"));
                let _ = self.expect_eat(
                    &TokenType::CloseBrackets,
                    SyntaxErr::ExpectedClosingBracket(TokenType::CloseBrackets),
                );
                t
            } else {
                None
            };
            Ok(Some(RuntimeType::List(Box::new(t))))
        } else {
            let _ = self.eat();
            match RuntimeType::from_str(&t.value) {
                Ok(x) => Ok(Some(x)),
                Err(_) => Ok(None),
            }
        }
    }

    pub fn parse_expression(&mut self) -> Result<NodeType, ParserError> {
        self.parse_assignment_expression()
    }

    pub fn parse_tuple_expression(&mut self) -> Result<NodeType, ParserError> {
        if self.first().token_type != TokenType::OpenBrackets {
            return self.parse_list_expression();
        }
        let mut values = Vec::new();
        let _ = self.expect_eat(
            &TokenType::OpenBrackets,
            SyntaxErr::ExpectedOpeningBracket(TokenType::OpenBrackets),
        );

        while !self.is_eof() && self.first().token_type != TokenType::CloseBrackets {
            values.push(self.parse_expression()?);

            if self.first().token_type != TokenType::CloseBrackets {
                let _ = self.expect_eat(&TokenType::Comma, SyntaxErr::ExpectedChar(','));
            }
        }

        let _ = self.expect_eat(
            &TokenType::CloseBrackets,
            SyntaxErr::ExpectedClosingBracket(TokenType::CloseBrackets),
        );
        Ok(NodeType::TupleLiteral(Box::new(values)))
    }

    pub fn parse_list_expression(&mut self) -> Result<NodeType, ParserError> {
        if self.first().token_type != TokenType::OpenSquare {
            return self.parse_object_expression();
        }

        let mut values = Vec::new();
        let _ = self.eat();

        while !self.is_eof() && self.first().token_type != TokenType::CloseSquare {
            values.push(self.parse_expression()?);

            if self.first().token_type != TokenType::CloseSquare {
                let _ = self.expect_eat(&TokenType::Comma, SyntaxErr::ExpectedChar(','));
            }
        }

        let _ = self.expect_eat(
            &TokenType::CloseSquare,
            SyntaxErr::ExpectedClosingBracket(TokenType::CloseSquare),
        );

        Ok(NodeType::ListLiteral(Box::new(values)))
    }
}
