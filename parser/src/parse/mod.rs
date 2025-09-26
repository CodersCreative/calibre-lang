pub mod binary;
pub mod declarations;
pub mod expressions;
pub mod functions;
pub mod r#type;

use crate::{
    ast::{binary::BinaryOperator, comparison::Comparison, LoopType, NodeType, ObjectType, ParserDataType, RefMutability}, lexer::{Bracket, Token, TokenType}, Parser, ParserError, SyntaxErr
};
use std::{collections::HashMap, str::FromStr};

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
        ParserError::Syntax(
            err,
            self.first().clone(),
            self.nth(1).clone(),
            self.nth(2).clone(),
            self.nth(3).clone(),
        )
    }

    fn expect_eat(&mut self, t: &TokenType, err: SyntaxErr) -> Result<Token, ParserError> {
        if &self.first().token_type != t {
            Err(self.get_err(err))
        } else{
            Ok(self.eat())
        }
    }

    fn parse_key_type_list_ordered_with_ref(
        &mut self,
        open_token: TokenType,
        close_token: TokenType,
    ) -> Result<Vec<(String, ParserDataType, RefMutability, Option<NodeType>)>, ParserError> {
        let mut properties = Vec::new();
        let mut defaulted = false;
        let _ = self.expect_eat(&open_token, SyntaxErr::ExpectedToken(open_token.clone()));

        while !self.is_eof() && self.first().token_type != close_token {
            let mut keys = Vec::new();

            while self.first().token_type == TokenType::Identifier {
                keys.push(self
                .expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedKey)?
                .value);
            }

            let mut ref_mutability = RefMutability::Value;

            let typ = if self.first().token_type == TokenType::Colon {
                let _ = self.expect_eat(&TokenType::Colon, SyntaxErr::ExpectedChar(':'))?;

                ref_mutability = RefMutability::from(self.first().token_type.clone());

                if ref_mutability != RefMutability::Value {
                    let _ = self.eat();
                }

                self.parse_type()?.expect("Expected data type.")
            } else {
                ParserDataType::Dynamic
            };

            let default = if defaulted || self.first().token_type == TokenType::Equals {
                let _ = self.expect_eat(&TokenType::Equals, SyntaxErr::ExpectedChar('='))?;
                defaulted = true;
                Some(self.parse_statement()?)
            } else {
                None
            };

            for key in keys{
                properties.push((key, typ.clone(), ref_mutability.clone(), default.clone()));
            }

            if self.first().token_type != close_token {
                let _ = self.expect_eat(&TokenType::Comma, SyntaxErr::ExpectedChar(','))?;
            }
        }

        let _ = self.expect_eat(&close_token, SyntaxErr::ExpectedToken(close_token.clone()))?;

        Ok(properties)
    }

    fn parse_key_type_list_object_val(
        &mut self,
    ) -> Result<ObjectType<ParserDataType>, ParserError> {
        let mut tuple = Vec::new();
        let mut properties = HashMap::new();

        let (is_tuple, _, close_token) = match self.first().token_type {
            TokenType::Open(Bracket::Curly) => {
                let _ = self.eat();
                (
                    false,
                    TokenType::Open(Bracket::Curly),
                    TokenType::Close(Bracket::Curly),
                )
            }
            TokenType::Open(Bracket::Paren) => {
                let _ = self.eat();
                (
                    true,
                    TokenType::Open(Bracket::Paren),
                    TokenType::Close(Bracket::Paren),
                )
            }
            _ => {
                return Err(self.get_err(SyntaxErr::ExpectedOpeningBracket(Bracket::Curly)));
            }
        };

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

        let _ = self.expect_eat(&close_token, SyntaxErr::ExpectedToken(close_token.clone()))?;

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
    ) -> Result<HashMap<String, ParserDataType>, ParserError> {
        let mut properties = HashMap::new();
        let _ = self.expect_eat(&open_token, SyntaxErr::ExpectedToken(open_token.clone()));

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

        let _ = self.expect_eat(&close_token, SyntaxErr::ExpectedToken(close_token.clone()))?;

        Ok(properties)
    }

    fn parse_type_list(
        &mut self,
        open_token: TokenType,
        close_token: TokenType,
    ) -> Result<Vec<(ParserDataType, RefMutability)>, ParserError> {
        let mut properties = Vec::new();
        let _ = self.split_comparison_lesser();
        let _ = self.expect_eat(&open_token, SyntaxErr::ExpectedToken(open_token.clone()));

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

            let _ = self.split_comparison_greater()?;
            if self.first().token_type != close_token {
                let _ = self.expect_eat(&TokenType::Comma, SyntaxErr::ExpectedChar(','))?;
            }
        }

        let _ = self.expect_eat(&close_token, SyntaxErr::ExpectedToken(close_token.clone()))?;

        Ok(properties)
    }

    pub fn parse_type(&mut self) -> Result<Option<ParserDataType>, ParserError> {
        let _ = self.split_comparison_lesser();
        let t = self.first().clone();

        if self.first().token_type == TokenType::Not {
            let _ = self.eat();
            let t = self.parse_type();

            return Ok(Some(ParserDataType::Result(
                Box::new(ParserDataType::Dynamic),
                Box::new(t?.unwrap()),
            )));
        }

        let mut typ = if t.token_type == TokenType::Comparison(Comparison::Lesser) {
            Ok(Some(ParserDataType::Tuple(
                self.parse_type_list(
                    TokenType::Comparison(Comparison::Lesser),
                    TokenType::Comparison(Comparison::Greater),
                )?
                .into_iter()
                .map(|x| x.0)
                .collect(),
            )))
        } else if t.token_type == TokenType::Func || t.token_type == TokenType::Async {
            let _ = self.eat();
            let is_async = if t.token_type == TokenType::Async {
                let _ = self.expect_eat(
                    &TokenType::Async,
                    SyntaxErr::ExpectedKeyword(String::from("async")),
                )?;
                true
            } else {
                false
            };

            let args = self.parse_type_list(
                TokenType::Open(Bracket::Paren),
                TokenType::Close(Bracket::Paren),
            )?;
            let mut ret = None;

            if self.first().token_type == TokenType::Arrow {
                let _ = self.eat();
                ret = Some(self.parse_type()?.unwrap());
            }

            Ok(Some(ParserDataType::Function {
                return_type: Box::new(ret),
                parameters: args,
                is_async,
            }))
        } else if t.token_type == TokenType::List {
            let _ = self.eat();
            let _ = self.split_comparison_lesser();
            let t = if self.first().token_type == TokenType::Comparison(Comparison::Lesser) {
                let _ = self.eat();
                let t = Some(self.parse_type()?.expect("Expected data type"));
                let _ = self.split_comparison_greater()?;
                let _ = self.expect_eat(
                    &TokenType::Comparison(Comparison::Greater),
                    SyntaxErr::ExpectedToken(TokenType::Comparison(Comparison::Greater)),
                )?;
                t
            } else {
                None
            };
            Ok(Some(ParserDataType::List(Box::new(t))))
        } else {
            let _ = self.eat();
            match ParserDataType::from_str(&t.value) {
                Ok(x) => Ok(Some(x)),
                Err(_) => Ok(None),
            }
        };

        if self.first().token_type == TokenType::Question {
            let _ = self.eat();

            typ = Ok(Some(ParserDataType::Option(Box::new(typ?.unwrap()))));
        }

        if self.first().token_type == TokenType::Not {
            let _ = self.eat();
            let t = self.parse_type();

            typ = Ok(Some(ParserDataType::Result(
                Box::new(typ?.unwrap()),
                Box::new(t?.unwrap()),
            )));
        }

        typ
    }

    pub fn split_comparison_lesser(&mut self) -> Result<(), ParserError> {
        let former = self.tokens[0].clone();
        
        let get_lesser_token = || -> Token {
            Token {
                value: String::from("<"),
                token_type: TokenType::Comparison(Comparison::Lesser),
                ..former
            }
        };

        if former.token_type == TokenType::Comparison(Comparison::LesserEqual) {
            self.tokens[0] = get_lesser_token();
            self.tokens.insert(1, Token {
                value: String::from("="),
                token_type: TokenType::Equals,
                ..former
            });
        } else if former.token_type == TokenType::BinaryOperator(BinaryOperator::Shl) {
            self.tokens[0] = get_lesser_token();
            self.tokens.insert(1, get_lesser_token());
        }

        Ok(())
    }
    pub fn split_comparison_greater(&mut self) -> Result<(), ParserError> {
        let former = self.tokens[0].clone();
        
        let get_greater_token = || -> Token {
            Token {
                value: String::from(">"),
                token_type: TokenType::Comparison(Comparison::Greater),
                ..former
            }
        };

        if former.token_type == TokenType::Comparison(Comparison::GreaterEqual) {
            self.tokens[0] = get_greater_token();
            self.tokens.insert(1, Token {
                value: String::from("="),
                token_type: TokenType::Equals,
                ..former
            });
        } else if former.token_type == TokenType::BinaryOperator(BinaryOperator::Shr) {
            self.tokens[0] = get_greater_token();
            self.tokens.insert(1, get_greater_token());
        }

        Ok(())
    }

    pub fn parse_tuple_expression(&mut self) -> Result<NodeType, ParserError> {
        let mut has_commas = false;
        let mut index = 0;

        while ![TokenType::Close(Bracket::Paren), TokenType::EOF]
            .contains(&self.nth(index).token_type)
        {
            index += 1;

            if self.nth(index).token_type == TokenType::Comma {
                has_commas = true;
            }
        }

        if !has_commas {
            self.eat();
            let value = self.parse_statement();
            let _ = self.expect_eat(
                &TokenType::Close(Bracket::Paren),
                SyntaxErr::ExpectedClosingBracket(Bracket::Paren),
            )?;
            return value;
        }

        let mut values = Vec::new();
        let _ = self.expect_eat(
            &TokenType::Open(Bracket::Paren),
            SyntaxErr::ExpectedOpeningBracket(Bracket::Paren),
        );

        while !self.is_eof() && self.first().token_type != TokenType::Close(Bracket::Paren) {
            values.push(self.parse_statement()?);

            if self.first().token_type != TokenType::Close(Bracket::Paren) {
                let _ = self.expect_eat(&TokenType::Comma, SyntaxErr::ExpectedChar(','));
            }
        }

        let _ = self.expect_eat(
            &TokenType::Close(Bracket::Paren),
            SyntaxErr::ExpectedClosingBracket(Bracket::Paren),
        );

        if values.len() == 1 {
            return Ok(values[0].clone());
        }

        Ok(NodeType::TupleLiteral(values))
    }

    pub fn parse_list_expression(&mut self) -> Result<NodeType, ParserError> {
        let mut values = Vec::new();
        let _ = self.eat();

        while !self.is_eof() && self.first().token_type != TokenType::Close(Bracket::Square) {
            values.push(self.parse_statement()?);
            if self.first().token_type == TokenType::For {
                let _ = self.eat();
                let loop_type = self.get_loop_type()?;

                if let LoopType::While(_) = loop_type {
                    return Err(self.get_err(SyntaxErr::UnexpectedWhileLoop));
                }

                let mut conditionals = Vec::new();

                while self.first().token_type == TokenType::If {
                    let _ = self.eat();
                    conditionals.push(self.parse_statement()?);
                }

                let _ = self.expect_eat(
                    &TokenType::Close(Bracket::Square),
                    SyntaxErr::ExpectedClosingBracket(Bracket::Square),
                );

                return Ok(NodeType::IterExpression {
                    map: Box::new(values[0].clone()),
                    loop_type: Box::new(loop_type),
                    conditionals,
                });
            } else if self.first().token_type != TokenType::Close(Bracket::Square) {
                let _ = self.expect_eat(&TokenType::Comma, SyntaxErr::ExpectedChar(','));
            }
        }

        let _ = self.expect_eat(
            &TokenType::Close(Bracket::Square),
            SyntaxErr::ExpectedClosingBracket(Bracket::Square),
        );

        Ok(NodeType::ListLiteral(values))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{Token, TokenType, tokenize};

    fn parser_with_tokens(tokens: Vec<Token>) -> Parser {
        Parser { tokens }
    }

    #[test]
    fn test_parse_type_list() {
        let tokens = tokenize(String::from("(int, float)")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let result = parser.parse_type_list(
            TokenType::Open(Bracket::Paren),
            TokenType::Close(Bracket::Paren),
        );
        assert!(result.is_ok());
        let types = result.unwrap();
        assert_eq!(types.len(), 2);
    }

    #[test]
    fn test_parse_key_type_list_object_val_tuple() {
        let tokens = tokenize(String::from("(int, float)")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let result = parser.parse_key_type_list_object_val();
        assert!(result.is_ok());
        match result.unwrap() {
            ObjectType::Tuple(v) => assert_eq!(v.len(), 2),
            _ => panic!("Expected tuple"),
        }
    }

    #[test]
    fn test_parse_key_type_list_object_val_map() {
        let tokens = tokenize(String::from("{a : int, b : float}")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let result = parser.parse_key_type_list_object_val();
        assert!(result.is_ok());
        match result.unwrap() {
            ObjectType::Map(m) => {
                assert_eq!(m.len(), 2);
                assert!(m.contains_key("a"));
                assert!(m.contains_key("b"));
            }
            _ => panic!("Expected map"),
        }
    }
}
