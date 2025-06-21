use crate::parser::{Parser, ParserError, SyntaxErr};

use crate::{ast::NodeType, lexer::TokenType};

impl Parser {
    pub fn parse_call_member_expression(&mut self) -> Result<NodeType, ParserError> {
        let member = self.parse_member_expression()?;

        if self.first().token_type == TokenType::OpenBrackets {
            return self.parse_call_expression(member);
        }

        Ok(member)
    }

    pub fn parse_call_expression(&mut self, caller: NodeType) -> Result<NodeType, ParserError> {
        let mut expression = NodeType::CallExpression(
            Box::new(caller),
            Box::new(self.parse_arguments(TokenType::OpenBrackets, TokenType::CloseBrackets)?),
        );

        if self.first().token_type == TokenType::OpenBrackets {
            expression = self.parse_call_expression(expression)?;
        }

        Ok(expression)
    }

    pub fn parse_member_expression(&mut self) -> Result<NodeType, ParserError> {
        let mut object = self.parse_primary_expression()?;

        while self.first().token_type == TokenType::FullStop
            || self.first().token_type == TokenType::OpenSquare
        {
            let (property, is_computed) = if self.eat().token_type == TokenType::FullStop {
                let prop = self.parse_call_member_expression()?;
                // println!("{:?}", prop);
                (prop, false)
            } else {
                let prop = self.parse_expression()?;

                self.expect_eat(
                    &TokenType::CloseSquare,
                    SyntaxErr::ExpectedClosingBracket(TokenType::CloseSquare),
                )?;

                (prop, true)
            };

            // let _ = self.eat();
            if !is_computed && self.first().token_type == TokenType::OpenCurly {
                if let NodeType::Identifier(identifier) = &object {
                    if let NodeType::Identifier(value) = &property {
                        let data = self.parse_potential_key_value()?;
                        return Ok(NodeType::EnumExpression {
                            identifier: identifier.to_string(),
                            value: value.to_string(),
                            data: Some(data),
                        });
                    }
                }
            }

            object = NodeType::MemberExpression {
                object: Box::new(object),
                property: Box::new(property),
                is_computed,
            };
        }

        Ok(object)
    }

    pub fn parse_arguments(
        &mut self,
        open_token: TokenType,
        close_token: TokenType,
    ) -> Result<Vec<NodeType>, ParserError> {
        let _ = self.expect_eat(
            &open_token,
            SyntaxErr::ExpectedOpeningBracket(open_token.clone()),
        )?;

        let args = if self.first().token_type == close_token {
            Vec::new()
        } else {
            self.parse_arguments_list()?
        };

        let _ = self.expect_eat(
            &close_token,
            SyntaxErr::ExpectedClosingBracket(close_token.clone()),
        )?;

        Ok(args)
    }

    pub fn parse_arguments_list(&mut self) -> Result<Vec<NodeType>, ParserError> {
        let mut args = vec![self.parse_expression()?];

        while self.first().token_type == TokenType::Comma {
            let _ = self.eat();
            if [TokenType::CloseCurly, TokenType::CloseBrackets].contains(&self.first().token_type)
            {
                break;
            }
            args.push(self.parse_expression()?);
        }

        Ok(args)
    }
}
