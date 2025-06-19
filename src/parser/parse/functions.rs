use crate::parser::Parser;

use crate::{
    ast::NodeType,
    lexer::TokenType,
};

impl Parser {
    pub fn parse_call_member_expression(&mut self) -> NodeType {
        let member = self.parse_member_expression();

        if self.first().token_type == TokenType::OpenBrackets {
            return self.parse_call_expression(member);
        }

        member
    }

    pub fn parse_call_expression(&mut self, caller: NodeType) -> NodeType {
        let mut expression = NodeType::CallExpression(
            Box::new(caller),
            Box::new(self.parse_arguments(TokenType::OpenBrackets, TokenType::CloseBrackets)),
        );

        if self.first().token_type == TokenType::OpenBrackets {
            expression = self.parse_call_expression(expression);
        }

        expression
    }

    pub fn parse_member_expression(&mut self) -> NodeType {
        let mut object = self.parse_primary_expression();

        while self.first().token_type == TokenType::FullStop
            || self.first().token_type == TokenType::OpenSquare
        {
            let (property, is_computed) = if self.eat().token_type == TokenType::FullStop {
                let prop = self.parse_primary_expression();

                if let NodeType::Identifier(_) = prop {
                    (prop, false)
                } else {
                    panic!("Cannot use dot operator without an identifier");
                }
            } else {
                let prop = self.parse_expression();
                self.expect_eat(&TokenType::CloseSquare, "Missing Closing Bracket");
                (prop, true)
            };

            object = NodeType::MemberExpression {
                object: Box::new(object),
                property: Box::new(property),
                is_computed,
            };
        }

        object
    }

    pub fn parse_arguments(
        &mut self,
        open_token: TokenType,
        close_token: TokenType,
    ) -> Vec<NodeType> {
        let _ = self.expect_eat(&open_token, "Expected open brackers.");

        let args = if self.first().token_type == close_token {
            Vec::new()
        } else {
            self.parse_arguments_list()
            // self.parse_key_type_list(open_token, close_token)
        };

        let _ = self.expect_eat(&close_token, "Missing closing brackets");

        args
    }

    pub fn parse_arguments_list(&mut self) -> Vec<NodeType> {
        let mut args = vec![self.parse_assignment_expression()];

        while self.first().token_type == TokenType::Comma {
            let _ = self.eat();
            if [TokenType::CloseCurly, TokenType::CloseBrackets].contains(&self.first().token_type)
            {
                break;
            }
            args.push(self.parse_assignment_expression());
        }

        args
    }
}
