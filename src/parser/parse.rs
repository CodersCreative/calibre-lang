use core::panic;
use std::{
    collections::{self, HashMap},
    ops::Not,
    str::FromStr,
};

use crate::{
    ast::{BinaryOperator, NodeType},
    lexer::{Token, TokenType},
    parser::Parser,
    runtime::values::{self, RuntimeType},
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

    fn expect_eat(&mut self, t: &TokenType, msg: &str) -> Token {
        let value = self.eat();
        if &value.token_type != t {
            panic!(
                "Parser Error:\n{:?}, {:?} \nExpecting: {:?} \nNext {:?}",
                msg,
                value,
                t,
                self.first()
            );
        }
        value
    }
    pub fn parse_statement(&mut self) -> NodeType {
        match self.first().token_type {
            TokenType::Var | TokenType::Let => self.parse_variable_declaration(),
            TokenType::Struct => self.parse_struct_declaration(),
            TokenType::Func => self.parse_function_declaration(),
            _ => self.parse_expression(),
        }
    }

    pub fn parse_function_declaration(&mut self) -> NodeType {
        let _ = self.eat();
        let identifier = self
            .expect_eat(&TokenType::Identifier, "Expected function identifier")
            .value;

        let parameters =
            self.parse_key_type_list_ordered(TokenType::OpenBrackets, TokenType::CloseBrackets);

        println!("{:?}", parameters);

        let is_async = self.first().token_type == TokenType::Async;

        if is_async {
            let _ = self.eat();
        }

        println!("{:?}", self.first());
        let return_type = if self.first().token_type == TokenType::OpenCurly {
            None
        } else {
            let _ = self.expect_eat(&TokenType::Arrow, "Expected arrow to show return type");
            // let t = self.expect_eat(&TokenType::Identifier, "Expected return type");
            //
            self.parse_type()
        };

        let _ = self.expect_eat(&TokenType::OpenCurly, "Expected opening brackets");

        let mut body: Vec<NodeType> = Vec::new();

        while ![TokenType::EOF, TokenType::CloseCurly].contains(&self.first().token_type) {
            body.push(self.parse_statement());
        }

        let _ = self.expect_eat(&TokenType::CloseCurly, "Expected closing brackets");

        NodeType::FunctionDeclaration {
            identifier,
            parameters,
            body: Box::new(body),
            return_type,
            is_async,
        }
    }

    fn parse_key_type_list_ordered(
        &mut self,
        open_token: TokenType,
        close_token: TokenType,
    ) -> Vec<(String, RuntimeType)> {
        let mut properties = Vec::new();
        let _ = self.expect_eat(&open_token, "Expected opening brackets");

        while !self.is_eof() && self.first().token_type != close_token {
            let key = self
                .expect_eat(&TokenType::Identifier, "Expected object literal key.")
                .value;

            let _ = self.expect_eat(&TokenType::Colon, "Object missing colon after identifier.");

            properties.push((key, self.parse_type().expect("Expected data type.")));

            if self.first().token_type != close_token {
                let _ = self.expect_eat(&TokenType::Comma, "Object missing comma after property");
            }
        }

        let _ = self.expect_eat(&close_token, "Object missing closing brace.");

        properties
    }

    fn parse_key_type_list(
        &mut self,
        open_token: TokenType,
        close_token: TokenType,
    ) -> HashMap<String, RuntimeType> {
        let mut properties = HashMap::new();
        let _ = self.expect_eat(&open_token, "Expected opening brackets");

        while !self.is_eof() && self.first().token_type != close_token {
            let key = self
                .expect_eat(&TokenType::Identifier, "Expected object literal key.")
                .value;

            let _ = self.expect_eat(&TokenType::Colon, "Object missing colon after identifier.");

            properties.insert(key, self.parse_type().expect("Expected data type."));

            if self.first().token_type != close_token {
                let _ = self.expect_eat(&TokenType::Comma, "Object missing comma after property");
            }
        }

        let _ = self.expect_eat(&close_token, "Object missing closing brace.");

        properties
    }

    fn parse_type_list(
        &mut self,
        open_token: TokenType,
        close_token: TokenType,
    ) -> Vec<RuntimeType> {
        let mut properties = Vec::new();
        let _ = self.expect_eat(&open_token, "Expected opening brackets");

        while !self.is_eof() && self.first().token_type != close_token {
            properties.push(
                self.parse_type()
                    .expect("Expected data type after identifier"),
            );
            // let value = self
            //     .expect_eat(&TokenType::Identifier, "Expected data type.")
            //     .value;
            //
            // properties.push(RuntimeType::from_str(&value).unwrap());

            if self.first().token_type != close_token {
                let _ = self.expect_eat(&TokenType::Comma, "Object missing comma after property");
            }
        }

        let _ = self.expect_eat(&close_token, "Object missing closing brace.");

        properties
    }

    pub fn parse_struct_declaration(&mut self) -> NodeType {
        let _ = self.expect_eat(&TokenType::Struct, "Expected struct keyword");

        let identifier = self
            .expect_eat(&TokenType::Identifier, "Expected struct name")
            .value;

        NodeType::StructDeclaration {
            identifier,
            properties: self.parse_key_type_list(TokenType::OpenCurly, TokenType::CloseCurly),
        }
    }

    pub fn parse_type(&mut self) -> Option<RuntimeType> {
        let t = self.eat();

        if t.token_type == TokenType::Func || t.token_type == TokenType::Async {
            let is_async = if t.token_type == TokenType::Async {
                let _ = self.expect_eat(&TokenType::Func, "Expected function keyword after async");
                true
            } else {
                false
            };

            let args = self.parse_type_list(TokenType::OpenBrackets, TokenType::CloseBrackets);
            let mut ret = None;

            if self.first().token_type == TokenType::Arrow {
                let _ = self.eat();
                ret = Some(Box::new(self.parse_type().unwrap()));
            }

            Some(RuntimeType::Function {
                return_type: ret,
                parameters: args,
                is_async,
            })
        } else {
            match RuntimeType::from_str(&t.value) {
                Ok(x) => Some(x),
                Err(_) => None,
            }
        }
    }
    pub fn parse_variable_declaration(&mut self) -> NodeType {
        let is_mutable = self.eat().token_type == TokenType::Var;
        let identifier = self
            .expect_eat(&TokenType::Identifier, "Expected variable identifier")
            .value;

        let data_type = match self.first().token_type {
            TokenType::Colon => {
                let _ = self.eat();
                self.parse_type()
            }
            _ => None,
        };

        match self.eat().token_type {
            TokenType::Equals => {
                let node = NodeType::VariableDeclaration {
                    is_mutable,
                    identifier,
                    data_type,
                    value: Some(Box::new(self.parse_expression())),
                };

                node
            }
            _ => {
                if !is_mutable {
                    panic!("Cannot declare null constant")
                }

                NodeType::VariableDeclaration {
                    is_mutable,
                    identifier,
                    value: None,
                    data_type,
                }
            } // _ => panic!("Expected either variable declaration (EOL) or assignment (=)"),
        }
    }

    pub fn parse_expression(&mut self) -> NodeType {
        self.parse_assignment_expression()
    }

    pub fn parse_assignment_expression(&mut self) -> NodeType {
        let mut left = self.parse_object_expression();

        if [TokenType::UnaryAssign].contains(&self.first().token_type) {
            let operator = self.eat();

            if ['^', '/', '*'].contains(&operator.value.chars().nth(0).unwrap()) {
                panic!(
                    "This notation cannot be used with operator {:?} as it would not alter the value.",
                    operator.value.chars().nth(0).unwrap()
                );
            }

            left = NodeType::AssignmentExpression {
                identifier: Box::new(left.clone()),
                value: Box::new(NodeType::BinaryExpression {
                    left: Box::new(left),
                    right: Box::new(NodeType::IntegerLiteral(1)),
                    operator: BinaryOperator::from_symbol(operator.value.chars().nth(0).unwrap())
                        .unwrap(),
                }),
            };
        } else if [TokenType::BinaryAssign].contains(&self.first().token_type) {
            let operator = self.eat();
            left = NodeType::AssignmentExpression {
                identifier: Box::new(left.clone()),
                value: Box::new(NodeType::BinaryExpression {
                    left: Box::new(left),
                    right: Box::new(self.parse_object_expression()),
                    operator: BinaryOperator::from_symbol(operator.value.chars().nth(0).unwrap())
                        .unwrap(),
                }),
            };
        } else if [TokenType::Equals].contains(&self.first().token_type) {
            let _ = self.eat();
            let right = self.parse_object_expression();
            left = NodeType::AssignmentExpression {
                identifier: Box::new(left),
                value: Box::new(right),
            };
        }

        left
    }

    pub fn parse_object_expression(&mut self) -> NodeType {
        if self.first().token_type != TokenType::OpenCurly {
            return self.parse_additive_expression();
        }

        let mut properties = HashMap::new();
        let _ = self.eat();

        while !self.is_eof() && self.first().token_type != TokenType::CloseCurly {
            let key = self
                .expect_eat(&TokenType::Identifier, "Expected object literal key.")
                .value;

            if [TokenType::Comma, TokenType::CloseCurly].contains(&self.first().token_type) {
                if self.first().token_type != TokenType::CloseCurly {
                    let _ = self.eat();
                }

                properties.insert(key, None);
                continue;
            }

            let _ = self.expect_eat(&TokenType::Colon, "Object missing colon after identifier.");

            properties.insert(key, Some(self.parse_expression()));

            if self.first().token_type != TokenType::CloseCurly {
                let _ = self.expect_eat(&TokenType::Comma, "Object missing comma after property");
            }
        }

        let _ = self.expect_eat(&TokenType::CloseCurly, "Object missing closing brace.");

        NodeType::MapLiteral(properties)
    }

    pub fn parse_additive_expression(&mut self) -> NodeType {
        let mut left = self.parse_multiplicative_expression();

        while ["+", "-"].contains(&self.first().value.trim())
            && self.first().token_type == TokenType::BinaryOperator
        {
            let operator = self.eat().value.trim().chars().nth(0).unwrap();
            let right = self.parse_multiplicative_expression();

            left = NodeType::BinaryExpression {
                left: Box::new(left),
                right: Box::new(right),
                operator: BinaryOperator::from_symbol(operator).unwrap(),
            };
        }

        left
    }

    pub fn parse_multiplicative_expression(&mut self) -> NodeType {
        let mut left = self.parse_power_expression();

        while ["/", "*", "%"].contains(&self.first().value.trim())
            && self.first().token_type == TokenType::BinaryOperator
        {
            let operator = self.eat().value.trim().chars().nth(0).unwrap();
            let right = self.parse_power_expression();

            left = NodeType::BinaryExpression {
                left: Box::new(left),
                right: Box::new(right),
                operator: BinaryOperator::from_symbol(operator).unwrap(),
            };
        }

        left
    }

    pub fn parse_power_expression(&mut self) -> NodeType {
        let mut left = self.parse_call_member_expression();

        while ["^"].contains(&self.first().value.trim())
            && self.first().token_type == TokenType::BinaryOperator
        {
            let operator = self.eat().value.trim().chars().nth(0).unwrap();
            let right = self.parse_call_member_expression();

            left = NodeType::BinaryExpression {
                left: Box::new(left),
                right: Box::new(right),
                operator: BinaryOperator::from_symbol(operator).unwrap(),
            };
        }

        left
    }

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

    pub fn parse_primary_expression(&mut self) -> NodeType {
        match self.first().token_type {
            TokenType::Identifier => NodeType::Identifier(self.eat().value),
            TokenType::Float => NodeType::FloatLiteral(self.eat().value.trim().parse().unwrap()),
            TokenType::Integer => {
                NodeType::IntegerLiteral(self.eat().value.trim().parse().unwrap())
            }
            TokenType::String => {
                let val = self.eat().value;
                if val.len() == 1 {
                    NodeType::CharLiteral(val.chars().nth(0).unwrap())
                } else {
                    NodeType::StringLiteral(val.to_string())
                }
            }
            TokenType::OpenBrackets => {
                self.eat();
                let value = self.parse_expression();
                self.expect_eat(
                    &TokenType::CloseBrackets,
                    "Unexpected token found inside parenthesis.",
                );
                value
            }
            _ => panic!("Unexpected Token : {:?}.", self.first()),
        }
    }
}
