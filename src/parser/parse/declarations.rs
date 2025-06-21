use crate::{
    ast::{LoopType, RefMutability},
    parser::{Parser, ParserError, SyntaxErr},
};

use crate::{ast::NodeType, lexer::TokenType, runtime::values::RuntimeType};

impl Parser {
    pub fn parse_statement(&mut self) -> Result<NodeType, ParserError> {
        match self.first().token_type {
            TokenType::Let => self.parse_variable_declaration(),
            TokenType::Struct => self.parse_struct_declaration(),
            TokenType::Func => self.parse_function_declaration(false),
            TokenType::If => self.parse_if_statement(),
            TokenType::Return => self.parse_return_declaration(),
            TokenType::Trait => self.parse_if_statement(),
            TokenType::Impl => self.parse_impl_declaration(),
            TokenType::Continue => Ok(NodeType::Continue),
            TokenType::Break => Ok(NodeType::Break),
            TokenType::For => self.parse_loop_declaration(),
            _ => self.parse_expression(),
        }
    }

    pub fn parse_variable_declaration(&mut self) -> Result<NodeType, ParserError> {
        let _ = self.expect_eat(
            &TokenType::Let,
            SyntaxErr::ExpectedKeyword(String::from("let")),
        )?;
        let is_mutable = self.first().token_type == TokenType::Mut;
        if is_mutable {
            let _ = self.eat();
        }
        let identifier = self
            .expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedName)?
            .value;

        let data_type = match self.first().token_type {
            TokenType::Colon => {
                let _ = self.eat();
                self.parse_type()?
            }
            _ => None,
        };

        Ok(match self.eat().token_type {
            TokenType::Equals => NodeType::VariableDeclaration {
                is_mutable,
                identifier,
                data_type,
                value: Some(Box::new(self.parse_expression()?)),
            },
            _ => {
                if !is_mutable {
                    return Err(self.get_err(SyntaxErr::NullConstant));
                }

                NodeType::VariableDeclaration {
                    is_mutable,
                    identifier,
                    value: None,
                    data_type,
                }
            }
        })
    }

    pub fn parse_impl_declaration(&mut self) -> Result<NodeType, ParserError> {
        let _ = self.expect_eat(
            &TokenType::Impl,
            SyntaxErr::ExpectedKeyword(String::from("impl")),
        )?;

        let identifier = self
            .expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedIdentifier)?
            .value;

        let mut functions = Vec::new();

        let _ = self.expect_eat(
            &TokenType::OpenCurly,
            SyntaxErr::ExpectedOpeningBracket(TokenType::OpenCurly),
        )?;

        while self.first().token_type == TokenType::Func {
            let func = self.parse_function_declaration(true)?;

            match func {
                NodeType::FunctionDeclaration {
                    identifier: name,
                    mut parameters,
                    body,
                    return_type,
                    is_async,
                } => {
                    let mut depends = false;
                    if parameters.len() > 0 {
                        if &parameters[0].0 == "self" {
                            parameters[0].1 = RuntimeType::Struct(Some(identifier.clone()));
                            depends = true;
                        }
                    }

                    functions.push((
                        NodeType::FunctionDeclaration {
                            identifier: name,
                            parameters,
                            body,
                            return_type,
                            is_async,
                        },
                        depends,
                    ));
                }
                _ => return Err(self.get_err(SyntaxErr::ExpectedFunctions)),
            }
        }

        let _ = self.expect_eat(
            &TokenType::CloseCurly,
            SyntaxErr::ExpectedClosingBracket(TokenType::CloseCurly),
        );

        Ok(NodeType::ImplDeclaration {
            identifier,
            functions,
        })
    }
    pub fn parse_return_declaration(&mut self) -> Result<NodeType, ParserError> {
        let _ = self.expect_eat(
            &TokenType::Return,
            SyntaxErr::ExpectedKeyword(String::from("return")),
        )?;

        Ok(NodeType::Return {
            value: Box::new(self.parse_expression()?),
        })
    }

    pub fn parse_enum_declaration(&mut self) -> Result<NodeType, ParserError> {
        let _ = self.expect_eat(
            &TokenType::Enum,
            SyntaxErr::ExpectedKeyword(String::from("enum")),
        )?;

        let identifier = self
            .expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedIdentifier)?
            .value;

        let _ = self.expect_eat(
            &TokenType::OpenCurly,
            SyntaxErr::ExpectedOpeningBracket(TokenType::OpenCurly),
        )?;

        let mut options = Vec::new();

        while self.first().token_type == TokenType::Identifier {
            let option = self.eat().value;

            if self.first().token_type == TokenType::OpenCurly {
                let data = self.parse_key_type_list(TokenType::OpenCurly, TokenType::CloseCurly)?;
                options.push((option, Some(data)));
            } else {
                options.push((option, None));
            }
        }

        let _ = self.expect_eat(
            &TokenType::CloseCurly,
            SyntaxErr::ExpectedClosingBracket(TokenType::CloseCurly),
        );

        Ok(NodeType::EnumDeclaration {
            identifier,
            options,
        })
    }

    pub fn parse_struct_declaration(&mut self) -> Result<NodeType, ParserError> {
        let _ = self.expect_eat(
            &TokenType::Struct,
            SyntaxErr::ExpectedKeyword(String::from("struct")),
        )?;

        let identifier = self
            .expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedIdentifier)?
            .value;

        Ok(NodeType::StructDeclaration {
            identifier,
            properties: self.parse_key_type_list(TokenType::OpenCurly, TokenType::CloseCurly)?,
        })
    }

    pub fn parse_loop_declaration(&mut self) -> Result<NodeType, ParserError> {
        let _ = self.expect_eat(
            &TokenType::For,
            SyntaxErr::ExpectedKeyword(String::from("for")),
        )?;

        let loop_type = if self.first().token_type == TokenType::Identifier
            && self.nth(1).token_type == TokenType::In
        {
            let identifier = self
                .expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedIdentifier)?
                .value;

            let _ = self.eat();

            let ref_mutability = RefMutability::from(self.first().token_type.clone());

            if ref_mutability != RefMutability::Value {
                let _ = self.eat();

                let var = self
                    .expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedIdentifier)?
                    .value;

                LoopType::ForEach(identifier, (var, ref_mutability))
            } else {
                let lst = self.parse_expression()?;

                if let NodeType::Identifier(x) = lst {
                    LoopType::ForEach(identifier, (x, ref_mutability))
                } else {
                    LoopType::For(identifier, lst)
                }
            }
        } else {
            let task = self.parse_expression()?;
            LoopType::While(task)
        };

        Ok(NodeType::LoopDeclaration {
            loop_type: Box::new(loop_type),
            body: Box::new(self.parse_block()?),
        })
    }

    pub fn parse_function_declaration(
        &mut self,
        self_valid: bool,
    ) -> Result<NodeType, ParserError> {
        let _ = self.eat();
        let identifier = self
            .expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedIdentifier)?
            .value;

        let mut parameters = Vec::new();

        if self.tokens[1].value == "self" || self.tokens[2].value == "self" {
            if !self_valid {
                return Err(self.get_err(SyntaxErr::This));
            }
            let _ = self.expect_eat(
                &TokenType::OpenBrackets,
                SyntaxErr::ExpectedOpeningBracket(TokenType::OpenBrackets),
            )?;

            let ref_mutability = RefMutability::from(self.first().token_type.clone());

            if ref_mutability != RefMutability::Value {
                let _ = self.eat();
            }

            parameters.push((self.eat().value, RuntimeType::Str, ref_mutability));

            if self.first().token_type == TokenType::Comma {
                let mut other = self.parse_key_type_list_ordered_with_ref(
                    TokenType::Comma,
                    TokenType::CloseBrackets,
                )?;
                parameters.append(&mut other);
            } else {
                let _ = self.expect_eat(
                    &TokenType::CloseBrackets,
                    SyntaxErr::ExpectedClosingBracket(TokenType::CloseBrackets),
                )?;
            }
        } else {
            parameters = self.parse_key_type_list_ordered_with_ref(
                TokenType::OpenBrackets,
                TokenType::CloseBrackets,
            )?;
        }

        let is_async = self.first().token_type == TokenType::Async;

        if is_async {
            let _ = self.eat();
        }

        let return_type = if self.first().token_type == TokenType::OpenCurly {
            None
        } else {
            let _ = self.expect_eat(
                &TokenType::Arrow,
                SyntaxErr::ExpectedKeyword(String::from("->")),
            )?;
            self.parse_type()?
        };

        Ok(NodeType::FunctionDeclaration {
            identifier,
            parameters,
            body: Box::new(self.parse_block()?),
            return_type,
            is_async,
        })
    }

    pub fn parse_block(&mut self) -> Result<Vec<NodeType>, ParserError> {
        let _ = self.expect_eat(
            &TokenType::OpenCurly,
            SyntaxErr::ExpectedOpeningBracket(TokenType::OpenCurly),
        )?;

        let mut body: Vec<NodeType> = Vec::new();

        while ![TokenType::EOF, TokenType::CloseCurly].contains(&self.first().token_type) {
            body.push(self.parse_statement()?);
        }

        let _ = self.expect_eat(
            &TokenType::CloseCurly,
            SyntaxErr::ExpectedClosingBracket(TokenType::CloseCurly),
        )?;

        Ok(body)
    }

    pub fn parse_if_statement(&mut self) -> Result<NodeType, ParserError> {
        let mut comparisons = Vec::new();
        let mut bodies = Vec::new();

        while self.first().token_type == TokenType::If {
            let _ = self.eat();
            comparisons.push(self.parse_expression()?);
            bodies.push(Box::new(self.parse_block()?));

            if self.first().token_type == TokenType::Else {
                let _ = self.eat();
                if self.first().token_type == TokenType::OpenCurly {
                    bodies.push(Box::new(self.parse_block()?));
                    break;
                }
            } else {
                break;
            }
        }
        Ok(NodeType::IfStatement {
            comparisons: Box::new(comparisons),
            bodies,
        })
    }
}
