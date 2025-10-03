use crate::{
    ast::{binary::BinaryOperator, IfComparisonType, LoopType, Node, ParserDataType, RefMutability, VarType}, lexer::{Bracket, StopValue}, Parser, ParserError, SyntaxErr
};
use crate::{ast::NodeType, lexer::TokenType};

impl Parser {
    pub fn parse_statement(&mut self) -> Result<Node, ParserError> {
        match &self.first().token_type {
            TokenType::Let | TokenType::Const => self.parse_variable_declaration(),
            TokenType::Func => self.parse_function_declaration(),
            TokenType::If => self.parse_if_statement(),
            TokenType::Try => self.parse_try_expression(),
            TokenType::Match => self.parse_match_declaration(),
            TokenType::Stop(x) => match x {
                StopValue::Return => self.parse_return_declaration(),
                StopValue::Break => {
                    let val = self.eat();
                    Ok(Node::new(NodeType::Break, val.line, val.col))
                }
                StopValue::Continue => {
                    let val = self.eat();
                    Ok(Node::new(NodeType::Continue, val.line, val.col))
                }
            },
            TokenType::Trait => self.parse_if_statement(),
            TokenType::Impl => self.parse_impl_declaration(),
            TokenType::Import => self.parse_import_declaration(),
            TokenType::Type => self.parse_type_decaration(),
            TokenType::For => self.parse_loop_declaration(),
            TokenType::Open(Bracket::Curly) => self.parse_scope_declaration(),
            _ => self.parse_assignment_expression(),
        }
    }

    pub fn parse_scope_declaration(&mut self) -> Result<Node, ParserError> {
        let open = self.expect_eat(
            &TokenType::Open(Bracket::Curly),
            SyntaxErr::ExpectedOpeningBracket(Bracket::Curly),
        )?;

        let mut body: Vec<Node> = Vec::new();

        while ![TokenType::EOF, TokenType::Close(Bracket::Curly)].contains(&self.first().token_type)
        {
            body.push(self.parse_statement()?);
        }

        let _ = self.expect_eat(
            &TokenType::Close(Bracket::Curly),
            SyntaxErr::ExpectedClosingBracket(Bracket::Curly),
        )?;

        Ok(Node::new(NodeType::ScopeDeclaration {
            body,
            is_temp: true,
        }, open.line, open.col))
    }

    pub fn parse_variable_declaration(&mut self) -> Result<Node, ParserError> {
        let var_type = match self.eat().token_type {
            TokenType::Const => VarType::Constant,
            TokenType::Let => {
                if let TokenType::Mut = self.first().token_type {
                    let _ = self.eat();
                    VarType::Mutable
                } else {
                    VarType::Immutable
                }
            }
            _ => return Err(self.get_err(SyntaxErr::UnexpectedToken)),
        };

        let identifier = self
            .expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedName)?;

        let data_type = match self.first().token_type {
            TokenType::Colon => {
                let _ = self.eat();
                self.parse_type()?
            }
            _ => None,
        };

        let _ = self.expect_eat(&TokenType::Equals, SyntaxErr::ExpectedChar('='))?;
        Ok(Node::new(NodeType::VariableDeclaration {
            var_type,
            identifier: identifier.value,
            data_type,
            value: Box::new(self.parse_statement()?),
        }, identifier.line, identifier.col))
    }

    pub fn parse_impl_declaration(&mut self) -> Result<Node, ParserError> {
        let _ = self.expect_eat(
            &TokenType::Impl,
            SyntaxErr::ExpectedKeyword(String::from("impl")),
        )?;

        let identifier = self
            .expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedIdentifier)?;

        let mut functions = Vec::new();

        let _ = self.expect_eat(
            &TokenType::Open(Bracket::Curly),
            SyntaxErr::ExpectedOpeningBracket(Bracket::Curly),
        )?;

        while self.first().token_type == TokenType::Const {
            let decl = self.parse_variable_declaration()?;
            match decl.node_type {
                NodeType::VariableDeclaration {
                    var_type: VarType::Constant,
                    identifier: name,
                    value: func,
                    data_type,
                } => {
                    if let NodeType::FunctionDeclaration {
                        parameters,
                        body,
                        return_type,
                        is_async,
                    } = func.node_type
                    {
                        let mut depends = false;
                        if parameters.len() > 0 {
                            if let ParserDataType::Struct(Some(obj)) = &parameters[0].1 {
                                if obj == &identifier.value {
                                    depends = true;
                                }
                            }
                        }

                        functions.push((Node::new(
                            NodeType::VariableDeclaration {
                                var_type: VarType::Constant,
                                identifier: name,
                                value: Box::new(Node::new(NodeType::FunctionDeclaration {
                                    parameters,
                                    body,
                                    return_type,
                                    is_async,
                                }, func.line, func.col)),
                                data_type,
                            }, decl.line, decl.col),
                            depends,
                        ));
                    } else if let NodeType::MatchDeclaration {
                        parameters,
                        body,
                        return_type,
                        is_async,
                    } = func.node_type
                    {
                        let mut depends = false;
                        if let ParserDataType::Struct(Some(obj)) = &parameters.1 {
                            if obj == &identifier.value {
                                depends = true;
                            }
                        }

                        functions.push((Node::new(
                            NodeType::VariableDeclaration {
                                var_type: VarType::Constant,
                                identifier: name,
                                value: Box::new(Node::new(NodeType::MatchDeclaration {
                                    parameters,
                                    body,
                                    return_type,
                                    is_async,
                                }, func.line, func.col)),
                                data_type,
                            }, decl.line, decl.col),
                            depends,
                        ));
                    }
                }
                _ => return Err(self.get_err(SyntaxErr::ExpectedFunctions)),
            }
        }

        let _ = self.expect_eat(
            &TokenType::Close(Bracket::Curly),
            SyntaxErr::ExpectedClosingBracket(Bracket::Curly),
        );

        Ok(Node::new(NodeType::ImplDeclaration {
            identifier: identifier.value,
            functions,
        }, identifier.line, identifier.col))
    }
    pub fn parse_try_expression(&mut self) -> Result<Node, ParserError> {
        if self.first().token_type == TokenType::Try {
            let open = self.expect_eat(
                &TokenType::Try,
                SyntaxErr::ExpectedKeyword(String::from("try")),
            )?;

            Ok(Node::new(NodeType::Try {
                value: Box::new(self.parse_statement()?),
            }, open.line, open.col))
        } else {
            self.parse_range_expression()
        }
    }

    pub fn parse_import_declaration(&mut self) -> Result<Node, ParserError> {
        let open = self.expect_eat(
            &TokenType::Import,
            SyntaxErr::ExpectedKeyword(String::from("import")),
        )?;

        let get_module = |this: &mut Parser| -> Result<Vec<String>, ParserError> {
            let mut module = vec![
                this.expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedIdentifier)?
                    .value,
            ];

            while this.first().token_type == TokenType::FullStop {
                module.push(
                    this.expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedIdentifier)?
                        .value,
                );
            }

            Ok(module)
        };

        if [
            TokenType::Open(Bracket::Paren),
            TokenType::BinaryOperator(BinaryOperator::Mul),
        ]
        .contains(&self.first().token_type)
        {
            let values = if TokenType::Open(Bracket::Paren) == self.first().token_type {
                self.parse_arguments(
                    TokenType::Open(Bracket::Paren),
                    TokenType::Close(Bracket::Paren),
                )?
                .into_iter()
                .map(|x| match x.0.node_type {
                    NodeType::Identifier(x) => x,
                    _ => panic!(),
                })
                .collect()
            } else {
                let _ = self.eat();
                vec!["*".to_string()]
            };

            let _ = self.expect_eat(
                &TokenType::From,
                SyntaxErr::ExpectedKeyword(String::from("from")),
            )?;

            let module = get_module(self)?;
            return Ok(Node::new(NodeType::ImportStatement {
                module,
                alias: None,
                values,
            }, open.line, open.col));
        }

        let module = get_module(self)?;

        let alias = if self.first().token_type == TokenType::As {
            let _ = self.eat();
            Some(
                self.expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedIdentifier)?
                    .value,
            )
        } else {
            None
        };

        Ok(Node::new(NodeType::ImportStatement {
            module,
            alias,
            values: Vec::new(),
        }, open.line, open.col))
    }

    pub fn parse_return_declaration(&mut self) -> Result<Node, ParserError> {
        let open = self.expect_eat(
            &TokenType::Stop(StopValue::Return),
            SyntaxErr::ExpectedKeyword(String::from("return")),
        )?;

        Ok(Node::new(NodeType::Return {
            value: Box::new(self.parse_statement()?),
        }, open.line, open.col))
    }

    pub fn parse_match_declaration(&mut self) -> Result<Node, ParserError> {
        let open = self.expect_eat(
            &TokenType::Match,
            SyntaxErr::ExpectedKeyword(String::from("match")),
        )?;

        let is_async = self.first().token_type == TokenType::Async;

        if is_async {
            let _ = self.eat();
        };

        let ref_mutability = RefMutability::from(self.first().token_type.clone());

        if ref_mutability != RefMutability::Value {
            let _ = self.eat();
        }

        let typ = if self.first().token_type != TokenType::Open(Bracket::Curly) {
            self.parse_type()?
        } else {
            None
        };

        let default = if self.first().token_type == TokenType::Equals {
            let _ = self.eat();
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };

        let return_type = if self.first().token_type == TokenType::Open(Bracket::Curly) {
            None
        } else {
            let _ = self.expect_eat(
                &TokenType::Arrow,
                SyntaxErr::ExpectedKeyword(String::from("->")),
            )?;
            self.parse_type()?
        };

        let _ = self.expect_eat(
            &TokenType::Open(Bracket::Curly),
            SyntaxErr::ExpectedOpeningBracket(Bracket::Curly),
        )?;

        let mut patterns = Vec::new();

        while self.first().token_type != TokenType::Close(Bracket::Curly) {
            let mut values = vec![self.parse_statement()?];
            let mut conditions = Vec::new();

            while self.first().token_type == TokenType::Or {
                let _ = self.eat();
                values.push(self.parse_statement()?);
            }

            while self.first().token_type == TokenType::If {
                let _ = self.eat();
                conditions.push(self.parse_statement()?);
            }

            let body = Box::new(self.parse_block()?);

            for value in values {
                patterns.push((value, conditions.clone(), body.clone()));
            }

            if self.first().token_type == TokenType::Comma {
                let _ = self.eat();
            }
        }

        let _ = self.expect_eat(
            &TokenType::Close(Bracket::Curly),
            SyntaxErr::ExpectedClosingBracket(Bracket::Curly),
        );

        let func = Node::new(NodeType::MatchDeclaration {
            parameters: (
                String::from("input_value"),
                typ.unwrap_or(ParserDataType::Dynamic),
                ref_mutability,
                default,
            ),
            body: patterns,
            return_type,
            is_async,
        }, open.line, open.col);

        if self.first().token_type == TokenType::Open(Bracket::Paren) {
            self.parse_call_expression(func)
        } else {
            Ok(func)
        }
    }

    pub fn get_loop_type(&mut self) -> Result<LoopType, ParserError> {
        Ok(
            if self.first().token_type == TokenType::Identifier
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
                    let lst = self.parse_statement()?;

                    if let NodeType::Identifier(x) = lst.node_type {
                        LoopType::ForEach(identifier, (x, ref_mutability))
                    } else {
                        LoopType::For(identifier, lst)
                    }
                }
            } else {
                let task = self.parse_statement()?;
                LoopType::While(task)
            },
        )
    }

    pub fn parse_loop_declaration(&mut self) -> Result<Node, ParserError> {
        let open = self.expect_eat(
            &TokenType::For,
            SyntaxErr::ExpectedKeyword(String::from("for")),
        )?;

        Ok(Node::new(NodeType::LoopDeclaration {
            loop_type: Box::new(self.get_loop_type()?),
            body: Box::new(self.parse_block()?),
        }, open.line, open.col))
    }

    pub fn parse_function_declaration(&mut self) -> Result<Node, ParserError> {
        let open = self.expect_eat(
            &TokenType::Func,
            SyntaxErr::ExpectedKeyword(String::from("fn")),
        )?;

        let parameters = self.parse_key_type_list_ordered_with_ref(
            TokenType::Open(Bracket::Paren),
            TokenType::Close(Bracket::Paren),
        )?;

        let is_async = self.first().token_type == TokenType::Async;

        if is_async {
            let _ = self.eat();
        }

        let return_type = if self.first().token_type == TokenType::FatArrow {
            None
        } else {
            let _ = self.expect_eat(
                &TokenType::Arrow,
                SyntaxErr::ExpectedKeyword(String::from("->")),
            )?;
            self.parse_type()?
        };

        let func = Node::new(NodeType::FunctionDeclaration {
            parameters,
            body: Box::new(self.parse_block()?),
            return_type,
            is_async,
        }, open.line, open.col);

        if self.first().token_type == TokenType::Open(Bracket::Paren) {
            self.parse_call_expression(func)
        } else {
            Ok(func)
        }
    }

    pub fn parse_block(&mut self) -> Result<Node, ParserError> {
        let _ = self.expect_eat(
            &TokenType::FatArrow,
            SyntaxErr::ExpectedToken(TokenType::FatArrow),
        )?;

        if self.first().token_type != TokenType::Open(Bracket::Curly) {
            self.parse_statement()
        } else {
            self.parse_scope_declaration()
        }
    }

    pub fn parse_if_statement(&mut self) -> Result<Node, ParserError> {
        let open = self.expect_eat(
            &TokenType::If,
            SyntaxErr::ExpectedKeyword(String::from("if")),
        )?;

        let comparison = if self.first().token_type == TokenType::Let {
            let _ = self.eat();
            let mut values = vec![self.parse_statement()?];
            let mut conditions = Vec::new();

            while self.first().token_type == TokenType::Or {
                let _ = self.eat();
                values.push(self.parse_statement()?);
            }

            while self.first().token_type == TokenType::If {
                let _ = self.eat();
                conditions.push(self.parse_statement()?);
            }

            let _ = self.expect_eat(
                &TokenType::LeftArrow,
                SyntaxErr::ExpectedKeyword(String::from("<-")),
            )?;

            let ref_mutability = RefMutability::from(self.first().token_type.clone());

            if ref_mutability != RefMutability::Value {
                let _ = self.eat();
            }

            let value = self.parse_statement()?;
            IfComparisonType::IfLet {
                mutability: ref_mutability.clone(),
                value: value.clone(),
                pattern: (value, conditions.clone()),
            }
        } else {
            IfComparisonType::If(self.parse_statement()?)
        };

        let then = Box::new(self.parse_block()?);

        let otherwise = if self.first().token_type == TokenType::Else {
            let _ = self.eat();
            if self.first().token_type == TokenType::If {
                Some(Box::new(self.parse_if_statement()?))
            } else {
                Some(Box::new(self.parse_block()?))
            }
        } else {
            None
        };

        Ok(Node::new(NodeType::IfStatement {
            comparison: Box::new(comparison),
            then,
            otherwise,
        }, open.line, open.col))
    }
}

#[cfg(test)]
mod tests {
    use crate::Parser;
    use crate::ast::NodeType;
    use crate::ast::VarType;
    use crate::lexer::{Token, tokenize};

    fn parser_with_tokens(tokens: Vec<Token>) -> Parser {
        Parser { tokens }
    }

    #[test]
    fn test_parse_variable_declaration_let() {
        let tokens = tokenize(String::from("let x = 42")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_variable_declaration().unwrap();
        match node {
            NodeType::VariableDeclaration {
                var_type,
                identifier,
                ..
            } => {
                assert_eq!(identifier, "x");
                assert!(matches!(var_type, VarType::Immutable));
            }
            _ => panic!("Expected VariableDeclaration"),
        }
    }

    #[test]
    fn test_parse_variable_declaration_let_mut() {
        let tokens = tokenize(String::from("let mut x = 42")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_variable_declaration().unwrap();
        match node {
            NodeType::VariableDeclaration {
                var_type,
                identifier,
                ..
            } => {
                assert_eq!(identifier, "x");
                assert!(matches!(var_type, VarType::Mutable));
            }
            _ => panic!("Expected VariableDeclaration"),
        }
    }

    #[test]
    fn test_parse_variable_declaration_const() {
        let tokens = tokenize(String::from("const y = 3.14")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_variable_declaration().unwrap();
        match node {
            NodeType::VariableDeclaration {
                var_type,
                identifier,
                ..
            } => {
                assert_eq!(identifier, "y");
                assert!(matches!(var_type, VarType::Constant));
            }
            _ => panic!("Expected VariableDeclaration"),
        }
    }

    #[test]
    fn test_parse_function_declaration_simple() {
        let tokens = tokenize(String::from("fn(x: int, y: int) => return x + y ")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_function_declaration().unwrap();
        match node {
            NodeType::FunctionDeclaration { parameters, .. } => {
                assert_eq!(parameters.len(), 2);
            }
            _ => panic!("Expected FunctionDeclaration"),
        }
    }

    #[test]
    fn test_parse_loop_declaration_for() {
        let tokens = tokenize(String::from("for i in xs => x = x + 1")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_loop_declaration().unwrap();
        match node {
            NodeType::LoopDeclaration { .. } => {}
            _ => panic!("Expected LoopDeclaration"),
        }
    }

    #[test]
    fn test_parse_return_declaration() {
        let tokens = tokenize(String::from("return 123")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_return_declaration().unwrap();
        match node {
            NodeType::Return { .. } => {}
            _ => panic!("Expected Return"),
        }
    }
}
