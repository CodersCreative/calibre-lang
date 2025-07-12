use crate::{
    ast::{LoopType, RefMutability},
    lexer::Bracket,
    parser::{Parser, ParserError, SyntaxErr},
    runtime::{
        interpreter::statements::matching,
        values::{
            self,
            helper::{ObjectType, StopValue, VarType},
        },
    },
};

use crate::{ast::NodeType, lexer::TokenType, runtime::values::RuntimeType};

impl Parser {
    pub fn parse_statement(&mut self) -> Result<NodeType, ParserError> {
        match &self.first().token_type {
            TokenType::Let | TokenType::Const => self.parse_variable_declaration(),
            TokenType::Struct => self.parse_struct_declaration(),
            TokenType::Func => self.parse_function_declaration(),
            TokenType::If => self.parse_if_statement(),
            TokenType::Try => self.parse_try_declaration(),
            TokenType::Match => self.parse_match_declaration(),
            TokenType::Stop(x) => match x {
                StopValue::Return => self.parse_return_declaration(),
                StopValue::Break => {
                    self.eat();
                    Ok(NodeType::Break)
                }
                StopValue::Continue => {
                    self.eat();
                    Ok(NodeType::Continue)
                }
            },
            TokenType::Trait => self.parse_if_statement(),
            TokenType::Impl => self.parse_impl_declaration(),
            TokenType::Enum => self.parse_enum_declaration(),
            TokenType::For => self.parse_loop_declaration(),
            TokenType::Open(Bracket::Curly) => Ok(NodeType::ScopeDeclaration {
                body: self.parse_block()?,
            }),
            _ => self.parse_assignment_expression(),
            // _ => self.parse_expression(),
        }
    }

    pub fn parse_variable_declaration(&mut self) -> Result<NodeType, ParserError> {
        let var_type = match self.eat().token_type {
            TokenType::Const => VarType::Constant,
            TokenType::Let => {
                if let TokenType::Mut = self.first().token_type {
                    let _ = self.eat();
                    VarType::Mutable(None)
                } else {
                    VarType::Immutable(None)
                }
            }
            _ => return Err(self.get_err(SyntaxErr::UnexpectedToken)),
        };

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

        Ok(match self.first().token_type {
            TokenType::Equals => {
                let _ = self.eat();
                NodeType::VariableDeclaration {
                    var_type,
                    identifier,
                    data_type,
                    value: Some(Box::new(self.parse_statement()?)),
                }
            }
            _ => {
                if let VarType::Mutable(_) = var_type {
                    NodeType::VariableDeclaration {
                        var_type,
                        identifier,
                        value: None,
                        data_type,
                    }
                } else {
                    return Err(self.get_err(SyntaxErr::NullConstant));
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
            &TokenType::Open(Bracket::Curly),
            SyntaxErr::ExpectedOpeningBracket(Bracket::Curly),
        )?;

        while self.first().token_type == TokenType::Func {
            let func = self.parse_function_declaration()?;

            match func {
                NodeType::FunctionDeclaration {
                    identifier: name,
                    parameters,
                    body,
                    return_type,
                    is_async,
                } => {
                    let mut depends = false;
                    if parameters.len() > 0 {
                        if let RuntimeType::Struct(Some(obj)) = &parameters[0].1 {
                            if obj == &identifier {
                                depends = true;
                            }
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
            &TokenType::Close(Bracket::Curly),
            SyntaxErr::ExpectedClosingBracket(Bracket::Curly),
        );

        Ok(NodeType::ImplDeclaration {
            identifier,
            functions,
        })
    }
    pub fn parse_try_declaration(&mut self) -> Result<NodeType, ParserError> {
        let _ = self.expect_eat(
            &TokenType::Try,
            SyntaxErr::ExpectedKeyword(String::from("try")),
        )?;

        Ok(NodeType::Try {
            value: Box::new(self.parse_statement()?),
        })
    }
    pub fn parse_return_declaration(&mut self) -> Result<NodeType, ParserError> {
        let _ = self.expect_eat(
            &TokenType::Stop(StopValue::Return),
            SyntaxErr::ExpectedKeyword(String::from("return")),
        )?;

        Ok(NodeType::Return {
            value: Box::new(self.parse_statement()?),
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

        Ok(NodeType::EnumDeclaration {
            identifier,
            options,
        })
    }

    pub fn parse_match_declaration(&mut self) -> Result<NodeType, ParserError> {
        let _ = self.expect_eat(
            &TokenType::Match,
            SyntaxErr::ExpectedKeyword(String::from("match")),
        )?;

        let value = Box::new(self.parse_statement()?);

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

            let _ = self.expect_eat(
                &TokenType::Arrow,
                SyntaxErr::ExpectedKeyword(String::from("->")),
            )?;

            let mut body = Vec::new();

            if self.first().token_type == TokenType::Open(Bracket::Curly) {
                body = self.parse_block()?;
            } else {
                body.push(self.parse_statement()?);
            }

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

        Ok(NodeType::MatchDeclaration { value, patterns })
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
            properties: match self.first().token_type {
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
            },
        })
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

                    if let NodeType::Identifier(x) = lst {
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

    pub fn parse_loop_declaration(&mut self) -> Result<NodeType, ParserError> {
        let _ = self.expect_eat(
            &TokenType::For,
            SyntaxErr::ExpectedKeyword(String::from("for")),
        )?;

        Ok(NodeType::LoopDeclaration {
            loop_type: Box::new(self.get_loop_type()?),
            body: self.parse_block()?,
        })
    }

    pub fn parse_function_declaration(&mut self) -> Result<NodeType, ParserError> {
        let _ = self.eat();
        let identifier = self
            .expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedIdentifier)?
            .value;

        let parameters = self.parse_key_type_list_ordered_with_ref(
            TokenType::Open(Bracket::Paren),
            TokenType::Close(Bracket::Paren),
        )?;

        let is_async = self.first().token_type == TokenType::Async;

        if is_async {
            let _ = self.eat();
        }

        let return_type = if self.first().token_type == TokenType::Open(Bracket::Curly) {
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
            body: self.parse_block()?,
            return_type,
            is_async,
        })
    }

    pub fn parse_block(&mut self) -> Result<Vec<NodeType>, ParserError> {
        let _ = self.expect_eat(
            &TokenType::Open(Bracket::Curly),
            SyntaxErr::ExpectedOpeningBracket(Bracket::Curly),
        )?;

        let mut body: Vec<NodeType> = Vec::new();

        while ![TokenType::EOF, TokenType::Close(Bracket::Curly)].contains(&self.first().token_type)
        {
            body.push(self.parse_statement()?);
        }

        let _ = self.expect_eat(
            &TokenType::Close(Bracket::Curly),
            SyntaxErr::ExpectedClosingBracket(Bracket::Curly),
        )?;

        Ok(body)
    }

    pub fn parse_if_statement(&mut self) -> Result<NodeType, ParserError> {
        let mut comparisons = Vec::new();
        let mut bodies = Vec::new();

        while self.first().token_type == TokenType::If {
            let _ = self.eat();
            comparisons.push(self.parse_statement()?);
            bodies.push(self.parse_block()?);

            if self.first().token_type == TokenType::Else {
                let _ = self.eat();
                if self.first().token_type == TokenType::Open(Bracket::Curly) {
                    bodies.push(self.parse_block()?);
                    break;
                }
            } else {
                break;
            }
        }
        Ok(NodeType::IfStatement {
            comparisons,
            bodies,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::NodeType;
    use crate::lexer::{Token, TokenType, tokenize};
    use crate::parser::Parser;
    use crate::runtime::values::helper::{ObjectType, StopValue, VarType};

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
                value,
                ..
            } => {
                assert_eq!(identifier, "x");
                assert!(matches!(var_type, VarType::Immutable(_)));
                assert!(value.is_some());
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
                value,
                ..
            } => {
                assert_eq!(identifier, "x");
                assert!(matches!(var_type, VarType::Mutable(_)));
                assert!(value.is_some());
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
                value,
                ..
            } => {
                assert_eq!(identifier, "y");
                assert!(matches!(var_type, VarType::Constant));
                assert!(value.is_some());
            }
            _ => panic!("Expected VariableDeclaration"),
        }
    }

    #[test]
    fn test_parse_struct_declaration_tuple_one() {
        let tokens = tokenize(String::from("struct Point (int)")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_struct_declaration().unwrap();
        match node {
            NodeType::StructDeclaration {
                identifier,
                properties,
            } => {
                assert_eq!(identifier, "Point");
                match properties {
                    ObjectType::Tuple(v) => assert!(!v.is_empty()),
                    _ => panic!("Expected Tuple properties"),
                }
            }
            _ => panic!("Expected StructDeclaration"),
        }
    }

    #[test]
    fn test_parse_struct_declaration_tuple() {
        let tokens = tokenize(String::from("struct Point (int, int)")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_struct_declaration().unwrap();
        match node {
            NodeType::StructDeclaration {
                identifier,
                properties,
            } => {
                assert_eq!(identifier, "Point");
                match properties {
                    ObjectType::Tuple(v) => assert!(!v.is_empty()),
                    _ => panic!("Expected Tuple properties"),
                }
            }
            _ => panic!("Expected StructDeclaration"),
        }
    }

    #[test]
    fn test_parse_struct_declaration_map() {
        let tokens = tokenize(String::from("struct Person { name: str, age: int }")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_struct_declaration().unwrap();
        match node {
            NodeType::StructDeclaration {
                identifier,
                properties,
            } => {
                assert_eq!(identifier, "Person");
                match properties {
                    ObjectType::Map(m) => {
                        assert!(m.contains_key("name"));
                        assert!(m.contains_key("age"));
                    }
                    _ => panic!("Expected Map properties"),
                }
            }
            _ => panic!("Expected StructDeclaration"),
        }
    }

    #[test]
    fn test_parse_enum_declaration_simple() {
        let tokens = tokenize(String::from("enum Color { Red, Green, Blue }")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_enum_declaration().unwrap();
        match node {
            NodeType::EnumDeclaration {
                identifier,
                options,
            } => {
                assert_eq!(identifier, "Color");
                assert_eq!(options.len(), 3);
            }
            _ => panic!("Expected EnumDeclaration"),
        }
    }

    #[test]
    fn test_parse_function_declaration_simple() {
        let tokens = tokenize(String::from("func foo(x: int, y: int) { return x + y }")).unwrap();
        let mut parser = parser_with_tokens(tokens);
        let node = parser.parse_function_declaration(false).unwrap();
        match node {
            NodeType::FunctionDeclaration {
                identifier,
                parameters,
                ..
            } => {
                assert_eq!(identifier, "foo");
                assert_eq!(parameters.len(), 2);
            }
            _ => panic!("Expected FunctionDeclaration"),
        }
    }

    #[test]
    fn test_parse_loop_declaration_for() {
        let tokens = tokenize(String::from("for i in xs { x = x + 1 }")).unwrap();
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
