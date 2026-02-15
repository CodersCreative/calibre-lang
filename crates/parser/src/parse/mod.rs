pub mod binary;
pub mod declarations;
pub mod expressions;
pub mod functions;
pub mod r#type;

use crate::{
    Parser, ParserError, SyntaxErr,
    ast::{
        Node, NodeType, ParserDataType, ParserFfiDataType, ParserFfiInnerType, ParserInnerType,
        PotentialDollarIdentifier, PotentialFfiDataType, PotentialNewType, RefMutability,
        comparison::ComparisonOperator,
    },
    lexer::{Bracket, Span, StopValue, Token, TokenType, Tokenizer},
};
use std::{str::FromStr, sync::OnceLock};

fn eof_token() -> &'static Token {
    static EOF_TOKEN: OnceLock<Token> = OnceLock::new();
    EOF_TOKEN.get_or_init(|| Token {
        value: String::new(),
        token_type: TokenType::EOF,
        span: Span::default(),
    })
}

impl Parser {
    fn tokenize_or_err(&mut self, input: &str) -> Vec<Token> {
        let mut tokenizer = Tokenizer::new(false);
        match tokenizer.tokenize(input) {
            Ok(tokens) => tokens,
            Err(err) => {
                self.errors.push(ParserError::Lexer(err));
                vec![eof_token().clone()]
            }
        }
    }

    fn first(&self) -> &Token {
        self.tokens.first().unwrap_or_else(|| eof_token())
    }

    fn second(&self) -> &Token {
        if let Some(x) = self.nth(1) {
            x
        } else {
            self.first()
        }
    }

    fn nth(&self, i: usize) -> Option<&Token> {
        self.tokens.get(i)
    }

    fn eat(&mut self) -> Token {
        if self.is_eof() {
            return self
                .tokens
                .last()
                .cloned()
                .unwrap_or_else(|| eof_token().clone());
        }
        let token = self.tokens.remove(0);
        self.prev_token = Some(token.clone());
        token
    }

    fn add_err(&mut self, err: SyntaxErr) {
        let span = if let Some(prev) = self.prev_token.clone() {
            prev.span
        } else {
            self.first().span
        };
        self.errors.push(ParserError::Syntax { err, span })
    }

    fn add_err_at(&mut self, err: SyntaxErr, span: Span) {
        self.errors.push(ParserError::Syntax { err, span })
    }

    fn expect_eat(&mut self, t: &TokenType, err: SyntaxErr) -> Token {
        let token = self.eat();
        if &token.token_type != t {
            self.add_err_at(err, token.span);
            if &self.first().token_type == t {
                self.eat()
            } else {
                token
            }
        } else {
            token
        }
    }

    fn parse_potential_ffi_type(&mut self) -> Option<PotentialFfiDataType> {
        if self.first().token_type == TokenType::At {
            let _ = self.eat();
            let ty = self.expect_potential_dollar_ident();

            match ParserFfiInnerType::from_str(&ty.to_string()) {
                Ok(data_type) => Some(PotentialFfiDataType::Ffi(ParserFfiDataType {
                    span: *ty.span(),
                    data_type,
                })),
                Err(_) => {
                    self.add_err_at(SyntaxErr::ExpectedType, *ty.span());
                    None
                }
            }
        } else {
            self.parse_type()
                .map(|x| PotentialFfiDataType::Normal(x.into()))
        }
    }

    fn expect_potential_ffi_type(&mut self) -> PotentialFfiDataType {
        if let Some(x) = self.parse_potential_ffi_type() {
            x
        } else {
            self.add_err(SyntaxErr::ExpectedType);
            PotentialFfiDataType::Normal(
                ParserDataType::new(self.first().span, ParserInnerType::Auto(None)).into(),
            )
        }
    }

    fn parse_potential_new_type(&mut self) -> Option<PotentialNewType> {
        if self.first().token_type == TokenType::Type {
            let _ = self.eat();
            let identifier = self.expect_potential_dollar_ident();
            let _ = self.expect_eat(&TokenType::Equals, SyntaxErr::ExpectedChar('='));
            Some(PotentialNewType::NewType {
                identifier,
                type_def: self.parse_type_def_type(),
                overloads: self.parse_overloads(),
            })
        } else {
            self.parse_type().map(|x| x.into())
        }
    }

    fn coerce_data_type(&mut self, typ: PotentialNewType) -> ParserDataType {
        match typ {
            PotentialNewType::DataType(x) => x,
            PotentialNewType::NewType { .. } => {
                let span = *typ.span();
                self.add_err_at(SyntaxErr::ExpectedType, span);
                ParserDataType::new(span, ParserInnerType::Auto(None))
            }
        }
    }

    fn expect_potential_new_type(&mut self) -> PotentialNewType {
        if let Some(x) = self.parse_potential_new_type() {
            x
        } else {
            self.add_err(SyntaxErr::ExpectedType);
            ParserDataType::new(self.first().span, ParserInnerType::Auto(None)).into()
        }
    }

    fn expect_type(&mut self) -> ParserDataType {
        if let Some(x) = self.parse_type() {
            x
        } else {
            self.add_err(SyntaxErr::ExpectedType);
            ParserDataType::new(self.first().span, ParserInnerType::Auto(None))
        }
    }

    fn parse_key_type_list_ordered_with_ref(
        &mut self,
        open_token: TokenType,
        close_token: TokenType,
    ) -> Vec<(PotentialDollarIdentifier, PotentialNewType)> {
        let mut properties = Vec::new();
        let _ = self.expect_eat(&open_token, SyntaxErr::ExpectedToken(open_token.clone()));

        while !self.is_eof() && self.first().token_type != close_token {
            let mut keys = Vec::new();

            while self.first().token_type == TokenType::Identifier {
                keys.push(self.expect_potential_dollar_ident());
            }

            let typ = if self.first().token_type == TokenType::Colon {
                let _ = self.expect_eat(&TokenType::Colon, SyntaxErr::ExpectedChar(':'));

                self.expect_potential_new_type()
            } else {
                ParserDataType::new(self.first().span, ParserInnerType::Auto(None)).into()
            };

            for key in keys {
                properties.push((key, typ.clone()));
            }

            if self.first().token_type != close_token {
                let _ = self.expect_eat(&TokenType::Comma, SyntaxErr::ExpectedChar(','));
            }
        }

        let _ = self.expect_eat(&close_token, SyntaxErr::ExpectedToken(close_token.clone()));

        properties
    }

    fn parse_key_type_list(
        &mut self,
        open_token: TokenType,
        close_token: TokenType,
    ) -> Vec<(String, PotentialNewType)> {
        let mut properties = Vec::new();
        let _ = self.expect_eat(&open_token, SyntaxErr::ExpectedToken(open_token.clone()));

        while !self.is_eof() && self.first().token_type != close_token {
            let key = self
                .expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedKey)
                .value;

            let _ = self.expect_eat(&TokenType::Colon, SyntaxErr::ExpectedChar(':'));

            properties.push((key, self.expect_potential_new_type()));

            if self.first().token_type != close_token {
                let _ = self.expect_eat(&TokenType::Comma, SyntaxErr::ExpectedChar(','));
            }
        }

        let _ = self.expect_eat(&close_token, SyntaxErr::ExpectedToken(close_token.clone()));

        properties
    }

    fn parse_type_list(
        &mut self,
        open_token: TokenType,
        close_token: TokenType,
    ) -> Vec<PotentialNewType> {
        let mut properties = Vec::new();
        let _ = self.expect_eat(&open_token, SyntaxErr::ExpectedToken(open_token.clone()));

        while !self.is_eof() && self.first().token_type != close_token {
            properties.push(self.expect_potential_new_type());

            if self.first().token_type != close_token {
                let _ = self.expect_eat(&TokenType::Comma, SyntaxErr::ExpectedChar(','));
            }
        }

        let _ = self.expect_eat(&close_token, SyntaxErr::ExpectedToken(close_token.clone()));

        properties
    }

    pub fn parse_delimited(&mut self) -> Token {
        if self.is_eof() {
            return Token {
                token_type: TokenType::EOL,
                value: String::new(),
                span: Span::default(),
            };
        }
        match self.first().token_type {
            TokenType::Close(Bracket::Curly) => self.first().clone(),
            TokenType::EOL => self.eat(),
            _ => {
                if let Some(prev) = self.prev_token.clone() {
                    match prev.token_type {
                        TokenType::Close(Bracket::Curly) | TokenType::EOL => {
                            return prev;
                        }
                        _ => {}
                    }
                }

                self.expect_eat(&TokenType::EOL, SyntaxErr::ExpectedChar(';'))
            }
        }
    }

    pub fn parse_type(&mut self) -> Option<ParserDataType> {
        let t = self.first().clone();
        let mutability = RefMutability::from(t.token_type.clone());

        let mut typ = if t.token_type == TokenType::Not {
            let not = self.eat();
            let typ = self.expect_type();
            Some(ParserDataType::new(
                Span::new_from_spans(not.span, typ.span),
                ParserInnerType::Result {
                    err: Box::new(ParserDataType::new(not.span, ParserInnerType::Auto(None))),
                    ok: Box::new(typ),
                },
            ))
        } else if t.token_type == TokenType::At {
            let at = self.eat();
            let ty = self.expect_potential_dollar_ident();
            let ffi = ParserFfiInnerType::from_str(&ty.to_string());
            match ffi {
                Ok(inner) => Some(ParserDataType::new(
                    Span::new_from_spans(at.span, *ty.span()),
                    inner.into(),
                )),
                Err(_) => {
                    self.add_err(SyntaxErr::ExpectedType);
                    Some(ParserDataType::new(
                        Span::new_from_spans(at.span, *ty.span()),
                        ParserInnerType::Auto(None),
                    ))
                }
            }
        } else if t.token_type == TokenType::Dollar {
            let open = self.eat();
            let ident = self.expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedIdentifier);
            Some(ParserDataType::new(
                Span::new_from_spans(open.span, ident.span),
                ParserInnerType::DollarIdentifier(ident.value),
            ))
        } else if mutability != RefMutability::Value {
            let mutability_token = self.eat();
            let typ = self.expect_type();
            Some(ParserDataType::new(
                Span::new_from_spans(mutability_token.span, typ.span),
                ParserInnerType::Ref(Box::new(typ), mutability.clone()),
            ))
        } else if t.token_type == TokenType::Comparison(ComparisonOperator::Lesser) {
            let types = self.parse_type_list(
                TokenType::Comparison(ComparisonOperator::Lesser),
                TokenType::Comparison(ComparisonOperator::Greater),
            );

            let span = if let (Some(first), Some(last)) = (types.first(), types.last()) {
                Span::new_from_spans(*first.span(), *last.span())
            } else {
                self.first().span
            };

            Some(ParserDataType::new(
                span,
                ParserInnerType::Tuple(
                    types
                        .into_iter()
                        .map(|x| self.coerce_data_type(x))
                        .collect(),
                ),
            ))
        } else if t.token_type == TokenType::Func {
            let open = self.eat();

            let args: Vec<ParserDataType> = self
                .parse_type_list(
                    TokenType::Open(Bracket::Paren),
                    TokenType::Close(Bracket::Paren),
                )
                .into_iter()
                .map(|x| self.coerce_data_type(x))
                .collect();
            let mut ret = ParserDataType::new(open.span, ParserInnerType::Null);

            if self.first().token_type == TokenType::Arrow {
                let _ = self.eat();
                ret = self.expect_type();
            }

            let close = if ret.data_type != ParserInnerType::Null {
                ret.span
            } else if let Some(arg) = args.last() {
                arg.span
            } else {
                open.span
            };

            Some(ParserDataType::new(
                Span::new_from_spans(open.span, close),
                ParserInnerType::Function {
                    return_type: Box::new(ret),
                    parameters: args,
                },
            ))
        } else if t.token_type == TokenType::List {
            let open = self.eat();
            let _ = self.expect_eat(
                &TokenType::ColonAngled,
                SyntaxErr::ExpectedKeyword(String::from(":<")),
            );

            let t = self.expect_type();

            let close = self.expect_eat(
                &TokenType::Comparison(ComparisonOperator::Greater),
                SyntaxErr::ExpectedToken(TokenType::Comparison(ComparisonOperator::Greater)),
            );

            Some(ParserDataType::new(
                Span::new_from_spans(open.span, close.span),
                ParserInnerType::List(Box::new(t)),
            ))
        } else {
            match ParserInnerType::from_str(&t.value) {
                Ok(x) => {
                    let mut path = Vec::new();
                    let open = self.eat().span;
                    let mut close = open;
                    let x = ParserDataType::new(open, x);

                    if let ParserInnerType::Struct(_) = x.data_type {
                        if t.value == "ptr" {
                            if self.first().token_type == TokenType::ColonAngled
                                || self.first().token_type
                                    == TokenType::Comparison(ComparisonOperator::Lesser)
                            {
                                let (open_token, close_token) =
                                    if self.first().token_type == TokenType::ColonAngled {
                                        (
                                            TokenType::ColonAngled,
                                            TokenType::Comparison(ComparisonOperator::Greater),
                                        )
                                    } else {
                                        (
                                            TokenType::Comparison(ComparisonOperator::Lesser),
                                            TokenType::Comparison(ComparisonOperator::Greater),
                                        )
                                    };

                                let inner = self.parse_type_list(open_token, close_token);
                                let span = if let (Some(first), Some(last)) =
                                    (inner.first(), inner.last())
                                {
                                    Span::new_from_spans(*first.span(), *last.span())
                                } else {
                                    open
                                };
                                let elem = inner.into_iter().next().unwrap_or(
                                    ParserDataType::new(open, ParserInnerType::Auto(None)).into(),
                                );
                                let elem = match elem {
                                    PotentialNewType::DataType(x) => x,
                                    PotentialNewType::NewType { .. } => {
                                        ParserDataType::new(open, ParserInnerType::Auto(None))
                                    }
                                };
                                return Some(ParserDataType::new(
                                    Span::new_from_spans(open, span),
                                    ParserInnerType::Ptr(Box::new(elem)),
                                ));
                            }

                            return Some(ParserDataType::new(
                                open,
                                ParserInnerType::Ptr(Box::new(ParserDataType::new(
                                    open,
                                    ParserInnerType::Null,
                                ))),
                            ));
                        }
                        path.push(x);

                        while self.first().token_type == TokenType::DoubleColon {
                            let _sep = self.eat();

                            let next = self
                                .expect_eat(&TokenType::Identifier, SyntaxErr::ExpectedIdentifier);
                            close = next.span;

                            let inner = ParserInnerType::from_str(&next.value)
                                .unwrap_or(ParserInnerType::Struct(next.value));
                            path.push(ParserDataType::new(next.span, inner));
                        }

                        if path.len() == 1 {
                            let mut typ = path.remove(0);
                            if self.first().token_type == TokenType::ColonAngled {
                                let types = self.parse_type_list(
                                    TokenType::ColonAngled,
                                    TokenType::Comparison(ComparisonOperator::Greater),
                                );
                                let span = if let (Some(_first), Some(last)) =
                                    (types.first(), types.last())
                                {
                                    Span::new_from_spans(typ.span, *last.span())
                                } else {
                                    typ.span
                                };
                                let generic_types = types
                                    .into_iter()
                                    .map(|x| match x {
                                        PotentialNewType::DataType(dt) => dt,
                                        PotentialNewType::NewType { .. } => ParserDataType::new(
                                            typ.span,
                                            ParserInnerType::Auto(None),
                                        ),
                                    })
                                    .collect();
                                typ = ParserDataType::new(
                                    span,
                                    ParserInnerType::StructWithGenerics {
                                        identifier: typ.to_string(),
                                        generic_types,
                                    },
                                );
                            }
                            Some(typ)
                        } else {
                            Some(ParserDataType::new(
                                Span::new_from_spans(open, close),
                                ParserInnerType::Scope(path),
                            ))
                        }
                    } else {
                        Some(x)
                    }
                }
                Err(_) => None,
            }
        };

        if self.first().token_type == TokenType::Question {
            let close = self.eat();
            let typ2: ParserDataType = typ?;
            typ = Some(ParserDataType::new(
                Span::new_from_spans(typ2.span, close.span),
                ParserInnerType::Option(Box::new(typ2)),
            ));
        }

        if self.first().token_type == TokenType::Not {
            let _ = self.eat();
            let t = self.parse_type()?;
            let typ2: ParserDataType = typ?;
            typ = Some(ParserDataType::new(
                Span::new_from_spans(typ2.span, t.span),
                ParserInnerType::Result {
                    err: Box::new(typ2),
                    ok: Box::new(t),
                },
            ));
        }

        typ
    }

    pub fn parse_paren_expression(&mut self) -> Node {
        let open = self.expect_eat(
            &TokenType::Open(Bracket::Paren),
            SyntaxErr::ExpectedOpeningBracket(Bracket::Paren),
        );

        let first = self.parse_statement();
        let mut values = vec![first];
        while self.first().token_type == TokenType::Comma {
            let _ = self.eat();
            if self.first().token_type == TokenType::Close(Bracket::Paren) {
                break;
            }
            values.push(self.parse_statement());
        }

        let _ = self.expect_eat(
            &TokenType::Close(Bracket::Paren),
            SyntaxErr::ExpectedClosingBracket(Bracket::Paren),
        );

        if self.first().token_type == TokenType::Question {
            let _ = self.eat();
            let then = self.parse_pipe_expression();
            let _ = self.expect_eat(&TokenType::Colon, SyntaxErr::ExpectedChar(':'));
            let otherwise = self.parse_pipe_expression();
            Node {
                span: Span::new_from_spans(open.span, otherwise.span),
                node_type: NodeType::Ternary {
                    comparison: Box::new(values.remove(0)),
                    then: Box::new(then),
                    otherwise: Box::new(otherwise),
                },
            }
        } else if values.len() > 1 {
            let span = values
                .last()
                .map(|v| Span::new_from_spans(open.span, v.span))
                .unwrap_or(open.span);
            Node::new(span, NodeType::TupleLiteral { values })
        } else {
            let value = values
                .pop()
                .unwrap_or_else(|| Node::new(open.span, NodeType::Null));
            Node::new(
                value.span,
                NodeType::ParenExpression {
                    value: Box::new(value),
                },
            )
        }
    }

    pub fn parse_list_iter_expression(&mut self) -> Node {
        let mut values = Vec::new();

        let t = if self.first().token_type == TokenType::List {
            let _ = self.expect_eat(&TokenType::List, SyntaxErr::ExpectedType);
            if self.first().token_type == TokenType::ColonAngled {
                let _ = self.expect_eat(
                    &TokenType::ColonAngled,
                    SyntaxErr::ExpectedKeyword(String::from(":<")),
                );

                let t = self.expect_potential_new_type();

                let _ = self.expect_eat(
                    &TokenType::Comparison(ComparisonOperator::Greater),
                    SyntaxErr::ExpectedToken(TokenType::Comparison(ComparisonOperator::Greater)),
                );
                Some(t)
            } else {
                None
            }
        } else {
            None
        }
        .unwrap_or(PotentialNewType::DataType(ParserDataType::new(
            self.first().span,
            ParserInnerType::Auto(None),
        )));

        let open = self.expect_eat(
            &TokenType::Open(Bracket::Square),
            SyntaxErr::ExpectedOpeningBracket(Bracket::Square),
        );

        while !self.is_eof() && self.first().token_type != TokenType::Close(Bracket::Square) {
            values.push(self.parse_statement());
            if self.first().token_type == TokenType::For {
                let _ = self.eat();
                let loop_type = self.get_loop_type();

                let mut conditionals = Vec::new();

                while self.first().token_type == TokenType::If {
                    let _ = self.eat();
                    conditionals.push(self.parse_statement());
                }

                let until = if self.first().token_type == TokenType::Stop(StopValue::Until) {
                    let _ = self.eat();
                    Some(Box::new(self.parse_statement()))
                } else {
                    None
                };

                let close = self.expect_eat(
                    &TokenType::Close(Bracket::Square),
                    SyntaxErr::ExpectedClosingBracket(Bracket::Square),
                );

                return Node::new(
                    Span::new_from_spans(open.span, close.span),
                    NodeType::IterExpression {
                        data_type: t,
                        map: Box::new(values[0].clone()),
                        loop_type: Box::new(loop_type),
                        conditionals,
                        until,
                    },
                );
            } else if self.first().token_type != TokenType::Close(Bracket::Square) {
                let _ = self.expect_eat(&TokenType::Comma, SyntaxErr::ExpectedChar(','));
            }
        }

        let close = self.expect_eat(
            &TokenType::Close(Bracket::Square),
            SyntaxErr::ExpectedClosingBracket(Bracket::Square),
        );

        Node::new(
            Span::new_from_spans(open.span, close.span),
            NodeType::ListLiteral(t, values),
        )
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
