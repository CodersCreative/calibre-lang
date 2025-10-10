use std::fs;

use calibre_parser::{
    ast::{Node, NodeType},
    lexer::Tokenizer,
};

use crate::environment::{Environment, Location, RuntimeType, RuntimeValue};

pub enum ScopeSearchResult {
    Success(u64),
    Failed { counter: u64, scope: u64 },
}

impl<T: RuntimeValue, U: RuntimeType> Environment<T, U> {
    pub fn find_checker_scope_at(&mut self, target: Location) -> ScopeSearchResult {
        let (root, path) = {
            let root = self.get_root_scope();
            (root.id.clone(), root.path.clone())
        };

        let mut parser = calibre_parser::Parser::default();

        let mut tokenizer = Tokenizer::default();
        let program = parser
            .produce_ast(
                tokenizer
                    .tokenize(fs::read_to_string(path).unwrap())
                    .unwrap(),
            )
            .unwrap();

        self.find_checker_scope_at_recursive(root, root + 1, program, target)
    }

    fn find_checker_scope_at_recursive(
        &mut self,
        scope: u64,
        counter: u64,
        node: Node,
        target: Location,
    ) -> ScopeSearchResult {
        if node.span.from >= target.span.from {
            if self.scopes.get(&scope).unwrap().path == target.path {
                return ScopeSearchResult::Success(scope);
            }
        }

        match node.node_type {
            NodeType::IterExpression {
                map: _,
                loop_type: _,
                conditionals: _,
            } => ScopeSearchResult::Failed {
                counter: counter + 1,
                scope,
            },
            NodeType::ScopeDeclaration { body, is_temp } => {
                let (scope, mut counter) = if is_temp {
                    (scope + 1, counter + 1)
                } else {
                    (scope, counter)
                };

                for stmt in body.into_iter() {
                    match self.find_checker_scope_at_recursive(scope, counter, stmt, target.clone())
                    {
                        ScopeSearchResult::Success(x) => return ScopeSearchResult::Success(x),
                        ScopeSearchResult::Failed {
                            counter: c,
                            scope: _,
                        } => counter = c,
                    };
                }

                ScopeSearchResult::Failed { counter, scope }
            }
            NodeType::LoopDeclaration { loop_type: _, body } => {
                match self.find_checker_scope_at_recursive(
                    scope + 1,
                    counter + 1,
                    *body,
                    target.clone(),
                ) {
                    ScopeSearchResult::Success(x) => ScopeSearchResult::Success(x),
                    ScopeSearchResult::Failed { counter, scope: _ } => {
                        ScopeSearchResult::Failed { counter, scope }
                    }
                }
            }
            NodeType::ImplDeclaration {
                identifier: _,
                functions,
            } => {
                let (scope, mut counter) = (scope + 1, counter + 1);

                for stmt in functions.into_iter() {
                    match self.find_checker_scope_at_recursive(
                        scope,
                        counter,
                        stmt.0,
                        target.clone(),
                    ) {
                        ScopeSearchResult::Success(x) => return ScopeSearchResult::Success(x),
                        ScopeSearchResult::Failed {
                            counter: c,
                            scope: _,
                        } => counter = c,
                    };
                }

                ScopeSearchResult::Failed { counter, scope }
            }
            NodeType::ImportStatement {
                module,
                alias,
                values,
            } => {
                if !values.is_empty() || alias.is_some() {
                    ScopeSearchResult::Failed { counter, scope }
                } else {
                    if let Ok(x) = self.get_scope_list(scope, module).clone() {
                        let mut parser = calibre_parser::Parser::default();
                        let mut tokenizer = Tokenizer::default();
                        let program = parser
                            .produce_ast(
                                tokenizer
                                    .tokenize(
                                        fs::read_to_string(
                                            self.scopes.get(&x).unwrap().path.clone(),
                                        )
                                        .unwrap(),
                                    )
                                    .unwrap(),
                            )
                            .unwrap();
                        match self.find_checker_scope_at_recursive(
                            scope + 1,
                            counter + 1,
                            program,
                            target.clone(),
                        ) {
                            ScopeSearchResult::Success(x) => ScopeSearchResult::Success(x),
                            ScopeSearchResult::Failed { counter, scope: _ } => {
                                ScopeSearchResult::Failed { counter, scope }
                            }
                        }
                    } else {
                        ScopeSearchResult::Failed { counter, scope }
                    }
                }
            }
            _ => ScopeSearchResult::Failed { counter, scope },
        }
    }
}
