use calibre_parser::{
    ast::{NamedScope, Node, PotentialDollarIdentifier},
    lexer::Span,
};

use crate::{
    ast::{MiddleNode, MiddleNodeType},
    environment::{MiddleEnvironment, ScopeMacro},
    errors::MiddleErr,
};

impl MiddleEnvironment {
    pub fn evaluate_scope_alias(
        &mut self,
        scope: &u64,
        span: Span,
        identifier: PotentialDollarIdentifier,
        value: NamedScope,
        create_new_scope: Option<bool>,
    ) -> Result<MiddleNode, MiddleErr> {
        let identifer = self
            .resolve_dollar_ident_only(scope, &identifier)
            .unwrap()
            .text;

        let name = self
            .resolve_dollar_ident_only(scope, &value.name)
            .unwrap()
            .text;

        let scope_macro = self.resolve_macro(scope, &name).unwrap().clone();
        let mut args = Vec::new();

        let mut added = Vec::new();

        for arg in value.args {
            let arg_text = self.resolve_dollar_ident_only(scope, &arg.0).unwrap();
            added.push(arg_text.text.clone());
            args.push(arg);
        }

        for arg in scope_macro.args {
            let arg_text = self.resolve_dollar_ident_only(scope, &arg.0).unwrap();
            if !added.contains(&arg_text) {
                added.push(arg_text.text.clone());
                args.push(arg);
            }
        }

        let scope_macro = ScopeMacro {
            name: name.clone(),
            args,
            create_new_scope: create_new_scope.unwrap_or(scope_macro.create_new_scope),
            ..scope_macro
        };

        self.scopes
            .get_mut(scope)
            .unwrap()
            .macros
            .insert(identifer, scope_macro);

        Ok(MiddleNode {
            node_type: MiddleNodeType::EmptyLine,
            span,
        })
    }

    pub fn evaluate_scope_declaration(
        &mut self,
        scope: &u64,
        span: Span,
        mut body: Option<Vec<Node>>,
        named: Option<NamedScope>,
        create_new_scope: Option<bool>,
        define: bool,
        is_temp: bool,
    ) -> Result<MiddleNode, MiddleErr> {
        let mut stmts = Vec::new();
        let mut og_create_new_scope = create_new_scope;
        let mut create_new_scope = create_new_scope.unwrap_or(true);
        let mut macro_args_to_insert: Vec<(String, Node)> = Vec::new();

        if let Some(named) = named {
            if define {
                let name = self
                    .resolve_dollar_ident_only(scope, &named.name)
                    .unwrap()
                    .text;

                let scope_macro = ScopeMacro {
                    name: name.clone(),
                    args: named.args.clone(),
                    body: body.clone().unwrap_or_default(),
                    create_new_scope: og_create_new_scope.unwrap_or(create_new_scope),
                };

                self.scopes
                    .get_mut(scope)
                    .unwrap()
                    .macros
                    .insert(name, scope_macro);

                return Ok(MiddleNode {
                    node_type: MiddleNodeType::EmptyLine,
                    span,
                });
            }

            let name = self
                .resolve_dollar_ident_only(scope, &named.name)
                .unwrap()
                .text;
            let mut added = Vec::new();

            let scope_macro_args: Vec<(PotentialDollarIdentifier, Node)> = {
                let scope_macro = self.resolve_macro(scope, &name).unwrap();
                if og_create_new_scope.is_none() {
                    og_create_new_scope = Some(scope_macro.create_new_scope);
                }
                body = Some(scope_macro.body.clone());
                scope_macro.args.clone()
            };

            for arg in named.args {
                let arg_text = self.resolve_dollar_ident_only(scope, &arg.0).unwrap();
                added.push(arg_text.text.clone());
                macro_args_to_insert.push((arg_text.text, arg.1));
            }

            for arg in scope_macro_args {
                let arg_text = self.resolve_dollar_ident_only(scope, &arg.0).unwrap();
                if !added.contains(&arg_text) {
                    added.push(arg_text.text.clone());
                    macro_args_to_insert.push((arg_text.text, arg.1));
                }
            }
        }

        if let Some(og) = og_create_new_scope {
            create_new_scope = og;
        }

        let new_scope = if create_new_scope && !define {
            self.new_scope_from_parent_shallow(*scope)
        } else {
            *scope
        };

        if !macro_args_to_insert.is_empty() {
            let scope_data = self.scopes.get_mut(&new_scope).unwrap();
            for (key, value) in macro_args_to_insert {
                scope_data.macro_args.insert(key, value);
            }
        }

        if let Some(mut body) = body {
            if is_temp {
                let last = body.pop();
                for statement in body.into_iter() {
                    stmts.push(self.evaluate(&new_scope, statement));
                }

                let mut in_return = Vec::new();

                let last = if let Some(x) = last {
                    let x = self.evaluate(&new_scope, x);
                    in_return = x
                        .identifiers_used()
                        .into_iter()
                        .map(|x| x.to_string())
                        .collect();
                    Some(x)
                } else {
                    None
                };

                for x in self.scopes.get(&new_scope).unwrap().defers.clone() {
                    stmts.push(self.evaluate(&new_scope, x));
                }

                for value in self
                    .scopes
                    .get(&new_scope)
                    .unwrap()
                    .defined
                    .clone()
                    .into_iter()
                    .filter(|x| !in_return.contains(x))
                {
                    stmts.push(MiddleNode::new(
                        MiddleNodeType::Drop(value.into()),
                        self.current_span(),
                    ))
                }

                if let Some(last) = last {
                    stmts.push(last);
                }
            } else {
                let mut errored = Vec::new();

                for statement in body.into_iter() {
                    if let Ok(x) = self.evaluate_inner(&new_scope, statement.clone()) {
                        stmts.push(x);
                    } else {
                        errored.push(statement);
                    }
                }

                let mut last_len = 0;
                while !errored.is_empty() && last_len != errored.len() {
                    last_len = errored.len();

                    for elem in errored.clone() {
                        if let Ok(x) = self.evaluate_inner(&new_scope, elem.clone()) {
                            stmts.push(x);
                            errored.retain(|x| x != &elem);
                        }
                    }
                }

                for val in errored {
                    let _ = self.evaluate(&new_scope, val);
                }
            }
        }

        if &new_scope != scope && !og_create_new_scope.unwrap() {
            let (mappings, macros) = {
                let scope = self.scopes.get(&new_scope).unwrap();
                (scope.mappings.clone(), scope.macros.clone())
            };

            for mapping in mappings {
                self.scopes
                    .get_mut(scope)
                    .unwrap()
                    .mappings
                    .insert(mapping.0, mapping.1);
            }

            for scope_macro in macros {
                self.scopes
                    .get_mut(scope)
                    .unwrap()
                    .macros
                    .insert(scope_macro.0, scope_macro.1);
            }
        }

        Ok(MiddleNode {
            node_type: MiddleNodeType::ScopeDeclaration {
                body: {
                    let mut body: Vec<MiddleNode> = stmts
                        .into_iter()
                        .filter(|x| x.node_type != MiddleNodeType::EmptyLine)
                        .collect();
                    let defined = self
                        .scopes
                        .get(&new_scope)
                        .map(|s| s.defined.clone())
                        .unwrap_or_default();
                    let defers_empty = self
                        .scopes
                        .get(&new_scope)
                        .map(|s| s.defers.is_empty())
                        .unwrap_or(true);
                    if defers_empty {
                        let mut protected_tail = Vec::new();
                        if is_temp && let Some(last) = body.last() {
                            protected_tail = last
                                .identifiers_used()
                                .into_iter()
                                .map(|x| x.to_string())
                                .collect();
                        }
                        self.insert_auto_drops(&mut body, &defined, &protected_tail);
                    }
                    if is_temp && body.len() > 1 {
                        let mut trailing = Vec::new();
                        while matches!(
                            body.last().map(|n| &n.node_type),
                            Some(MiddleNodeType::Drop(_))
                        ) {
                            trailing.push(body.pop().unwrap());
                        }
                        if !trailing.is_empty() {
                            let insert_at = body.len().saturating_sub(1);
                            for drop_node in trailing.into_iter().rev() {
                                body.insert(insert_at, drop_node);
                            }
                        }
                    }
                    body
                },
                is_temp,
                create_new_scope: og_create_new_scope.unwrap(),
                scope_id: new_scope,
            },
            span,
        })
    }
}
