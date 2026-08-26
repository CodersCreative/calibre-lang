use crate::{
    ast::{MiddleNode, MiddleNodeType},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::{ScopeId, ScopeMacro},
    symbols::resolve::ResolutionOptions,
};
use calibre_parser::{
    Span,
    ast::{
        idents::{ParserText, PotentialDollarIdentifier},
        nodes::{AstNode, AstNodeType, LoopType, NamedScope},
    },
};

impl MiddleEnvironment {
    fn ends_in_control_flow(node: &MiddleNode) -> bool {
        match &node.node_type {
            MiddleNodeType::Break { .. }
            | MiddleNodeType::Continue { .. }
            | MiddleNodeType::Return { .. } => true,
            MiddleNodeType::ScopeDeclaration { body, .. } => {
                body.last().is_some_and(Self::ends_in_control_flow)
            }
            _ => false,
        }
    }

    pub fn evaluate_scope_alias(
        &mut self,
        scope: ScopeId,
        span: Span,
        identifier: PotentialDollarIdentifier,
        value: NamedScope,
        create_new_scope: Option<bool>,
    ) -> Result<MiddleNode, MiddleErr> {
        let identifer = self.resolve(
            scope,
            &identifier,
            ResolutionOptions::default().with_dollar(),
        )?;

        let name = self.resolve(
            scope,
            &value.name,
            ResolutionOptions::default().with_dollar(),
        )?;

        let scope_macro = self
            .scoping
            .resolve_macro(scope, &name)
            .cloned()
            .ok_or_else(|| MiddleErr::At(span, Box::new(MiddleErr::Scope(name.clone()))))?;
        let mut args = Vec::new();

        let mut added = Vec::new();

        for arg in value.args {
            added.push(self.resolve(scope, &arg.0, ResolutionOptions::default().with_dollar())?);
            args.push(arg);
        }

        for arg in scope_macro.args {
            let arg_text =
                self.resolve(scope, &arg.0, ResolutionOptions::default().with_dollar())?;
            if !added.contains(&arg_text) {
                added.push(arg_text.clone());
                args.push(arg);
            }
        }

        let scope_macro = ScopeMacro {
            name: name.clone(),
            args,
            create_new_scope: create_new_scope.unwrap_or(scope_macro.create_new_scope),
            ..scope_macro
        };

        self.scoping
            .scope_mut_or_err(scope)?
            .macros
            .insert(identifer, scope_macro);

        Ok(MiddleNode {
            node_type: MiddleNodeType::EmptyLine,
            span,
        })
    }

    pub fn evaluate_scope_declaration(
        &mut self,
        scope: ScopeId,
        mut body: Option<Vec<AstNode>>,
        named: Option<NamedScope>,
        create_new_scope: Option<bool>,
        define: bool,
        is_temp: bool,
    ) -> Result<MiddleNode, MiddleErr> {
        let span = self.context.current_span();
        let mut stmts = Vec::new();
        let mut og_create_new_scope = create_new_scope;
        let mut create_new_scope = create_new_scope.unwrap_or(true);
        let mut macro_args_to_insert: Vec<(String, AstNode)> = Vec::new();

        if let Some(named) = named {
            if define {
                let name = self.resolve(
                    scope,
                    &named.name,
                    ResolutionOptions::default().with_dollar(),
                )?;

                let scope_macro = ScopeMacro {
                    name: name.clone(),
                    args: named.args.clone(),
                    body: body.clone().unwrap_or_default(),
                    create_new_scope: og_create_new_scope.unwrap_or(create_new_scope),
                };

                self.scoping
                    .scope_mut_or_err(scope)?
                    .macros
                    .insert(name, scope_macro);

                return Ok(MiddleNode {
                    node_type: MiddleNodeType::EmptyLine,
                    span,
                });
            }

            let name = self.resolve(
                scope,
                &named.name,
                ResolutionOptions::default().with_dollar(),
            )?;
            if self.scoping.resolve_macro(scope, &name).is_none() {
                if !named.args.is_empty() {
                    let scope_macro = ScopeMacro {
                        name: name.clone(),
                        args: named.args.clone(),
                        body: body.clone().unwrap_or_default(),
                        create_new_scope,
                    };
                    self.scoping
                        .scope_mut_or_err(scope)?
                        .macros
                        .insert(name.clone(), scope_macro);
                }

                let mut body_nodes = body.unwrap_or_default();
                let last = body_nodes.pop();
                let break_value = last.map(Box::new);
                body_nodes.push(AstNode::new(
                    span,
                    AstNodeType::Break {
                        label: Some(named.name.clone()),
                        value: break_value,
                    },
                ));

                let loop_body =
                    AstNode::new_temp_scope_with_create(body_nodes, Some(create_new_scope));

                return self.evaluate_loop_statement(
                    scope,
                    LoopType::Loop,
                    loop_body,
                    None,
                    Some(named.name),
                    Some(Box::new(AstNode::new(span, AstNodeType::Null))),
                );
            }
            let mut added = Vec::new();

            let scope_macro_args: Vec<(PotentialDollarIdentifier, AstNode)> = {
                let scope_macro = self
                    .scoping
                    .resolve_macro(scope, &name)
                    .ok_or_else(|| MiddleErr::At(span, Box::new(MiddleErr::Scope(name.clone()))))?;
                if og_create_new_scope.is_none() {
                    og_create_new_scope = Some(scope_macro.create_new_scope);
                }
                body = Some(scope_macro.body.clone());
                scope_macro.args.clone()
            };

            for arg in named.args {
                let arg_text =
                    self.resolve(scope, &arg.0, ResolutionOptions::default().with_dollar())?;
                added.push(arg_text.clone());
                macro_args_to_insert.push((arg_text, arg.1));
            }

            for arg in scope_macro_args {
                let arg_text =
                    self.resolve(scope, &arg.0, ResolutionOptions::default().with_dollar())?;
                if !added.contains(&arg_text) {
                    added.push(arg_text.clone());
                    macro_args_to_insert.push((arg_text, arg.1));
                }
            }
        }

        if let Some(og) = og_create_new_scope {
            create_new_scope = og;
        }

        let new_scope = if create_new_scope && !define {
            self.scoping.new_scope_from_parent_shallow(scope)
        } else {
            scope
        };

        if !macro_args_to_insert.is_empty() {
            let scope_data = self.scoping.scope_mut_or_err(new_scope)?;
            for (key, value) in macro_args_to_insert {
                scope_data.macro_args.insert(key, value);
            }
        }

        if let Some(mut body) = body {
            for stmt in body.iter() {
                if let AstNodeType::VariableDeclaration {
                    identifier, value, ..
                } = &stmt.node_type
                    && matches!(value.node_type, AstNodeType::FunctionDeclaration { .. })
                {
                    let ident = self.resolve(
                        new_scope,
                        identifier,
                        ResolutionOptions::default().with_dollar(),
                    )?;
                    let new_name = ParserText::temp_name_with_suffix(ident.trim(), span).text;
                    self.scoping
                        .scope_mut_or_err(new_scope)?
                        .mappings
                        .entry(ident.clone())
                        .or_insert(new_name);
                }
            }

            if is_temp {
                let last = body.pop();
                for statement in body.into_iter() {
                    stmts.push(self.evaluate(new_scope, statement));
                }

                let last = last.map(|x| self.evaluate(new_scope, x));

                if !last.as_ref().is_some_and(Self::ends_in_control_flow) {
                    for x in self.scoping.scope_or_err(new_scope)?.defers.clone() {
                        stmts.push(self.evaluate(new_scope, x));
                    }
                }

                if let Some(last) = last {
                    stmts.push(last);
                }
            } else {
                for statement in body.into_iter() {
                    if let Ok(x) = self.evaluate_inner(new_scope, statement.clone()) {
                        stmts.push(x);
                    }
                }
            }
        }

        if new_scope != scope && !og_create_new_scope.unwrap_or(create_new_scope) {
            let (mappings, macros) = {
                let scope = self.scoping.scope_or_err(new_scope)?;
                (scope.mappings.clone(), scope.macros.clone())
            };

            for mapping in mappings {
                self.scoping
                    .scope_mut_or_err(scope)?
                    .mappings
                    .insert(mapping.0, mapping.1);
            }

            for scope_macro in macros {
                self.scoping
                    .scope_mut_or_err(scope)?
                    .macros
                    .insert(scope_macro.0, scope_macro.1);
            }
        }

        Ok(MiddleNode {
            node_type: MiddleNodeType::ScopeDeclaration {
                body: {
                    stmts
                        .into_iter()
                        .filter(|x| x.node_type != MiddleNodeType::EmptyLine)
                        .collect()
                },
                is_temp,
                create_new_scope: og_create_new_scope.unwrap_or(create_new_scope),
                scope_id: new_scope,
            },
            span,
        })
    }
}
