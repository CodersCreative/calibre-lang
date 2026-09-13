use crate::{
    ast::{MiddleNode, MiddleNodeType, MirScopeDecl},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::{ScopeId, ScopeMacro},
    symbols::resolve::ResolutionOptions,
    translate::MirLowering,
};
use calibre_parser::{
    Span,
    ast::{
        idents::{ParserText, PotentialDollarIdentifier},
        nodes::{
            AstNode, AstNodeType,
            declaration::AstDeclaration,
            flow::AstBreak,
            loops::{AstLoop, LoopType},
            scopes::{AstScopeAlias, AstScopeDef},
        },
        types::ParserDataType,
    },
};
use ustr::Ustr;

impl MiddleEnvironment {
    fn ends_in_control_flow(node: &MiddleNode) -> bool {
        match &node.node_type {
            MiddleNodeType::Break { .. }
            | MiddleNodeType::Continue { .. }
            | MiddleNodeType::Return { .. } => true,
            MiddleNodeType::ScopeDeclaration(MirScopeDecl { body, .. }) => {
                body.last().is_some_and(Self::ends_in_control_flow)
            }
            _ => false,
        }
    }
}

impl MirLowering for AstScopeDef {
    fn lower(
        mut self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        let mut stmts = Vec::new();
        let mut create_new_scope = self.create_new_scope.unwrap_or(true);
        let mut macro_args_to_insert: Vec<(Ustr, AstNode)> = Vec::new();

        if let Some(named) = self.named {
            if self.define {
                let name = env.resolve(
                    scope,
                    &named.name,
                    ResolutionOptions::default().with_dollar(),
                )?;

                let scope_macro = ScopeMacro {
                    name,
                    args: named.args.clone(),
                    body: self.body.clone().unwrap_or_default(),
                    create_new_scope: self.create_new_scope.unwrap_or(create_new_scope),
                };

                env.scoping
                    .scope_mut_or_err(scope)?
                    .macros
                    .insert(name, scope_macro);

                return Ok(MiddleNode {
                    node_type: MiddleNodeType::EmptyLine,
                    span,
                });
            }

            let name = env.resolve(
                scope,
                &named.name,
                ResolutionOptions::default().with_dollar(),
            )?;

            if env.scoping.resolve_macro(scope, &name).is_none() {
                if !named.args.is_empty() {
                    let scope_macro = ScopeMacro {
                        name,
                        args: named.args.clone(),
                        body: self.body.clone().unwrap_or_default(),
                        create_new_scope,
                    };
                    env.scoping
                        .scope_mut_or_err(scope)?
                        .macros
                        .insert(name, scope_macro);
                }

                let mut body_nodes = self.body.unwrap_or_default();
                let last = body_nodes.pop();
                let break_value = last.map(Box::new);

                body_nodes.push(AstNode::new(
                    span,
                    AstNodeType::Break(AstBreak {
                        label: Some(named.name.clone()),
                        value: break_value,
                    }),
                ));

                return AstLoop {
                    loop_type: Box::new(LoopType::Loop),
                    body: Box::new(AstNode::new_temp_scope_with_create(
                        body_nodes,
                        Some(create_new_scope),
                    )),
                    until: None,
                    label: Some(named.name),
                    else_body: Some(Box::new(AstNode::new(span, AstNodeType::Null))),
                }
                .lower(env, scope, span);
            }
            let mut added = Vec::new();

            let scope_macro_args: Vec<(PotentialDollarIdentifier, AstNode)> = {
                let scope_macro = env.scoping.resolve_macro(scope, &name).ok_or_else(|| {
                    MiddleErr::At(span, Box::new(MiddleErr::Scope(name.to_string())))
                })?;

                if self.create_new_scope.is_none() {
                    self.create_new_scope = Some(scope_macro.create_new_scope);
                }

                self.body = Some(scope_macro.body.clone());
                scope_macro.args.clone()
            };

            for arg in named.args {
                let arg_text =
                    env.resolve(scope, &arg.0, ResolutionOptions::default().with_dollar())?;
                added.push(arg_text);
                macro_args_to_insert.push((arg_text, arg.1));
            }

            for arg in scope_macro_args {
                let arg_text =
                    env.resolve(scope, &arg.0, ResolutionOptions::default().with_dollar())?;
                if !added.contains(&arg_text) {
                    added.push(arg_text);
                    macro_args_to_insert.push((arg_text, arg.1));
                }
            }
        }

        if let Some(og) = self.create_new_scope {
            create_new_scope = og;
        }

        let new_scope = if create_new_scope && !self.define {
            env.scoping.new_scope_from_parent_shallow(scope)
        } else {
            scope
        };

        if !macro_args_to_insert.is_empty() {
            let scope_data = env.scoping.scope_mut_or_err(new_scope)?;
            for (key, value) in macro_args_to_insert {
                scope_data.macro_args.insert(key, value);
            }
        }

        if let Some(mut body) = self.body {
            for stmt in body.iter() {
                if let AstNodeType::VariableDeclaration(AstDeclaration {
                    identifier, value, ..
                }) = &stmt.node_type
                    && matches!(value.node_type, AstNodeType::FunctionDeclaration { .. })
                {
                    let ident = env.resolve(
                        new_scope,
                        identifier,
                        ResolutionOptions::default().with_dollar(),
                    )?;

                    let new_name =
                        Ustr::from(&ParserText::temp_name_with_suffix(ident.trim(), span).text);

                    env.scoping
                        .scope_mut_or_err(new_scope)?
                        .mappings
                        .entry(ident)
                        .or_insert(new_name);
                }
            }

            if self.is_temp {
                let last = body.pop();
                for statement in body.into_iter() {
                    stmts.push(statement.lower_or_empty(env, new_scope, span));
                }

                let last = last.map(|x| x.lower_or_empty(env, new_scope, span));

                if !last
                    .as_ref()
                    .is_some_and(MiddleEnvironment::ends_in_control_flow)
                {
                    for x in env.scoping.scope_or_err(new_scope)?.defers.clone() {
                        stmts.push(x.lower_or_empty(env, new_scope, span));
                    }
                }

                if let Some(last) = last {
                    stmts.push(last);
                }
            } else {
                for statement in body.into_iter() {
                    if let Ok(x) = statement.clone().lower(env, new_scope, span) {
                        stmts.push(x);
                    }
                }
            }
        }

        if new_scope != scope && !self.create_new_scope.unwrap_or(create_new_scope) {
            let (mappings, macros) = {
                let scope = env.scoping.scope_or_err(new_scope)?;
                (scope.mappings.clone(), scope.macros.clone())
            };

            for mapping in mappings {
                env.scoping
                    .scope_mut_or_err(scope)?
                    .mappings
                    .insert(mapping.0, mapping.1);
            }

            for scope_macro in macros {
                env.scoping
                    .scope_mut_or_err(scope)?
                    .macros
                    .insert(scope_macro.0, scope_macro.1);
            }
        }

        Ok(MiddleNode {
            node_type: MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                body: {
                    stmts
                        .into_iter()
                        .filter(|x| x.node_type != MiddleNodeType::EmptyLine)
                        .collect()
                },
                is_temp: self.is_temp,
                create_new_scope: self.create_new_scope.unwrap_or(create_new_scope),
                scope_id: new_scope,
            }),
            span,
        })
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        if self.define {
            None
        } else if let Some(body) = &self.body {
            let mut typ = None;

            for node in body {
                typ = env.resolve_emit_type_from_node(scope, node);
                if typ.is_some() {
                    break;
                }
            }

            typ
        } else if let Some(named) = &self.named {
            let name = env
                .resolve(
                    scope,
                    &named.name,
                    ResolutionOptions::default().with_dollar(),
                )
                .ok()?;

            let resolved = env
                .scoping
                .resolve_macro(scope, &name)?
                .body
                .last()?
                .clone();

            resolved.type_of(env, scope, span)
        } else {
            unreachable!()
        }
    }
}

impl MirLowering for AstScopeAlias {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        let identifer = env.resolve(
            scope,
            &self.identifier,
            ResolutionOptions::default().with_dollar(),
        )?;

        let name = env.resolve(
            scope,
            &self.value.name,
            ResolutionOptions::default().with_dollar(),
        )?;

        let scope_macro = env
            .scoping
            .resolve_macro(scope, &name)
            .cloned()
            .ok_or_else(|| MiddleErr::At(span, Box::new(MiddleErr::Scope(name.to_string()))))?;

        let mut args = Vec::new();
        let mut added = Vec::new();

        for arg in self.value.args {
            added.push(env.resolve(scope, &arg.0, ResolutionOptions::default().with_dollar())?);
            args.push(arg);
        }

        for arg in scope_macro.args {
            let arg_text =
                env.resolve(scope, &arg.0, ResolutionOptions::default().with_dollar())?;

            if !added.contains(&arg_text) {
                added.push(arg_text);
                args.push(arg);
            }
        }

        let scope_macro = ScopeMacro {
            name,
            args,
            create_new_scope: self
                .create_new_scope
                .unwrap_or(scope_macro.create_new_scope),
            ..scope_macro
        };

        env.scoping
            .scope_mut_or_err(scope)?
            .macros
            .insert(identifer, scope_macro);

        Ok(MiddleNode {
            node_type: MiddleNodeType::EmptyLine,
            span,
        })
    }
}
