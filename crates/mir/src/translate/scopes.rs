use crate::{
    ast::{MiddleNode, MiddleNodeType, MirScopeDecl, types::MirDataType},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::{ScopeId, ScopeMacro},
    symbols::resolve::{KeyOrAstNode, ResolutionOptions},
    translate::MirLowering,
};
use calibre_parser::{
    Span,
    ast::{
        idents::PotentialDollarIdentifier,
        nodes::{AstNode, scopes::AstScopeDef},
    },
};
use tracing::instrument;
use ustr::Ustr;

impl MirLowering for AstScopeDef {
    #[instrument(skip_all)]
    fn lower(
        mut self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        let mut stmts = Vec::new();
        let mut macro_args_to_insert: Vec<(Ustr, AstNode)> = Vec::new();

        let in_temp_scope = env.context.in_temp_scope;
        env.context.in_temp_scope = self.is_temp;

        if let Some(named) = self.named {
            let name = env
                .resolve(
                    scope,
                    &named.name,
                    ResolutionOptions::default().with_dollar(),
                )?
                .unwrap_dollar();

            if self.define {
                let scope_macro = ScopeMacro {
                    name,
                    args: named.args.clone(),
                    body: self.body.clone(),
                    create_new_scope: self.create_new_scope,
                };

                env.scoping
                    .scope_mut_or_err(scope)?
                    .macros
                    .insert(name, scope_macro);

                env.context.in_temp_scope = in_temp_scope;

                return Ok(MiddleNode {
                    node_type: MiddleNodeType::EmptyLine,
                    span,
                });
            }

            let mut added = Vec::new();

            let scope_macro_args: Vec<(PotentialDollarIdentifier, AstNode)> = {
                let scope_macro = env.scoping.resolve_macro(scope, &name).ok_or_else(|| {
                    MiddleErr::At(span, Box::new(MiddleErr::Scope(name.to_string())))
                })?;

                self.body = scope_macro.body.clone();
                scope_macro.args.clone()
            };

            for arg in named.args {
                let arg_text = env
                    .resolve(scope, &arg.0, ResolutionOptions::default().with_dollar())?
                    .unwrap_dollar();

                added.push(arg_text);
                macro_args_to_insert.push((arg_text, arg.1));
            }

            for arg in scope_macro_args {
                let arg_text = env
                    .resolve(scope, &arg.0, ResolutionOptions::default().with_dollar())?
                    .unwrap_dollar();

                if !added.contains(&arg_text) {
                    added.push(arg_text);
                    macro_args_to_insert.push((arg_text, arg.1));
                }
            }
        }

        let new_scope = if self.create_new_scope && !self.define {
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

        if self.is_temp {
            for statement in self.body {
                let span = statement.span;
                let is_emit = statement.is_emit();
                stmts.extend(
                    statement
                        .lower_or_empty(
                            env,
                            new_scope,
                            span,
                            if is_emit { data_type.clone() } else { None },
                        )
                        .nodes_if_no_new_scope(),
                );
            }

            for x in env.scoping.scope_or_err(new_scope)?.defers.clone() {
                let span = x.span;
                stmts.extend(
                    x.lower_or_empty(env, new_scope, span, None)
                        .nodes_if_no_new_scope(),
                );
            }

            if self.is_function_body {
                for x in env.symbols.function_defers.clone() {
                    let span = x.span;
                    stmts.extend(
                        x.lower_or_empty(env, new_scope, span, None)
                            .nodes_if_no_new_scope(),
                    );
                }
            }
        } else {
            for statement in self.body.into_iter() {
                let x = statement
                    .clone()
                    .lower_or_empty(env, new_scope, statement.span, None);

                if !env.monomorphizer.awaiting_lowering.is_empty() {
                    for (node, scope) in std::mem::take(&mut env.monomorphizer.awaiting_lowering) {
                        stmts.extend(
                            node.lower_or_empty(env, scope, span, None)
                                .nodes_if_isnt_temp(),
                        );
                    }
                }

                stmts.extend(x.nodes_if_isnt_temp());
            }
        }

        if new_scope != scope && !self.create_new_scope {
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

        env.context.in_temp_scope = in_temp_scope;

        Ok(MiddleNode {
            node_type: MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                body: {
                    stmts
                        .into_iter()
                        .filter(|x| x.node_type != MiddleNodeType::EmptyLine)
                        .collect()
                },
                is_temp: self.is_temp,
                create_new_scope: self.create_new_scope,
                scope_id: new_scope,
                function_body: false,
            }),
            span,
        })
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<MirDataType> {
        if self.define {
            None
        } else if !self.body.is_empty() {
            let mut typ = None;

            for node in &self.body {
                typ = env.resolve_emit_type_from_node(scope, node);
                if typ.is_some() {
                    break;
                }
            }

            typ
        } else if let Some(named) = &self.named
            && let KeyOrAstNode::Node(node) = env
                .resolve_potential_node(
                    scope,
                    &named.name,
                    ResolutionOptions::default().with_dollar(),
                )
                .ok()?
        {
            node.type_of(env, scope, span)
        } else {
            unreachable!()
        }
    }
}
