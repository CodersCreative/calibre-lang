use crate::{
    ast::{MiddleNode, MiddleNodeType, MirScopeDecl, types::MirDataType},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::{ScopeContext, ScopeId, ScopeMacro},
    symbols::{
        VariableKey,
        resolve::{KeyOrAstNode, ResolutionOptions},
    },
    translate::MirLowering,
};
use calibre_parser::{
    Span,
    ast::{
        idents::PotentialDollarIdentifier,
        nodes::{AstNode, VarType, scopes::AstScopeDef},
        types::{ParserDataType, ParserInnerType},
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
        mut data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        let mut macro_args_to_insert: Vec<(Ustr, AstNode)> = Vec::new();

        let in_temp_scope = env.context.in_temp_scope;
        env.context.in_temp_scope = self.is_temp;

        if let Some(named) = self.named.as_ref() {
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

            if let Some(scope_macro_args) = env.scoping.resolve_macro(scope, &name).map(|x| {
                self.body = x.body.clone();
                x.args.clone()
            }) {
                for arg in &named.args {
                    let arg_text = env
                        .resolve(scope, &arg.0, ResolutionOptions::default().with_dollar())?
                        .unwrap_dollar();

                    added.push(arg_text);
                    macro_args_to_insert.push((arg_text, arg.1.clone()));
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
        }

        data_type = data_type.or_else(|| self.type_of(env, scope, span));

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

        let mut result_target_decl = None;

        let label = if self.is_temp
            && !self.define
            && let Some(named) = self.named.as_ref()
            && !named.used_by_loop
            && let Some(label) = env
                .resolve(
                    new_scope,
                    &named.name,
                    ResolutionOptions::default().with_dollar(),
                )
                .ok()
                .map(|r| r.unwrap_dollar())
        {
            let scope_ref = env.scoping.scope_mut_or_err(new_scope)?;
            scope_ref
                .mappings
                .insert(label, VariableKey::from(label.as_str()));

            let result_target = Ustr::from(&env.context.get_temp("scope_result"));

            env.scoping.scope_stack.push(ScopeContext {
                label: Some(label),
                result_target: Some(result_target),
                data_type: data_type.clone(),
                is_function_body: self.is_function_body,
                scope_id: new_scope,
            });

            let data_type = data_type
                .clone()
                .map(|t| ParserDataType::new(span, ParserInnerType::Option(Box::new(t.into()))))
                .unwrap_or(ParserDataType::auto(span));

            result_target_decl = Some(
                AstNode::var_decl(
                    span,
                    PotentialDollarIdentifier::new(span, result_target),
                    VarType::Mutable,
                    AstNode::none(span),
                    data_type,
                )
                .lower_or_empty(env, scope, span, None),
            );

            Some(label)
        } else {
            env.scoping.scope_stack.push(ScopeContext {
                label: None,
                result_target: None,
                is_function_body: self.is_function_body,
                data_type: data_type.clone(),
                scope_id: new_scope,
            });

            None
        };

        let mut stmts = Vec::new();

        if self.is_temp {
            let no_defers = self.body.last().is_some_and(|x| x.is_emit());
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

            if !no_defers {
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
            }
        } else {
            for statement in self.body.into_iter() {
                let x = statement
                    .clone()
                    .lower_or_empty(env, new_scope, statement.span, None);

                while !env.monomorphizer.awaiting_lowering.is_empty() {
                    let awaiting_lowering =
                        std::mem::take(&mut env.monomorphizer.awaiting_lowering);

                    for (node, scope) in awaiting_lowering {
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

        let result_target = env.scoping.scope_stack.pop().and_then(|x| x.result_target);

        let result_target = result_target.map(|x| {
            AstNode::emit(AstNode::member(span, AstNode::identifier(span, x), "Some"))
                .lower_or_empty(env, scope, span, data_type)
        });

        env.context.in_temp_scope = in_temp_scope;

        let stmts = MiddleNode {
            node_type: MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                label,
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
        };

        if let (Some(decl), Some(access)) = (result_target_decl.clone(), result_target.clone()) {
            Ok(MiddleNode {
                node_type: MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                    label: None,
                    body: Box::new([decl, stmts, access]),
                    scope_id: scope,
                    create_new_scope: false,
                    is_temp: self.is_temp,
                    function_body: false,
                }),
                span,
            })
        } else {
            Ok(stmts)
        }
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
            None
        }
    }
}
