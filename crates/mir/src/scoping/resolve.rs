use crate::{
    ast::{MiddleNode, MiddleNodeType, MirScopeDecl},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
};
use calibre_parser::{
    Parser,
    ast::nodes::{AstNode, AstNodeType},
};
use std::{fs, print};
use tracing::{debug, instrument};

impl MiddleEnvironment {
    #[instrument(skip_all, fields(list = ?list))]
    pub fn get_scope_list(
        &self,
        scope: ScopeId,
        mut list: Vec<String>,
    ) -> Result<ScopeId, MiddleErr> {
        debug!("getting scope list");
        if list.is_empty() {
            debug!("empty scope list, returning current scope");
            return Ok(scope);
        }
        let first = list.remove(0);
        debug!(next = %first, "navigating to next scope");
        let scope = self.get_next_scope(scope, first.as_str())?;
        self.get_scope_list(scope, list)
    }

    #[instrument(skip_all, fields(list = ?list))]
    pub fn import_scope_list(
        &mut self,
        scope: ScopeId,
        mut list: Vec<String>,
    ) -> Result<(ScopeId, Option<MiddleNode>), MiddleErr> {
        debug!("importing scope list");
        let first = list.remove(0);
        debug!(first = %first, "importing first scope");
        let scope = self.import_next_scope(scope, first.as_str());
        if list.is_empty() {
            scope
        } else {
            self.import_scope_list(scope?.0, list)
        }
    }

    pub fn import_next_scope(
        &mut self,
        scope: ScopeId,
        key: &str,
    ) -> Result<(ScopeId, Option<MiddleNode>), MiddleErr> {
        Ok(match key {
            "super" => {
                let parent = scope
                    .ancestors(&self.scoping.scopes)
                    .nth(1)
                    .ok_or_else(|| MiddleErr::Scope("super".to_string()))?;
                (parent, None)
            }
            _ => {
                let scope = if let Ok(x) = self.scoping.get_scope_from_children(scope, key) {
                    x
                } else if let Some(x) = self
                    .scoping
                    .get_global_scope()
                    .and_then(|scope| self.scoping.get_scope_from_children(scope, key).ok())
                {
                    x
                } else if let Some(x) = self
                    .scoping
                    .get_root_scope()
                    .and_then(|scope| self.scoping.get_scope_from_children(scope, key).ok())
                {
                    x
                } else {
                    self.scoping.new_scope_from_parent(scope, key)?
                };

                self.load_import_scope(scope, scope, key)?
            }
        })
    }

    #[instrument(skip_all, fields(key = %key))]
    fn load_import_scope(
        &mut self,
        scope: ScopeId,
        parent: ScopeId,
        key: &str,
    ) -> Result<(ScopeId, Option<MiddleNode>), MiddleErr> {
        debug!("loading import scope");
        let mut parser = Parser::default();

        let build_node = if let Some(scope) = self.scoping.new_build_scope_from_parent(parent, key)
        {
            if self.scoping.scope_or_err(scope)?.built {
                debug!("scope already loaded, skipping build");
                None
            } else {
                let path = self.scoping.scope_or_err(scope)?.path.clone();
                debug!(path = ?path, "reading source file");
                let source = fs::read_to_string(&path).map_err(|err| {
                    self.context.err_at_current(MiddleErr::Internal(format!(
                        "failed to read {path:?}: {err}"
                    )))
                })?;
                parser.set_source_path(Some(path.clone()));
                let program = parser.produce_ast(&source);

                if !parser.errors.is_empty() {
                    let errors = std::mem::take(&mut parser.errors);
                    return Err(MiddleErr::ParserErrors {
                        path,
                        contents: source,
                        errors,
                    });
                }

                let mut program = match program.node_type {
                    AstNodeType::ScopeDeclaration { body, .. } => AstNode {
                        node_type: AstNodeType::ScopeDeclaration {
                            body,
                            named: None,
                            is_temp: false,
                            create_new_scope: Some(false),
                            define: false,
                        },
                        ..program
                    },
                    _ => program,
                };

                if let AstNodeType::ScopeDeclaration {
                    body: Some(body), ..
                } = &mut program.node_type
                {
                    self.predeclare_nodes(scope, body);
                }

                debug!("evaluating build scope");
                let node = self.evaluate(scope, program);
                self.scoping.scope_mut_or_err(scope)?.built = true;
                Some(node)
            }
        } else {
            None
        };

        if self.scoping.scope_or_err(scope)?.built {
            debug!("scope already loaded");
            return Ok((scope, None));
        }

        let path = self.scoping.scope_or_err(scope)?.path.clone();

        debug!(path = ?path, "reading source file for import");

        let source = fs::read_to_string(&path).map_err(|err| {
            self.context.err_at_current(MiddleErr::Internal(format!(
                "failed to read {path:?}: {err}"
            )))
        })?;

        parser.set_source_path(Some(path.clone()));
        let mut program = parser.produce_ast(&source);

        if !parser.errors.is_empty() {
            let errors = std::mem::take(&mut parser.errors);
            return Err(MiddleErr::ParserErrors {
                path,
                contents: source,
                errors,
            });
        }

        if let AstNodeType::ScopeDeclaration {
            body: Some(body), ..
        } = &mut program.node_type
        {
            self.predeclare_nodes(scope, body);
        }

        debug!("evaluating imported scope");
        let node = self.evaluate(scope, program);
        self.scoping.scope_mut_or_err(scope)?.built = true;

        let node = match (node.node_type.clone(), build_node) {
            (MiddleNodeType::ScopeDeclaration(MirScopeDecl { mut body, .. }), Some(build_node)) => {
                MiddleNode {
                    node_type: MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                        body: {
                            body.insert(0, build_node);
                            body
                        },
                        create_new_scope: true,
                        is_temp: false,
                        scope_id: scope,
                    }),
                    ..node
                }
            }
            (_, Some(build_node)) => MiddleNode::new(
                MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                    body: vec![node, build_node],
                    create_new_scope: false,
                    is_temp: false,
                    scope_id: scope,
                }),
                self.context.current_span(),
            ),
            _ => node,
        };

        Ok((scope, Some(node)))
    }

    pub fn get_next_scope(&self, scope: ScopeId, key: &str) -> Result<ScopeId, MiddleErr> {
        Ok(match key {
            "super" => scope
                .ancestors(&self.scoping.scopes)
                .nth(1)
                .ok_or_else(|| {
                    self.context
                        .err_at_current(MiddleErr::Scope("super".to_string()))
                })?,
            _ => {
                if let Ok(x) = self.scoping.get_scope_from_children(scope, key) {
                    x
                } else if let Some(x) = self
                    .scoping
                    .get_global_scope()
                    .and_then(|scope| self.scoping.get_scope_from_children(scope, key).ok())
                {
                    x
                } else if let Some(x) = self
                    .scoping
                    .get_root_scope()
                    .and_then(|scope| self.scoping.get_scope_from_children(scope, key).ok())
                {
                    x
                } else {
                    return Err(self
                        .context
                        .err_at_current(MiddleErr::Scope(key.to_string())));
                }
            }
        })
    }
}
