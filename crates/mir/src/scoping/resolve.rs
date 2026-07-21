impl MiddleEnvironment {
    pub fn get_scope_list(&self, scope: u64, mut list: Vec<String>) -> Result<u64, MiddleErr> {
        if list.len() <= 0 {
            return Ok(scope);
        }
        let first = list.remove(0);
        let scope = self.get_next_scope(scope, first.as_str())?;
        self.get_scope_list(scope, list)
    }

    pub fn import_scope_list(
        &mut self,
        scope: u64,
        mut list: Vec<String>,
    ) -> Result<(u64, Option<MiddleNode>), MiddleErr> {
        let first = list.remove(0);
        let scope = self.import_next_scope(scope, first.as_str());
        if list.is_empty() {
            scope
        } else {
            self.import_scope_list(scope?.0, list)
        }
    }

    pub fn import_next_scope(
        &mut self,
        scope: u64,
        key: &str,
    ) -> Result<(u64, Option<MiddleNode>), MiddleErr> {
        Ok(match key {
            "super" => {
                let parent = self
                    .scopes
                    .get(&scope)
                    .and_then(|s| s.parent)
                    .ok_or_else(|| self.err_at_current(MiddleErr::Scope("super".to_string())))?;
                (parent, None)
            }
            _ => {
                let current = self
                    .scopes
                    .get(&scope)
                    .cloned()
                    .ok_or_else(|| self.err_at_current(MiddleErr::Scope(scope.to_string())))?;
                let parent_id = current.id;

                let scope = if let Some(scope) = current.children.get(key) {
                    *scope
                } else if let Some(scope) = self.get_global_scope().children.get(key) {
                    *scope
                } else {
                    self.new_scope_from_parent(parent_id, key)?
                };

                self.load_import_scope(scope, parent_id, key)?
            }
        })
    }

    fn load_import_scope(
        &mut self,
        scope: u64,
        parent: u64,
        key: &str,
    ) -> Result<(u64, Option<MiddleNode>), MiddleErr> {
        let mut parser = Parser::default();
        let build_node = if let Some(scope) = self.new_build_scope_from_parent(parent, key) {
            if self.loaded_scopes.contains(&scope) {
                None
            } else {
                let path = self
                    .scopes
                    .get(&scope)
                    .ok_or_else(|| {
                        self.err_at_current(MiddleErr::Internal(format!(
                            "missing build scope {scope}"
                        )))
                    })?
                    .path
                    .clone();
                let source = fs::read_to_string(&path).map_err(|err| {
                    self.err_at_current(MiddleErr::Internal(format!(
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

                let program = match program.node_type {
                    NodeType::ScopeDeclaration { body, .. } => Node {
                        node_type: NodeType::ScopeDeclaration {
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
                let program = crate::multipass::prepare_ast(program);

                if let NodeType::ScopeDeclaration {
                    body: Some(ref body),
                    ..
                } = program.node_type
                {
                    self.predeclare_forward_refs(&scope, body);
                }

                let node = self.evaluate(&scope, program);
                self.loaded_scopes.insert(scope);
                Some(node)
            }
        } else {
            None
        };

        if self.loaded_scopes.contains(&scope) {
            return Ok((scope, None));
        }

        let path = self
            .scopes
            .get(&scope)
            .ok_or_else(|| {
                self.err_at_current(MiddleErr::Internal(format!(
                    "missing scope {scope} for import"
                )))
            })?
            .path
            .clone();
        let source = fs::read_to_string(&path).map_err(|err| {
            self.err_at_current(MiddleErr::Internal(format!(
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

        let program = crate::multipass::prepare_ast(program);
        if let NodeType::ScopeDeclaration {
            body: Some(ref body),
            ..
        } = program.node_type
        {
            self.predeclare_forward_refs(&scope, body);
        }
        let node = self.evaluate(&scope, program);
        self.loaded_scopes.insert(scope);

        let node = match (node.node_type.clone(), build_node) {
            (MiddleNodeType::ScopeDeclaration { mut body, .. }, Some(build_node)) => MiddleNode {
                node_type: MiddleNodeType::ScopeDeclaration {
                    body: {
                        body.insert(0, build_node);
                        body
                    },
                    create_new_scope: true,
                    is_temp: false,
                    scope_id: scope,
                },
                ..node
            },
            (_, Some(build_node)) => MiddleNode::new(
                MiddleNodeType::ScopeDeclaration {
                    body: vec![node, build_node],
                    create_new_scope: false,
                    is_temp: false,
                    scope_id: scope,
                },
                self.current_span(),
            ),
            _ => node,
        };

        Ok((scope, Some(node)))
    }

    pub fn get_next_scope(&self, scope: u64, key: &str) -> Result<u64, MiddleErr> {
        Ok(match key {
            "super" => self
                .scopes
                .get(&scope)
                .and_then(|s| s.parent)
                .ok_or_else(|| self.err_at_current(MiddleErr::Scope("super".to_string())))?,
            _ => {
                let current = self
                    .scopes
                    .get(&scope)
                    .ok_or_else(|| self.err_at_current(MiddleErr::Scope(scope.to_string())))?;
                if let Some(x) = current.children.get(key) {
                    x.clone()
                } else if let Some(mapped) = current.mappings.get(key)
                    && let Some(x) = current.children.get(mapped)
                {
                    x.clone()
                } else if let Some(s) = self.get_global_scope().children.get(key) {
                    s.clone()
                } else if let Some(mapped) = self.get_global_scope().mappings.get(key)
                    && let Some(s) = self.get_global_scope().children.get(mapped)
                {
                    s.clone()
                } else if let Some(s) = self.get_root_scope().children.get(key) {
                    s.clone()
                } else if let Some(mapped) = self.get_root_scope().mappings.get(key)
                    && let Some(s) = self.get_root_scope().children.get(mapped)
                {
                    s.clone()
                } else {
                    return Err(self.err_at_current(MiddleErr::Scope(key.to_string())));
                }
            }
        })
    }
}
