use crate::{
    ast::{MiddleNode, MiddleNodeType},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
    translate::MirLowering,
};
use calibre_parser::{
    Span,
    ast::{
        idents::{ParserText, PotentialDollarIdentifier},
        nodes::{
            AstNode, AstNodeType, VarType,
            declaration::AstDeclaration,
            functions::{AstFunction, FunctionHeader},
            misc::{AstImport, AstParen, AstTag, AstTest},
        },
        types::{GenericTypes, ParserDataType},
    },
};
use tracing::instrument;
use ustr::Ustr;

impl MirLowering for AstParen {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        self.value.lower(env, scope, span)
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        self.value.type_of(env, scope, span)
    }
}

impl MirLowering for AstTag {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        if let Some(handler) = env
            .tagging
            .tag_handlers
            .get(&Ustr::from(&self.tag.text))
            .cloned()
        {
            let handler_fn = handler.handler.lock().unwrap();
            handler_fn(env, scope, *self.node, self.tag, self.arguments)
        } else {
            env.context.push_error(MiddleErr::InvalidTag(self.tag.text));
            self.node.lower(env, scope, span)
        }
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        self.node.type_of(env, scope, span)
    }
}

impl MirLowering for AstTest {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        let func_identifier = format!(
            "test::{}",
            ParserText::temp_name_with_suffix(self.identifier.text.trim(), span).text
        );

        let file_path = env.scoping.scope_or_err(scope).map(|s| s.path.clone()).ok();

        env.register_test(
            Ustr::from(&self.identifier.text),
            Ustr::from(&func_identifier),
            scope,
            file_path,
        );

        AstNode::new(
            span,
            AstNodeType::VariableDeclaration(AstDeclaration {
                var_type: VarType::Constant,
                identifier: PotentialDollarIdentifier::Identifier(ParserText::new(
                    span,
                    func_identifier,
                )),
                data_type: ParserDataType::auto(span),
                value: Box::new(AstNode::new(
                    span,
                    AstNodeType::FunctionDeclaration(AstFunction {
                        header: FunctionHeader {
                            generics: GenericTypes::default(),
                            parameters: Vec::new(),
                            return_type: ParserDataType::null(span),
                            param_destructures: Vec::new(),
                        },
                        body: self.body,
                    }),
                )),
            }),
        )
        .lower(env, scope, span)
    }
}

impl MirLowering for AstImport {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        let values: Vec<Ustr> = self
            .values
            .into_iter()
            .map(|val| Ustr::from(&val.to_string()))
            .collect();

        let module_path: Vec<Ustr> = self
            .module
            .iter()
            .map(|x| Ustr::from(&x.to_string()))
            .collect();

        let alias = if let Some(alias) = &self.alias {
            env.resolve(scope, alias, ResolutionOptions::default().with_dollar())
                .ok()
        } else {
            None
        };

        let (new_scope, build_node) = if let Some(alias) = alias {
            if ["super", "root"].contains(&alias.as_str()) {
                // TODO return err
                return Ok(MiddleNode {
                    node_type: MiddleNodeType::EmptyLine,
                    span,
                });
            }

            let (new_scope_id, build_node) = env.import_scope_list(scope, &module_path)?;

            env.scoping
                .scope_mut_or_err(scope)?
                .children
                .insert(alias, new_scope_id);

            return Ok(build_node.unwrap_or(MiddleNode {
                node_type: MiddleNodeType::EmptyLine,
                span,
            }));
        } else if !values.is_empty() {
            let (new_scope_id, build_node) = env.import_scope_list(scope, &module_path)?;
            (new_scope_id, build_node)
        } else {
            let (_, n) = env.import_scope_list(scope, &module_path)?;
            return Ok(if let Some(x) = n {
                x
            } else {
                MiddleNode {
                    node_type: MiddleNodeType::EmptyLine,
                    span,
                }
            });
        };

        let (ident_map, type_map) = {
            let scope = env.scoping.scope_or_err(new_scope)?;

            (scope.mappings.clone(), scope.type_mappings.clone())
        };

        if &values[0] == "*" {
            let scope = env.scoping.scope_mut_or_err(scope)?;

            for (key, value) in ident_map {
                scope.mappings.entry(key).or_insert(value);
            }

            for (key, value) in type_map {
                scope.type_mappings.entry(key).or_insert(value);
            }
        } else {
            let scope = env.scoping.scope_mut_or_err(scope)?;

            for key in values {
                if let Some(value) = ident_map.get(&key).cloned() {
                    scope.mappings.insert(key, value);
                    continue;
                }

                if let Some(value) = type_map.get(&key).cloned() {
                    scope.type_mappings.insert(key, value);
                } else {
                    return Err(MiddleErr::At(
                        span,
                        Box::new(MiddleErr::CantImport(format!(
                            "{} at {:?}",
                            key, self.module
                        ))),
                    ));
                }
            }
        }

        Ok(build_node.unwrap_or(MiddleNode {
            node_type: MiddleNodeType::EmptyLine,
            span,
        }))
    }
}
