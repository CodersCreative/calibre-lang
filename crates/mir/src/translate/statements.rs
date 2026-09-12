use crate::{
    ast::{MiddleNode, MiddleNodeType, MirScopeDecl},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
    tags::TagInfo,
    typing::{MiddleObject, MiddleTypeDefType},
};
use calibre_parser::{
    Span,
    ast::{
        idents::{ParserText, PotentialGenericTypeIdentifier},
        nodes::{Overload, TypeDefType},
        types::{ParserDataType, ParserInnerType},
    },
};
use tracing::instrument;
use ustr::{Ustr, UstrMap};

impl MiddleEnvironment {
    #[instrument(skip_all, fields(scope, identifier))]
    pub fn evaluate_type_declaration(
        &mut self,
        scope: ScopeId,
        span: Span,
        identifier: PotentialGenericTypeIdentifier,
        object: TypeDefType,
        overloads: Vec<Overload>,
    ) -> Result<MiddleNode, MiddleErr> {
        let mut has_default = false;
        let mut has_builder = false;

        for tag in &self.tagging.tag_info {
            if has_builder && has_default {
                break;
            }

            match tag {
                TagInfo::Builder => {
                    has_builder = true;
                }
                TagInfo::Default => {
                    has_default = true;
                }
                _ => {}
            }
        }

        // TODO Work on NewTypes
        if let TypeDefType::NewType(inner) = &object {
            let generic_params: Vec<Ustr> = match &identifier {
                PotentialGenericTypeIdentifier::Generic { generic_types, .. } => generic_types
                    .iter()
                    .filter_map(|t| match t {
                        ParserDataType {
                            data_type: ParserInnerType::Struct(s),
                            ..
                        } => Some(Ustr::from(s)),
                        _ => None,
                    })
                    .collect(),
                _ => Vec::new(),
            };

            let identifier = self.resolve(
                scope,
                &identifier,
                ResolutionOptions::default().with_dollar(),
            )?;

            let inner =
                self.resolve_data_type(scope, inner.as_ref(), ResolutionOptions::typing())?;

            let target_name = if identifier == inner.impl_name() {
                Some(identifier)
            } else {
                None
            };

            {
                let scope_ref = self.scoping.scope_mut_or_err(scope)?;

                scope_ref.type_mappings.insert(identifier, inner.data_type);
            }

            if let Some(x) = self.context.in_stdlib
                && let Some(y) = target_name
            {
                self.symbols
                    .native_mappings
                    .insert(Ustr::from(&format!("{}.{}", x, identifier)), y);
            }

            if !overloads.is_empty() {
                for overload in overloads {
                    if let Some(processed) =
                        self.process_overload(scope, overload, generic_params.clone(), target_name)?
                    {
                        self.symbols.overloads.push(processed);
                    }
                }
            }

            return Ok(MiddleNode {
                node_type: MiddleNodeType::EmptyLine,
                span,
            });
        }

        let ident = self.resolve(
            scope,
            &identifier,
            ResolutionOptions::default().with_dollar(),
        )?;

        let generic_params = if let PotentialGenericTypeIdentifier::Generic {
            identifier: _,
            generic_types,
        } = identifier.clone()
        {
            let template_params: Vec<Ustr> = generic_types
                .iter()
                .filter_map(|t| match t {
                    ParserDataType {
                        data_type: ParserInnerType::Struct(s),
                        ..
                    } => Some(Ustr::from(s)),
                    _ => None,
                })
                .collect();

            self.typing.generic_type_templates.entry(ident).or_insert((
                template_params,
                object.clone(),
                overloads.clone(),
            ));

            self.typing
                .generic_type_templates
                .get(&ident)
                .map(|(params, _, _)| params.clone())
                .unwrap_or_default()
        } else {
            Vec::new()
        };

        let new_name = Ustr::from(&ParserText::temp_name_with_suffix(ident.trim(), span).text);

        let object = MiddleTypeDefType::from_type_def_type(self, scope, object.clone());

        has_default = has_default
            || match &object {
                MiddleTypeDefType::Enum {
                    default_variant, ..
                } => default_variant.is_some(),
                MiddleTypeDefType::Struct(_) => false,
                _ => false,
            };

        let default_ident = self.resolve(scope, &"Default", ResolutionOptions::typing());

        self.typing.objects.insert(
            new_name,
            MiddleObject {
                object_type: object.clone(),
                variables: UstrMap::default(),
                traits: if let Ok(x) = default_ident
                    && has_default
                {
                    vec![x]
                } else {
                    Vec::new()
                },
                location: self.context.current_location.clone(),
            },
        );

        if let Some(x) = self.context.in_stdlib {
            self.symbols
                .native_mappings
                .insert(Ustr::from(&format!("{}.{}", x, ident)), new_name);
        }

        let previous_self_type = {
            let scope = self.scoping.scope_mut_or_err(scope)?;

            scope
                .type_mappings
                .insert(ident, ParserInnerType::Struct(new_name.to_string()));

            scope.type_mappings.insert(
                Ustr::from("Self"),
                ParserInnerType::Struct(new_name.to_string()),
            )
        };

        let identifier = ParserText::new(span, ident);

        let default_node = if has_default {
            Some(self.generate_default_impl(scope, span, identifier.clone(), object.clone())?)
        } else {
            None
        };

        let builder_nodes = if has_builder {
            Some(self.generate_builder(
                scope,
                span,
                identifier.clone(),
                object.clone(),
                has_default,
            )?)
        } else {
            None
        };

        for overload in overloads {
            if let Some(processed) =
                self.process_overload(scope, overload, generic_params.clone(), Some(new_name))?
            {
                self.symbols.overloads.push(processed);
            }
        }

        {
            let scope = self.scoping.scope_mut_or_err(scope)?;

            if let Some(prev) = previous_self_type {
                scope.type_mappings.insert(Ustr::from("Self"), prev);
            }
        }

        match (default_node, builder_nodes) {
            (Some(node), None) => Ok(node),
            (None, None) => Ok(MiddleNode {
                node_type: MiddleNodeType::EmptyLine,
                span,
            }),
            (None, Some(nodes)) => Ok(MiddleNode {
                node_type: MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                    body: vec![nodes.0, nodes.1],
                    create_new_scope: false,
                    is_temp: false,
                    scope_id: scope,
                }),
                span,
            }),
            (Some(node), Some(nodes)) => Ok(MiddleNode {
                node_type: MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                    body: vec![node, nodes.0, nodes.1],
                    create_new_scope: false,
                    is_temp: false,
                    scope_id: scope,
                }),
                span,
            }),
        }
    }
}
