use crate::{
    ast::MiddleNode,
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::{MiddleScope, ScopeId},
    translate::MirLowering,
};
use calibre_parser::ast::{
    ObjectType,
    idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
    nodes::{
        AstNode, AstNodeType, VarType,
        declaration::AstDeclaration,
        literals::{AstString, AstStruct},
        scopes::AstScopeDef,
    },
    types::ParserDataType,
};
use serde::{Deserialize, Serialize};
use ustr::Ustr;

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PackageMetadata {
    pub name: Ustr,
    pub version: Ustr,
    pub description: Ustr,
    pub license: Ustr,
    pub repository: Ustr,
    pub homepage: Ustr,
    pub src: Ustr,
    pub root: Ustr,
}

impl MiddleEnvironment {
    pub fn package_metadata_for_scope(&self, scope: &MiddleScope) -> PackageMetadata {
        if scope.namespace == "std" {
            return PackageMetadata {
                name: Ustr::from("std"),
                version: Ustr::from(env!("CARGO_PKG_VERSION")),
                description: Ustr::from("Calibre standard library"),
                license: Ustr::from("MIT"),
                repository: Ustr::default(),
                homepage: Ustr::default(),
                src: Ustr::from(&scope.path.to_string_lossy()),
                root: Ustr::from(&scope.path.to_string_lossy()),
            };
        }

        if scope.namespace == "root" {
            return self
                .context
                .package_metadata
                .clone()
                .unwrap_or_else(|| PackageMetadata {
                    name: Ustr::from("__package__"),
                    version: Ustr::from("0.0.0"),
                    description: Ustr::from("default package metadata"),
                    license: Ustr::default(),
                    repository: Ustr::default(),
                    homepage: Ustr::default(),
                    src: scope.path_or_fallback(),
                    root: Ustr::default(),
                });
        }

        PackageMetadata {
            name: scope.namespace,
            version: Ustr::from("0.0.0"),
            description: Ustr::from("default package metadata"),
            license: Ustr::default(),
            repository: Ustr::default(),
            homepage: Ustr::default(),
            src: scope.path_or_fallback(),
            root: Ustr::default(),
        }
    }

    pub fn evaluate_with_package_injection(
        &mut self,
        scope: ScopeId,
        node: AstNode,
    ) -> Result<MiddleNode, MiddleErr> {
        let span = node.span;

        let Ok(scope_ref) = self.scoping.scope_or_err(scope) else {
            return node.lower(self, scope, span);
        };

        let meta = self.package_metadata_for_scope(scope_ref);
        let value = |v: Ustr| {
            AstNode::new(
                span,
                AstNodeType::StringLiteral(AstString {
                    value: ParserText::new(span, v),
                }),
            )
        };

        let mut prefix = vec![AstNode::new(
            span,
            AstNodeType::VariableDeclaration(AstDeclaration {
                var_type: VarType::Constant,
                identifier: PotentialDollarIdentifier::new(span, "package"),
                data_type: ParserDataType::object(span, "Package"),
                value: Box::new(AstNode::new(
                    span,
                    AstNodeType::StructLiteral(AstStruct {
                        identifier: PotentialGenericTypeIdentifier::new(span, "Package"),
                        value: ObjectType::Map(vec![
                            (Ustr::from("name"), value(meta.name)),
                            (Ustr::from("version"), value(meta.version)),
                            (Ustr::from("description"), value(meta.description)),
                            (Ustr::from("license"), value(meta.license)),
                            (Ustr::from("repository"), value(meta.repository)),
                            (Ustr::from("homepage"), value(meta.homepage)),
                            (Ustr::from("src"), value(meta.src)),
                            (Ustr::from("root"), value(meta.root)),
                        ]),
                    }),
                )),
            }),
        )];

        let mut body = match node.node_type {
            AstNodeType::ScopeDeclaration(AstScopeDef { body, .. }) => body.unwrap_or_default(),
            _ => vec![node],
        };
        prefix.append(&mut body);

        AstScopeDef {
            body: Some(prefix),
            named: None,
            is_temp: false,
            create_new_scope: Some(false),
            define: false,
        }
        .lower(self, scope, span)
    }

    pub fn evaluate_with_current_context_injection(
        &mut self,
        scope: ScopeId,
        node: AstNode,
    ) -> Result<MiddleNode, MiddleErr> {
        let span = node.span;

        let Ok(scope_ref) = self.scoping.scope_or_err(scope) else {
            return node.lower(self, scope, span);
        };

        let value = |v: Ustr| {
            AstNode::new(
                span,
                AstNodeType::StringLiteral(AstString {
                    value: ParserText::new(span, v),
                }),
            )
        };

        let function_name = match &node.node_type {
            AstNodeType::VariableDeclaration(AstDeclaration { identifier, .. }) => {
                Ustr::from(match identifier {
                    PotentialDollarIdentifier::Identifier(text) => &text.text,
                    PotentialDollarIdentifier::DollarIdentifier(text) => &text.text,
                })
            }
            _ => scope_ref.namespace,
        };

        let mut nodes = vec![AstNode::new(
            span,
            AstNodeType::VariableDeclaration(AstDeclaration {
                var_type: VarType::Constant,
                identifier: PotentialDollarIdentifier::new(span, "current_context"),
                data_type: ParserDataType::object(span, "ExecContext"),
                value: Box::new(AstNode::new(
                    span,
                    AstNodeType::StructLiteral(AstStruct {
                        identifier: PotentialGenericTypeIdentifier::new(span, "ExecContext"),
                        value: ObjectType::Map(vec![
                            (Ustr::from("function_name"), value(function_name)),
                            (Ustr::from("module_name"), value(scope_ref.namespace)),
                            (
                                Ustr::from("path"),
                                value(Ustr::from(
                                    &scope_ref
                                        .path
                                        .canonicalize()
                                        .unwrap_or_default()
                                        .to_string_lossy(),
                                )),
                            ),
                            (Ustr::from("span"), AstNode::range(span, span.to_range())),
                        ]),
                    }),
                )),
            }),
        )];

        let mut body = match node.node_type {
            AstNodeType::ScopeDeclaration(AstScopeDef { body, .. }) => body.unwrap_or_default(),
            _ => vec![node],
        };
        nodes.append(&mut body);

        AstScopeDef {
            body: Some(nodes),
            named: None,
            is_temp: false,
            create_new_scope: Some(false),
            define: false,
        }
        .lower(self, scope, span)
    }
}
