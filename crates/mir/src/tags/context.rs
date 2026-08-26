use crate::{
    ast::MiddleNode,
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::{MiddleScope, ScopeId},
};
use calibre_parser::ast::{
    ObjectType,
    idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
    nodes::{AstNode, AstNodeType, VarType},
    types::ParserDataType,
};

#[derive(Debug, Clone, Default)]
pub struct PackageMetadata {
    pub name: String,
    pub version: String,
    pub description: String,
    pub license: String,
    pub repository: String,
    pub homepage: String,
    pub src: String,
    pub root: String,
}

impl MiddleEnvironment {
    pub fn package_metadata_for_scope(&self, scope: &MiddleScope) -> PackageMetadata {
        if scope.namespace == "std" {
            return PackageMetadata {
                name: String::from("std"),
                version: env!("CARGO_PKG_VERSION").to_string(),
                description: String::from("Calibre standard library"),
                license: String::from("MIT"),
                repository: String::new(),
                homepage: String::new(),
                src: scope.path.to_string_lossy().to_string(),
                root: scope.path.to_string_lossy().to_string(),
            };
        }

        if scope.namespace == "root" {
            return self
                .context
                .package_metadata
                .clone()
                .unwrap_or_else(|| PackageMetadata {
                    name: String::from("__package__"),
                    version: String::from("0.0.0"),
                    description: String::from("default package metadata"),
                    license: String::new(),
                    repository: String::new(),
                    homepage: String::new(),
                    src: scope.path_or_fallback(),
                    root: String::new(),
                });
        }

        PackageMetadata {
            name: scope.namespace.clone(),
            version: String::from("0.0.0"),
            description: String::from("default package metadata"),
            license: String::new(),
            repository: String::new(),
            homepage: String::new(),
            src: scope.path_or_fallback(),
            root: String::new(),
        }
    }

    pub fn evaluate_with_package_injection(
        &mut self,
        scope: ScopeId,
        node: AstNode,
    ) -> Result<MiddleNode, MiddleErr> {
        let Ok(scope_ref) = self.scoping.scope_or_err(scope) else {
            return self.evaluate_inner(scope, node);
        };

        let sp = node.span;
        let meta = self.package_metadata_for_scope(scope_ref);
        let value =
            |v: String| AstNode::new(sp, AstNodeType::StringLiteral(ParserText::new(sp, v)));

        let mut prefix = vec![AstNode::new(
            sp,
            AstNodeType::VariableDeclaration {
                var_type: VarType::Constant,
                identifier: PotentialDollarIdentifier::new(sp, "package"),
                data_type: ParserDataType::object(sp, "Package"),
                value: Box::new(AstNode::new(
                    sp,
                    AstNodeType::StructLiteral {
                        identifier: PotentialGenericTypeIdentifier::new(sp, "Package"),
                        value: ObjectType::Map(vec![
                            ("name".to_string(), value(meta.name)),
                            ("version".to_string(), value(meta.version)),
                            ("description".to_string(), value(meta.description)),
                            ("license".to_string(), value(meta.license)),
                            ("repository".to_string(), value(meta.repository)),
                            ("homepage".to_string(), value(meta.homepage)),
                            ("src".to_string(), value(meta.src)),
                            ("root".to_string(), value(meta.root)),
                        ]),
                    },
                )),
            },
        )];

        let mut body = match node.node_type {
            AstNodeType::ScopeDeclaration { body, .. } => body.unwrap_or_default(),
            _ => vec![node],
        };
        prefix.append(&mut body);

        self.evaluate_inner(
            scope,
            AstNode::new(
                sp,
                AstNodeType::ScopeDeclaration {
                    body: Some(prefix),
                    named: None,
                    is_temp: false,
                    create_new_scope: Some(false),
                    define: false,
                },
            ),
        )
    }

    pub fn evaluate_with_current_context_injection(
        &mut self,
        scope: ScopeId,
        node: AstNode,
    ) -> Result<MiddleNode, MiddleErr> {
        let Ok(scope_ref) = self.scoping.scope_or_err(scope) else {
            return self.evaluate_inner(scope, node);
        };

        let sp = node.span;
        let value =
            |v: String| AstNode::new(sp, AstNodeType::StringLiteral(ParserText::new(sp, v)));

        let function_name = match &node.node_type {
            AstNodeType::VariableDeclaration { identifier, .. } => match identifier {
                PotentialDollarIdentifier::Identifier(text) => text.text.clone(),
                PotentialDollarIdentifier::DollarIdentifier(text) => text.text.clone(),
            },
            _ => scope_ref.namespace.clone(),
        };

        let mut nodes = vec![AstNode::new(
            sp,
            AstNodeType::VariableDeclaration {
                var_type: VarType::Constant,
                identifier: PotentialDollarIdentifier::new(sp, "current_context"),
                data_type: ParserDataType::object(sp, "ExecContext"),
                value: Box::new(AstNode::new(
                    sp,
                    AstNodeType::StructLiteral {
                        identifier: PotentialGenericTypeIdentifier::new(sp, "ExecContext"),
                        value: ObjectType::Map(vec![
                            ("function_name".to_string(), value(function_name)),
                            (
                                "module_name".to_string(),
                                value(scope_ref.namespace.clone()),
                            ),
                            (
                                "path".to_string(),
                                value(
                                    scope_ref
                                        .path
                                        .canonicalize()
                                        .unwrap_or_default()
                                        .to_string_lossy()
                                        .to_string(),
                                ),
                            ),
                            (
                                "line".to_string(),
                                AstNode::int(sp, format!("{}u", sp.from.line)),
                            ),
                            (
                                "col".to_string(),
                                AstNode::int(sp, format!("{}u", sp.from.col)),
                            ),
                        ]),
                    },
                )),
            },
        )];

        let mut body = match node.node_type {
            AstNodeType::ScopeDeclaration { body, .. } => body.unwrap_or_default(),
            _ => vec![node],
        };
        nodes.append(&mut body);

        self.evaluate_inner(
            scope,
            AstNode::new(
                sp,
                AstNodeType::ScopeDeclaration {
                    body: Some(nodes),
                    named: None,
                    is_temp: false,
                    create_new_scope: Some(false),
                    define: false,
                },
            ),
        )
    }
}
