use crate::environment::MiddleEnvironment;
use crate::scoping::ScopeId;
use crate::symbols::resolve::ResolutionOptions;
use crate::{ast::MiddleNode, errors::MiddleErr, typing::MiddleTypeDefType};
use calibre_parser::ast::idents::{
    ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier,
};
use calibre_parser::ast::nodes::{AstNode, AstNodeType, FunctionHeader, VarType};
use calibre_parser::ast::types::{GenericTypes, ParserDataType};
use calibre_parser::{
    Span,
    ast::{ObjectMap, ObjectType},
};

impl MiddleEnvironment {
    pub fn generate_default_impl(
        &mut self,
        scope: ScopeId,
        span: Span,
        identifier: ParserText,
        object_type: MiddleTypeDefType,
    ) -> Result<MiddleNode, MiddleErr> {
        let default_fn = match &object_type {
            MiddleTypeDefType::Enum {
                variants,
                default_variant,
                default_value,
            } => {
                if let Some(i) = default_variant {
                    if let Some((default_variant_name, _)) = variants.get(*i) {
                        AstNode::new(
                            span,
                            AstNodeType::VariableDeclaration {
                                var_type: VarType::Constant,
                                identifier: PotentialDollarIdentifier::Identifier(
                                    ParserText::from("default".to_string()),
                                ),
                                data_type: ParserDataType::auto(span),
                                value: Box::new(AstNode::new(
                                    span,
                                    AstNodeType::FunctionDeclaration {
                                        header: FunctionHeader {
                                            generics: GenericTypes::default(),
                                            parameters: Vec::new(),
                                            return_type: ParserDataType::object(
                                                span,
                                                &identifier.text,
                                            ),

                                            param_destructures: Vec::new(),
                                        },
                                        body: Box::new(AstNode::new_temp_scope(vec![
                                            AstNode::ret(AstNode::new(
                                                span,
                                                AstNodeType::EnumExpression {
                                                    identifier: identifier.clone().into(),
                                                    value: default_variant_name.clone().into(),
                                                    data: default_value.clone(),
                                                },
                                            )),
                                        ])),
                                    },
                                )),
                            },
                        )
                    } else {
                        return Err(MiddleErr::At(
                            span,
                            Box::new(MiddleErr::Internal(
                                "Invalid default variant index".to_string(),
                            )),
                        ));
                    }
                } else {
                    return Err(MiddleErr::At(
                        span,
                        Box::new(MiddleErr::Internal(
                            "Enum marked with @default but no default variant".to_string(),
                        )),
                    ));
                }
            }
            MiddleTypeDefType::Struct(ObjectMap(fields)) => {
                let fields = fields
                    .iter()
                    .map(|(field_name, (field_type, default_value))| {
                        if let Some(default) = default_value {
                            (field_name.clone(), *default.clone())
                        } else if let Some(default) = field_type.default_node() {
                            (field_name.clone(), default)
                        } else {
                            let resolved = self
                                .resolve_data_type(scope, field_type, ResolutionOptions::typing())
                                .unwrap_or(field_type.clone());
                            let type_name = resolved.impl_name();
                            (
                                field_name.clone(),
                                AstNode::member(
                                    span,
                                    AstNode::identifier(span, type_name),
                                    AstNode::call(
                                        span,
                                        AstNode::identifier(span, "default"),
                                        Vec::new(),
                                    ),
                                ),
                            )
                        }
                    })
                    .collect();

                AstNode::new(
                    span,
                    AstNodeType::VariableDeclaration {
                        var_type: VarType::Constant,
                        identifier: PotentialDollarIdentifier::Identifier(ParserText::from(
                            "default".to_string(),
                        )),
                        data_type: ParserDataType::auto(span),
                        value: Box::new(AstNode::new(
                            span,
                            AstNodeType::FunctionDeclaration {
                                header: FunctionHeader {
                                    generics: GenericTypes::default(),
                                    parameters: Vec::new(),
                                    return_type: ParserDataType::object(span, &identifier.text),
                                    param_destructures: Vec::new(),
                                },
                                body: Box::new(AstNode::new_temp_scope(vec![AstNode::ret(
                                    AstNode::new(
                                        span,
                                        AstNodeType::StructLiteral {
                                            identifier: identifier.clone().into(),
                                            value: ObjectType::Map(fields),
                                        },
                                    ),
                                )])),
                            },
                        )),
                    },
                )
            }
            _ => {
                return Err(MiddleErr::At(
                    span,
                    Box::new(MiddleErr::Internal(
                        "Cannot generate Default impl for this type".to_string(),
                    )),
                ));
            }
        };

        Ok(self.evaluate(
            scope,
            AstNode::new(
                span,
                AstNodeType::ImplTraitDeclaration {
                    generics: GenericTypes::default(),
                    trait_ident: PotentialGenericTypeIdentifier::new(Span::default(), "Default"),
                    target: ParserDataType::object(span, &identifier.text),
                    variables: vec![default_fn],
                },
            ),
        ))
    }
}
