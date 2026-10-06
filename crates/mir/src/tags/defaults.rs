use crate::environment::MiddleEnvironment;
use crate::scoping::ScopeId;
use crate::translate::MirLowering;
use crate::{ast::MiddleNode, errors::MiddleErr, typing::MiddleTypeDefType};
use calibre_parser::ast::idents::{
    ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier,
};
use calibre_parser::ast::nodes::declaration::AstDeclaration;
use calibre_parser::ast::nodes::functions::{AstFunction, FunctionHeader};
use calibre_parser::ast::nodes::types::AstImpl;
use calibre_parser::ast::nodes::{
    AstNode, AstNodeType, VarType,
    literals::{AstEnum, AstStruct},
};
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
                            AstNodeType::VariableDeclaration(AstDeclaration {
                                var_type: VarType::Constant,
                                identifier: PotentialDollarIdentifier::Identifier(
                                    ParserText::from("default".to_string()),
                                ),
                                data_type: ParserDataType::auto(span),
                                value: Box::new(AstNode::new(
                                    span,
                                    AstNodeType::FunctionDeclaration(AstFunction {
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
                                                AstNodeType::EnumExpression(AstEnum {
                                                    identifier: Some(
                                                        PotentialGenericTypeIdentifier::new(
                                                            span,
                                                            &identifier.text,
                                                        ),
                                                    ),
                                                    value: (*default_variant_name).into(),
                                                    data: default_value.clone(),
                                                }),
                                            )),
                                        ])),
                                    }),
                                )),
                                declared: false,
                            }),
                        )
                    } else {
                        return Err(MiddleErr::At(
                            span,
                            Box::new(MiddleErr::InternalInvalidDefaultVariantIndex),
                        ));
                    }
                } else {
                    return Err(MiddleErr::At(
                        span,
                        Box::new(MiddleErr::InternalMissingDefaultVariant),
                    ));
                }
            }
            MiddleTypeDefType::Struct(ObjectMap(fields)) => {
                let fields = fields
                    .iter()
                    .map(|(field_name, (resolved, default_value))| {
                        if let Some(default) = default_value {
                            (*field_name, *default.clone())
                        } else if let Some(default) = resolved.default_node(span) {
                            (*field_name, default)
                        } else {
                            let type_name = resolved.impl_name();
                            (
                                *field_name,
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
                    AstNodeType::VariableDeclaration(AstDeclaration {
                        var_type: VarType::Constant,
                        identifier: PotentialDollarIdentifier::Identifier(ParserText::from(
                            "default".to_string(),
                        )),
                        data_type: ParserDataType::auto(span),
                        value: Box::new(AstNode::new(
                            span,
                            AstNodeType::FunctionDeclaration(AstFunction {
                                header: FunctionHeader {
                                    generics: GenericTypes::default(),
                                    parameters: Vec::new(),
                                    return_type: ParserDataType::object(span, &identifier.text),
                                    param_destructures: Vec::new(),
                                },
                                body: Box::new(AstNode::new_temp_scope(vec![AstNode::ret(
                                    AstNode::new(
                                        span,
                                        AstNodeType::StructLiteral(AstStruct {
                                            identifier: Some(PotentialGenericTypeIdentifier::new(
                                                span,
                                                &identifier.text,
                                            )),
                                            value: ObjectType::Map(fields),
                                        }),
                                    ),
                                )])),
                            }),
                        )),
                        declared: false,
                    }),
                )
            }
            _ => {
                return Err(MiddleErr::At(
                    span,
                    Box::new(MiddleErr::InternalCannotGenerateDefaultImpl),
                ));
            }
        };

        AstNode::new(
            span,
            AstNodeType::ImplDeclaration(AstImpl {
                generics: GenericTypes::default(),
                target: ParserDataType::object(span, &identifier.text),
                variables: vec![default_fn],
            }),
        )
        .lower(self, scope, span, None)
    }
}
