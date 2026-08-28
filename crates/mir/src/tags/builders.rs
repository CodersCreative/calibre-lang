use std::format;

use crate::{
    ast::MiddleNode, environment::MiddleEnvironment, errors::MiddleErr, scoping::ScopeId,
    tags::TagInfo, typing::MiddleTypeDefType,
};
use calibre_parser::{
    Span,
    ast::{
        ObjectType,
        idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        matching::TryCatch,
        nodes::{AstNode, AstNodeType, CallArg, FunctionHeader, TypeDefType, VarType},
        types::{GenericTypes, ParserDataType, ParserInnerType},
    },
};

impl MiddleEnvironment {
    pub fn generate_builder(
        &mut self,
        scope: ScopeId,
        span: Span,
        identifier: ParserText,
        object_type: MiddleTypeDefType,
        has_default: bool,
    ) -> Result<(MiddleNode, MiddleNode), MiddleErr> {
        let MiddleTypeDefType::Struct(fields) = object_type else {
            return Err(MiddleErr::At(
                span,
                Box::new(MiddleErr::Internal(
                    "@builder can only be used on structs".into(),
                )),
            ));
        };

        let builder_name = format!("{}Builder", identifier.text.trim());

        let optional_fields: Vec<_> = fields
            .iter()
            .map(|(field, (ty, node))| {
                (
                    field.clone(),
                    (
                        ParserDataType::new(span, ParserInnerType::Option(Box::new(ty.clone()))),
                        node.clone().map(|x| {
                            AstNode::call(
                                span,
                                AstNode::identifier(span, "some"),
                                vec![CallArg::Value(*x)],
                            )
                        }),
                    ),
                )
            })
            .collect();

        let builder = AstNode::new(
            span,
            AstNodeType::TypeDeclaration {
                identifier: PotentialGenericTypeIdentifier::new(span, &builder_name),
                object: TypeDefType::Struct {
                    fields: ObjectType::Map(optional_fields),
                },
                overloads: Vec::new(),
            },
        );

        let self_ty = ParserDataType::object(span, &builder_name);
        let target_ty = ParserDataType::object(span, &identifier.text);
        let self_id = AstNode::identifier(span, "self");
        let target_default_id = AstNode::identifier(span, "target");
        let mut methods = Vec::new();

        for (field, (ty, _)) in fields.iter() {
            let value_id = AstNode::identifier(span, "value");
            let setter_fields = fields
                .iter()
                .map(|(other, _)| {
                    (
                        other.clone(),
                        if other == field {
                            AstNode::call(
                                span,
                                AstNode::identifier(span, "some"),
                                vec![CallArg::Value(value_id.clone())],
                            )
                        } else {
                            AstNode::member(span, self_id.clone(), other)
                        },
                    )
                })
                .collect();

            let setter = AstNode::new(
                span,
                AstNodeType::VariableDeclaration {
                    var_type: VarType::Constant,
                    identifier: PotentialDollarIdentifier::new(span, format!("set_{field}")),
                    data_type: ParserDataType::auto(span),
                    value: Box::new(AstNode::new(
                        span,
                        AstNodeType::FunctionDeclaration {
                            header: FunctionHeader {
                                generics: GenericTypes::default(),
                                parameters: vec![
                                    (
                                        PotentialDollarIdentifier::new(span, "self"),
                                        Some(self_ty.clone()),
                                        None,
                                    ),
                                    (
                                        PotentialDollarIdentifier::new(span, "value"),
                                        Some(ty.clone()),
                                        None,
                                    ),
                                ],
                                return_type: self_ty.clone(),
                                param_destructures: vec![],
                            },
                            body: Box::new(AstNode::new_temp_scope(vec![AstNode::ret(
                                AstNode::new(
                                    span,
                                    AstNodeType::StructLiteral {
                                        identifier: PotentialGenericTypeIdentifier::new(
                                            span,
                                            &builder_name,
                                        ),
                                        value: ObjectType::Map(setter_fields),
                                    },
                                ),
                            )])),
                        },
                    )),
                },
            );
            methods.push(setter);
        }

        let built_fields = fields
            .iter()
            .map(|(field, _)| {
                (
                    field.clone(),
                    AstNode::new(
                        span,
                        AstNodeType::Try {
                            value: Box::new(AstNode::member(span, self_id.clone(), field)),
                            catch: Some(TryCatch {
                                name: None,
                                body: if has_default {
                                    Box::new(AstNode::member(
                                        span,
                                        target_default_id.clone(),
                                        field,
                                    ))
                                } else {
                                    Box::new(AstNode::ret(AstNode::call(
                                        span,
                                        AstNode::identifier(span, "err"),
                                        vec![CallArg::Value(AstNode::new(
                                            span,
                                            AstNodeType::StringLiteral(ParserText::new(
                                                span,
                                                format!(
                                                    "Unable to build {}, field {} was None",
                                                    self_ty, field
                                                ),
                                            )),
                                        ))],
                                    )))
                                },
                            }),
                        },
                    ),
                )
            })
            .collect();

        methods.push(AstNode::new(
            span,
            AstNodeType::VariableDeclaration {
                var_type: VarType::Constant,
                identifier: PotentialDollarIdentifier::new(span, "build"),
                data_type: ParserDataType::auto(span),
                value: Box::new(AstNode::new(
                    span,
                    AstNodeType::FunctionDeclaration {
                        header: FunctionHeader {
                            generics: GenericTypes::default(),
                            parameters: vec![(
                                PotentialDollarIdentifier::new(span, "self"),
                                Some(self_ty),
                                None,
                            )],
                            return_type: ParserDataType::new(
                                span,
                                ParserInnerType::Result {
                                    ok: Box::new(target_ty.clone()),
                                    err: Box::new(ParserDataType::new(span, ParserInnerType::Str)),
                                },
                            ),
                            param_destructures: vec![],
                        },
                        body: Box::new(AstNode::new_temp_scope(vec![
                            if has_default {
                                AstNode::new(
                                    span,
                                    AstNodeType::VariableDeclaration {
                                        var_type: VarType::Constant,
                                        identifier: PotentialDollarIdentifier::new(
                                            span,
                                            &target_default_id,
                                        ),
                                        data_type: target_ty,
                                        value: Box::new(AstNode::call(
                                            span,
                                            AstNode::member(
                                                span,
                                                AstNode::identifier(span, &identifier.text),
                                                "default",
                                            ),
                                            Vec::new(),
                                        )),
                                    },
                                )
                            } else {
                                AstNode::null(span)
                            },
                            AstNode::ret(AstNode::call(
                                span,
                                AstNode::identifier(span, "ok"),
                                vec![CallArg::Value(AstNode::new(
                                    span,
                                    AstNodeType::StructLiteral {
                                        identifier: identifier.clone().into(),
                                        value: ObjectType::Map(built_fields),
                                    },
                                ))],
                            )),
                        ])),
                    },
                )),
            },
        ));

        let tags = std::mem::take(&mut self.tagging.tag_info);
        self.tagging.tag_info.push(TagInfo::Default);
        let builder = self.evaluate(scope, builder);
        self.tagging.tag_info = tags;

        Ok((
            builder,
            self.evaluate(
                scope,
                AstNode::new(
                    span,
                    AstNodeType::ImplDeclaration {
                        generics: GenericTypes::default(),
                        target: ParserDataType::object(span, &builder_name),
                        variables: methods,
                    },
                ),
            ),
        ))
    }
}
