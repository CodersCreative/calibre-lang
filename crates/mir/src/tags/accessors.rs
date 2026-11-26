use crate::{
    ast::MiddleNode, environment::MiddleEnvironment, errors::MiddleErr, scoping::ScopeId, translate::MirLowering,
};
use calibre_parser::{
    Span,
    ast::{
        ObjectType, RefMutability,
        comparison::ComparisonOperator,
        idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::{
            AstNode, AstNodeType, VarType,
            assignment::AstAssignment,
            binary::AstComparison,
            conditionals::{AstIf, IfComparisonType},
            declaration::AstDeclaration,
            functions::{AstFunction, CallArg, FunctionHeader, FunctionParam},
            literals::AstStruct,
            memory::AstRef,
            misc::StandaloneTag,
            types::AstImpl,
        },
        types::{GenericTypes, ParserDataType, ParserInnerType},
    },
};
use ustr::Ustr;

fn self_param(span: Span, type_name: &str, mutability: RefMutability) -> FunctionParam {
    FunctionParam {
        var_type: if mutability == RefMutability::MutRef {
            VarType::Mutable
        } else {
            VarType::Immutable
        },
        identifier: PotentialDollarIdentifier::new(span, "self"),
        data_type: Some(ParserDataType::reference(
            span,
            ParserDataType::object(span, type_name),
            mutability,
        )),
        default: None,
    }
}

fn value_param(span: Span, ty: &ParserDataType) -> FunctionParam {
    FunctionParam {
        var_type: VarType::Immutable,
        identifier: PotentialDollarIdentifier::new(span, "value"),
        data_type: Some(ty.clone()),
        default: None,
    }
}

fn method(
    span: Span,
    name: impl ToString,
    parameters: Vec<FunctionParam>,
    return_type: ParserDataType,
    body: AstNode,
) -> AstNode {
    AstNode::new(
        span,
        AstNodeType::VariableDeclaration(AstDeclaration {
            var_type: VarType::Constant,
            identifier: PotentialDollarIdentifier::new(span, name),
            data_type: ParserDataType::auto(span),
            value: Box::new(AstNode::new(
                span,
                AstNodeType::FunctionDeclaration(AstFunction {
                    header: FunctionHeader {
                        generics: GenericTypes::default(),
                        parameters,
                        return_type,
                        param_destructures: Vec::new(),
                    },
                    body: Box::new(AstNode::new_temp_scope(vec![body])),
                }),
            )),
            declared: false,
        }),
    )
}

fn self_member(span: Span, field: Ustr) -> AstNode {
    AstNode::member(span, AstNode::self_identifier(span), field)
}

impl MiddleEnvironment {
    pub fn generate_accessors(
        &mut self,
        scope: ScopeId,
        span: Span,
        type_name: ParserText,
        fields: Vec<(Ustr, Vec<StandaloneTag>, ParserDataType)>,
    ) -> Result<Option<MiddleNode>, MiddleErr> {
        let name = type_name.text.clone();
        let mut methods = Vec::new();

        for (field, tags, ty) in &fields {
            for tag in tags {
                let tag_name = tag.tag.text.as_str();

                match tag_name {
                    "get" => {
                        let method = method(
                            span,
                            format!("get_{field}"),
                            vec![self_param(span, &name, RefMutability::Ref)],
                            ty.clone(),
                            AstNode::ret(self_member(span, *field)),
                        );

                        methods.push(method);
                    }
                    "set" => {
                        let method = method(
                            span,
                            format!("set_{field}"),
                            vec![
                                self_param(span, &name, RefMutability::MutRef),
                                value_param(span, ty),
                            ],
                            ty.clone(),
                            AstNode::ret(AstNode::new(
                                span,
                                AstNodeType::AssignmentExpression(AstAssignment {
                                    identifier: Box::new(self_member(span, *field)),
                                    value: Box::new(AstNode::identifier(span, "value")),
                                }),
                            )),
                        );

                        methods.push(method);
                    }
                    "with" => {
                        let fields = fields
                            .iter()
                            .map(|(other, _, _)| {
                                (
                                    *other,
                                    if other == field {
                                        AstNode::identifier(span, "value")
                                    } else {
                                        self_member(span, *other)
                                    },
                                )
                            })
                            .collect();

                        let method = method(
                            span,
                            format!("with_{field}"),
                            vec![
                                self_param(span, &name, RefMutability::Ref),
                                value_param(span, ty),
                            ],
                            ParserDataType::object(span, &name),
                            AstNode::ret(AstNode::new(
                                span,
                                AstNodeType::StructLiteral(AstStruct {
                                    identifier: Some(PotentialGenericTypeIdentifier::new(
                                        span, &name,
                                    )),
                                    value: ObjectType::Map(fields),
                                }),
                            )),
                        );

                        methods.push(method);
                    }
                    "get_ref" => {
                        let method = method(
                            span,
                            format!("get_{field}_ref"),
                            vec![self_param(span, &name, RefMutability::Ref)],
                            ParserDataType::reference(span, ty.clone(), RefMutability::Ref),
                            AstNode::ret(AstNode::new(
                                span,
                                AstNodeType::RefStatement(AstRef {
                                    mutability: RefMutability::Ref,
                                    value: Box::new(self_member(span, *field)),
                                }),
                            )),
                        );

                        methods.push(method);
                    }
                    "get_mut" => {
                        let method = method(
                            span,
                            format!("get_{field}_mut"),
                            vec![self_param(span, &name, RefMutability::MutRef)],
                            ParserDataType::reference(span, ty.clone(), RefMutability::MutRef),
                            AstNode::ret(AstNode::new(
                                span,
                                AstNodeType::RefStatement(AstRef {
                                    mutability: RefMutability::MutRef,
                                    value: Box::new(self_member(span, *field)),
                                }),
                            )),
                        );

                        methods.push(method);
                    }
                    _ => {}
                }
            }
        }

        if methods.is_empty() {
            return Ok(None);
        }

        let node = AstNode::new(
            span,
            AstNodeType::ImplDeclaration(AstImpl {
                generics: GenericTypes::default(),
                target: ParserDataType::object(span, &name),
                variables: methods,
            }),
        )
        .lower(self, scope, span, None)?;

        Ok(Some(node))
    }

    pub fn generate_enum_accessors(
        &mut self,
        scope: ScopeId,
        span: Span,
        type_name: ParserText,
        variants: Vec<(Ustr, Vec<StandaloneTag>, Option<ParserDataType>)>,
    ) -> Result<Option<MiddleNode>, MiddleErr> {
        let name = type_name.text.clone();
        let mut methods = Vec::new();

        for (index, (variant, tags, payload)) in variants.iter().enumerate() {
            for tag in tags {
                let tag_name = tag.tag.text.as_str();

                let (suffix, mutability, payload_type, payload_value) = match tag_name {
                    "get" => (
                        String::new(),
                        RefMutability::Ref,
                        payload.clone().unwrap_or_else(|| {
                            ParserDataType::new(
                                span,
                                ParserInnerType::Bool,
                            )
                        }),
                        self_member(span, *variant),
                    ),
                    "get_ref" => (
                        String::from("_ref"),
                        RefMutability::Ref,
                        match payload {
                            Some(ty) => ParserDataType::reference(
                                span,
                                ty.clone(),
                                RefMutability::Ref,
                            ),
                            None => ParserDataType::new(span, ParserInnerType::Bool),
                        },
                        AstNode::new(
                            span,
                            AstNodeType::RefStatement(AstRef {
                                mutability: RefMutability::Ref,
                                value: Box::new(self_member(span, *variant)),
                            }),
                        ),
                    ),
                    "get_mut" => (
                        String::from("_mut"),
                        RefMutability::MutRef,
                        match payload {
                            Some(ty) => ParserDataType::reference(
                                span,
                                ty.clone(),
                                RefMutability::MutRef,
                            ),
                            None => ParserDataType::new(span, ParserInnerType::Bool),
                        },
                        AstNode::new(
                            span,
                            AstNodeType::RefStatement(AstRef {
                                mutability: RefMutability::MutRef,
                                value: Box::new(self_member(span, *variant)),
                            }),
                        ),
                    ),
                    _ => continue,
                };

                let payload_value = if payload.is_none() {
                    AstNode::bool(span, true)
                } else {
                    payload_value
                };

                let return_type = ParserDataType::new(
                    span,
                    ParserInnerType::Option(Box::new(payload_type)),
                );

                let condition = AstNode::new(
                    span,
                    AstNodeType::ComparisonExpression(AstComparison {
                        left: Box::new(AstNode::call(
                            span,
                            AstNode::identifier(span, "discriminant"),
                            vec![CallArg::Value(AstNode::self_identifier(span))],
                        )),
                        right: Box::new(AstNode::int(span, format!("{index}u"))),
                        operator: ComparisonOperator::Equal,
                    }),
                );

                let body = AstNode::new(
                    span,
                    AstNodeType::IfStatement(AstIf {
                        comparison: Box::new(IfComparisonType::If(condition)),
                        then: Box::new(AstNode::ret(AstNode::call(
                            span,
                            AstNode::identifier(span, "some"),
                            vec![CallArg::Value(payload_value)],
                        ))),
                        otherwise: Some(Box::new(AstNode::ret(AstNode::none(span)))),
                    }),
                );

                let method = method(
                    span,
                    format!("get_{variant}{suffix}"),
                    vec![self_param(span, &name, mutability)],
                    return_type,
                    body,
                );

                methods.push(method);
            }
        }

        if methods.is_empty() {
            return Ok(None);
        }

        let node = AstNode::new(
            span,
            AstNodeType::ImplDeclaration(AstImpl {
                generics: GenericTypes::default(),
                target: ParserDataType::object(span, &name),
                variables: methods,
            }),
        )
        .lower(self, scope, span, None)?;

        Ok(Some(node))
    }
}
