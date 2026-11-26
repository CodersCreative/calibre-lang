use crate::{
    environment::MiddleEnvironment, errors::MiddleErr, scoping::ScopeId, translate::MirLowering,
};
use calibre_parser::{
    Span,
    ast::{
        ObjectType, RefMutability,
        idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::{
            AstNode, AstNodeType, VarType,
            assignment::AstAssignment,
            declaration::AstDeclaration,
            functions::{AstFunction, FunctionHeader, FunctionParam},
            literals::AstStruct,
            memory::AstRef,
            misc::StandaloneTag,
            types::AstImpl,
        },
        types::{GenericTypes, ParserDataType},
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
    ) -> Result<Option<crate::ast::MiddleNode>, MiddleErr> {
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
}
