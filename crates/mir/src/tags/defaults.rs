use crate::environment::MiddleEnvironment;
use crate::{ast::MiddleNode, errors::MiddleErr, typing::MiddleTypeDefType};
use calibre_parser::{
    Span,
    ast::{
        FunctionHeader, GenericTypes, Node, NodeType, ObjectMap, ObjectType, ParserDataType,
        ParserInnerType, ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier,
        PotentialNewType, VarType,
    },
};

impl MiddleEnvironment {
    pub fn generate_default_impl(
        &mut self,
        scope: &u64,
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
                        Node::new(
                            span,
                            NodeType::VariableDeclaration {
                                var_type: VarType::Constant,
                                identifier: PotentialDollarIdentifier::Identifier(
                                    ParserText::from("default".to_string()),
                                ),
                                data_type: PotentialNewType::DataType(ParserDataType::new(
                                    span,
                                    ParserInnerType::Auto(None),
                                )),
                                value: Box::new(Node::new(
                                    span,
                                    NodeType::FunctionDeclaration {
                                        header: FunctionHeader {
                                            generics: GenericTypes::default(),
                                            parameters: Vec::new(),
                                            return_type: PotentialNewType::DataType(
                                                ParserDataType::new(
                                                    span,
                                                    ParserInnerType::Struct(
                                                        identifier.text.clone(),
                                                    ),
                                                ),
                                            ),
                                            param_destructures: Vec::new(),
                                        },
                                        body: Box::new(Node::new_temp_scope(vec![Node::ret(
                                            Node::new(
                                                span,
                                                NodeType::EnumExpression {
                                                    identifier: identifier.clone().into(),
                                                    value: default_variant_name.clone().into(),
                                                    data: default_value.clone(),
                                                },
                                            ),
                                        )])),
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
                            let resolved = self.resolve_data_type(scope, field_type.clone());
                            let type_name = resolved.impl_name();
                            (
                                field_name.clone(),
                                Node::member(
                                    span,
                                    Node::identifier(span, type_name),
                                    Node::call(span, Node::identifier(span, "default"), Vec::new()),
                                ),
                            )
                        }
                    })
                    .collect();

                Node::new(
                    span,
                    NodeType::VariableDeclaration {
                        var_type: VarType::Constant,
                        identifier: PotentialDollarIdentifier::Identifier(ParserText::from(
                            "default".to_string(),
                        )),
                        data_type: PotentialNewType::DataType(ParserDataType::new(
                            span,
                            ParserInnerType::Auto(None),
                        )),
                        value: Box::new(Node::new(
                            span,
                            NodeType::FunctionDeclaration {
                                header: FunctionHeader {
                                    generics: GenericTypes::default(),
                                    parameters: Vec::new(),
                                    return_type: PotentialNewType::DataType(ParserDataType::new(
                                        span,
                                        ParserInnerType::Struct(identifier.text.clone()),
                                    )),
                                    param_destructures: Vec::new(),
                                },
                                body: Box::new(Node::new_temp_scope(vec![Node::ret(Node::new(
                                    span,
                                    NodeType::StructLiteral {
                                        identifier: identifier.clone().into(),
                                        value: ObjectType::Map(fields),
                                    },
                                ))])),
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
            Node::new(
                span,
                NodeType::ImplTraitDeclaration {
                    generics: GenericTypes::default(),
                    trait_ident: PotentialGenericTypeIdentifier::Identifier(
                        PotentialDollarIdentifier::Identifier(ParserText::from(
                            "Default".to_string(),
                        )),
                    ),
                    target: PotentialNewType::DataType(ParserDataType::new(
                        span,
                        ParserInnerType::Struct(identifier.text),
                    )),
                    variables: vec![default_fn],
                },
            ),
        ))
    }
}
