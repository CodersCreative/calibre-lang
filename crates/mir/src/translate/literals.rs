use crate::{
    ast::{
        MiddleNode, MiddleNodeType, MirAggregate, MirBig, MirChar, MirEnum, MirFloat, MirInt,
        MirRange, MirString, types::MirDataType, typing::MirTypable,
    },
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
    tags::TagInfo,
    translate::MirLowering,
    typing::MiddleTypeDefType,
};
use calibre_parser::{
    Span,
    ast::{
        ObjectMap, ObjectType,
        idents::{IntLiteralType, ParsedIntLiteral},
        nodes::{
            AstNode, AstNodeType,
            binary::{AsFailureMode, AstAs},
            functions::CallArg,
            literals::{
                AstBig, AstChar, AstEnum, AstFloat, AstInt, AstRange, AstString, AstStruct,
                AstTuple,
            },
        },
    },
};
use tracing::instrument;
use ustr::{Ustr, UstrSet};

impl MirLowering for AstStruct {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        let identifier = if let Some(ident) = self.identifier {
            match env.resolve_to_data_type(scope, &ident)?.unwrap_all_refs() {
                MirDataType::Struct { identifier, .. } => identifier.clone(),
                _ => {
                    return Err(MiddleErr::At(
                        span,
                        Box::new(MiddleErr::Object("gen".to_string())),
                    ));
                }
            }
        } else {
            match data_type {
                Some(MirDataType::Struct { identifier, .. }) => identifier.clone(),
                _ => {
                    return Err(MiddleErr::At(
                        span,
                        Box::new(MiddleErr::CannotInferFromExpression(
                            "struct type inference".to_string(),
                        )),
                    ));
                }
            }
        };

        let obj = env.typing.objects.get(&identifier).cloned();

        let value = match self.value {
            ObjectType::Map(x) => {
                let mut map = Vec::new();

                if let Some(obj) = &obj
                    && let MiddleTypeDefType::Struct(fields) = &obj.object_type
                    && let Some(x) = fields
                        .iter()
                        .find(|field| !x.iter().any(|x| x.0 == field.0))
                {
                    return Err(env
                        .context
                        .err_at_span(span, MiddleErr::MissingStructField(x.0.to_string())));
                }

                let mut field_hashmap: UstrSet = UstrSet::default();

                for itm in x {
                    let item_span = itm.1.span;
                    let mut expected_type = None;

                    if let Some(obj) = &obj
                        && let MiddleTypeDefType::Struct(fields) = &obj.object_type
                    {
                        if let Some((_, (expected_ty, _))) =
                            fields.0.iter().find(|(name, _)| name == &itm.0)
                        {
                            expected_type = Some(expected_ty.clone());

                            if !field_hashmap.insert(itm.0) {
                                return Err(env.context.err_at_span(
                                    item_span,
                                    MiddleErr::StructFieldMultiple(itm.0.to_string()),
                                ));
                            }
                        } else {
                            return Err(env.context.err_at_span(
                                item_span,
                                MiddleErr::InvalidStructField {
                                    field: itm.0.to_string(),
                                    available: fields.0.iter().map(|x| x.0.to_string()).collect(),
                                    data_type: identifier.name().to_string(),
                                },
                            ));
                        }
                    }

                    let item = itm
                        .1
                        .lower_or_empty(env, scope, item_span, expected_type.clone());

                    if env.context.type_check
                        && let Some(expected_ty) = expected_type
                    {
                        let lowered_ty = item.mir_type_of(env, scope, item_span);
                        env.compare_types(
                            Some(expected_ty),
                            lowered_ty,
                            Some(&TagInfo::IgnoreInvalidTypeCheck),
                            item_span,
                        )?;
                    }

                    map.push((itm.0, item));
                }

                map
            }
            ObjectType::Tuple(x) => {
                let mut map = Vec::new();

                for (idx, itm) in x.into_iter().enumerate() {
                    let item_span = itm.span;
                    let field_name = idx.to_string();
                    let mut expected_type = None;

                    if let Some(obj) = &obj
                        && let MiddleTypeDefType::Struct(fields) = &obj.object_type
                        && let Some((_, (expected_ty, _))) =
                            fields.0.iter().find(|(name, _)| name == &field_name)
                    {
                        expected_type = Some(expected_ty.clone());
                    }

                    let item = itm.lower_or_empty(env, scope, item_span, expected_type.clone());

                    if env.context.type_check
                        && let Some(expected_ty) = expected_type
                    {
                        let lowered_ty = item.mir_type_of(env, scope, item_span);
                        env.compare_types(
                            Some(expected_ty),
                            lowered_ty,
                            Some(&TagInfo::IgnoreInvalidTypeCheck),
                            item_span,
                        )?;
                    }

                    map.push((Ustr::from(&idx.to_string()), item));
                }

                map
            }
        };

        Ok(MiddleNode {
            node_type: MiddleNodeType::AggregateExpression(MirAggregate {
                identifier: Some(identifier),
                value: ObjectMap(value),
            }),
            span,
        })
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        self.identifier
            .as_ref()
            .and_then(|ident| env.resolve_to_data_type(scope, ident).ok())
    }
}

impl MirLowering for AstEnum {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        let variant = env
            .resolve(
                scope,
                &self.value,
                ResolutionOptions::default().with_dollar(),
            )?
            .unwrap_dollar();

        if self.identifier.is_none() {
            let expected_type = data_type.as_ref().map(|t| t.unwrap_all_refs());

            match (variant.as_str(), expected_type) {
                ("Ok", Some(MirDataType::Result { .. })) => {
                    if let Some(data) = self.data {
                        return AstNode::call(
                            span,
                            AstNode::identifier(span, "ok"),
                            vec![CallArg::Value(*data)],
                        )
                        .lower(env, scope, span, data_type);
                    } else {
                        return Err(MiddleErr::At(
                            span,
                            Box::new(MiddleErr::CannotInferFromExpression(
                                "ok variant requires a value".to_string(),
                            )),
                        ));
                    }
                }
                ("Err", Some(MirDataType::Result { .. })) => {
                    if let Some(data) = self.data {
                        return AstNode::call(
                            span,
                            AstNode::identifier(span, "err"),
                            vec![CallArg::Value(*data)],
                        )
                        .lower(env, scope, span, data_type);
                    } else {
                        return Err(MiddleErr::At(
                            span,
                            Box::new(MiddleErr::CannotInferFromExpression(
                                "err variant requires a value".to_string(),
                            )),
                        ));
                    }
                }
                ("Some", Some(MirDataType::Option(_))) => {
                    if let Some(data) = self.data {
                        return AstNode::call(
                            span,
                            AstNode::identifier(span, "some"),
                            vec![CallArg::Value(*data)],
                        )
                        .lower(env, scope, span, data_type);
                    } else {
                        return Err(MiddleErr::At(
                            span,
                            Box::new(MiddleErr::CannotInferFromExpression(
                                "some variant requires a value".to_string(),
                            )),
                        ));
                    }
                }
                ("None", Some(MirDataType::Option(_))) => {
                    return AstNode::none(span).lower(env, scope, span, data_type);
                }
                _ => {}
            }
        }

        let identifier = if let Some(ident) = self.identifier {
            env.resolve(scope, &ident, ResolutionOptions::typing())?
                .unwrap_typing()
        } else {
            match data_type {
                Some(MirDataType::Struct { identifier, .. }) => identifier.clone(),
                _ => {
                    return Err(MiddleErr::At(
                        span,
                        Box::new(MiddleErr::CannotInferFromExpression(
                            "enum type inference".to_string(),
                        )),
                    ));
                }
            }
        };

        let obj = env.typing.objects.get(&identifier);

        let (value, variant_data_type) = if let Some(obj) = obj
            && let MiddleTypeDefType::Enum { variants, .. } = &obj.object_type
        {
            variants
                .iter()
                .find(|(name, _)| name.eq_ignore_ascii_case(&variant))
                .map(|(name, x)| (name, x.clone()))
                .ok_or(MiddleErr::At(
                    span,
                    Box::new(MiddleErr::EnumVariant(variant.to_string())),
                ))?
        } else {
            return Err(MiddleErr::At(
                span,
                Box::new(MiddleErr::Object(identifier.to_string())),
            ));
        };

        Ok(MiddleNode {
            node_type: MiddleNodeType::EnumExpression(MirEnum {
                identifier: Some(identifier),
                value: *value,
                data: if let Some(data) = self.data {
                    let data_span = data.span;

                    let data = data.lower(env, scope, data_span, variant_data_type.clone())?;

                    if env.context.type_check {
                        let lowered_ty = data.mir_type_of(env, scope, data_span);
                        env.compare_types(
                            lowered_ty,
                            variant_data_type,
                            Some(&TagInfo::IgnoreInvalidTypeCheck),
                            data_span,
                        )?;
                    }

                    Some(Box::new(data))
                } else {
                    None
                },
            }),
            span,
        })
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<MirDataType> {
        if let Some(x) = self
            .identifier
            .as_ref()
            .and_then(|ident| env.resolve_to_data_type(scope, ident).ok())
        {
            Some(x)
        } else {
            let variant = env
                .resolve(
                    scope,
                    &self.value,
                    ResolutionOptions::default().with_dollar(),
                )
                .ok()?
                .unwrap_dollar();

            match variant.as_str() {
                "Some" => Some(MirDataType::Option(Box::new(
                    self.data.as_ref()?.type_of(env, scope, span)?,
                ))),
                "None" => Some(MirDataType::Option(Box::new(MirDataType::Dynamic))),
                "Ok" => Some(MirDataType::Result {
                    ok: Box::new(self.data.as_ref()?.type_of(env, scope, span)?),
                    err: Box::new(MirDataType::Dynamic),
                }),
                "Err" => Some(MirDataType::Result {
                    ok: Box::new(MirDataType::Dynamic),
                    err: Box::new(self.data.as_ref()?.type_of(env, scope, span)?),
                }),
                _ => None,
            }
        }
    }
}

impl MirLowering for AstTuple {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        if let Some(data_type) = data_type.clone()
            && !matches!(data_type, MirDataType::Tuple(_))
        {
            AstAs {
                value: Box::new(AstNode::new(span, AstNodeType::TupleLiteral(self))),
                failure_mode: AsFailureMode::Panic,
                data_type: data_type.into(),
            }
            .lower(env, scope, span, None)
        } else {
            AstNode::call(
                span,
                AstNode::identifier(span, "tuple"),
                self.values.into_iter().map(CallArg::Value).collect(),
            )
            .lower(env, scope, span, data_type)
        }
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<MirDataType> {
        let mut types = Vec::new();

        for value in &self.values {
            types.push(value.type_of(env, scope, span)?);
        }

        Some(MirDataType::Tuple(types))
    }
}

impl MirLowering for AstRange {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        if let Some(data_type) = data_type
            && !matches!(data_type, MirDataType::Range)
        {
            return AstAs {
                value: Box::new(AstNode::new(span, AstNodeType::RangeDeclaration(self))),
                failure_mode: AsFailureMode::Panic,
                data_type: data_type.into(),
            }
            .lower(env, scope, span, None);
        }

        let from = self
            .from
            .lower_or_empty(env, scope, span, Some(MirDataType::Int));

        let to = self
            .to
            .lower_or_empty(env, scope, span, Some(MirDataType::Int));

        if env.context.type_check {
            let from_type = from.mir_type_of(env, scope, span);
            let to_type = to.mir_type_of(env, scope, span);

            if !(from_type.as_ref().is_none_or(|x| x.is_int())
                && to_type.as_ref().is_none_or(|x| x.is_int()))
            {
                return Err(env.context.err_at_span(
                    span,
                    MiddleErr::InvalidType {
                        expected: Box::new(MirDataType::Int),
                        found: Box::new(from_type.or(to_type).unwrap()),
                    },
                ));
            }
        }

        Ok(MiddleNode {
            node_type: MiddleNodeType::RangeDeclaration(MirRange {
                from: Box::new(from),
                to: Box::new(to),
                inclusive: self.inclusive,
            }),
            span,
        })
    }

    fn type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        Some(MirDataType::Range)
    }
}

impl MirLowering for AstString {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        if let Some(data_type) = data_type
            && !matches!(data_type, MirDataType::Str)
        {
            AstAs {
                value: Box::new(AstNode::new(span, AstNodeType::StringLiteral(self))),
                failure_mode: AsFailureMode::Panic,
                data_type: data_type.into(),
            }
            .lower(env, scope, span, None)
        } else {
            Ok(MiddleNode {
                node_type: MiddleNodeType::StringLiteral(MirString {
                    value: Ustr::from(&self.value.text),
                }),
                span,
            })
        }
    }

    fn type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        Some(MirDataType::Str)
    }
}

impl MirLowering for AstInt {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        if let Some(data_type) = data_type
            && !matches!(data_type, MirDataType::Int)
        {
            AstAs {
                value: Box::new(AstNode::new(span, AstNodeType::IntLiteral(self))),
                failure_mode: AsFailureMode::Panic,
                data_type: data_type.into(),
            }
            .lower(env, scope, span, None)
        } else {
            Ok(MiddleNode {
                node_type: MiddleNodeType::IntLiteral(MirInt {
                    value: ParsedIntLiteral::parse(self.value.clone()).ok_or_else(|| {
                        MiddleErr::At(
                            span,
                            Box::new(MiddleErr::InvalidIntegerLiteral(self.value.to_string())),
                        )
                    })?,
                }),
                span,
            })
        }
    }

    fn type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        Some(match self.value.int_type {
            IntLiteralType::Byte => MirDataType::Byte,
            IntLiteralType::UInt => MirDataType::UInt,
            IntLiteralType::Int => MirDataType::Int,
        })
    }
}

impl MirLowering for AstBig {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        if let Some(data_type) = data_type
            && !matches!(data_type, MirDataType::Big)
        {
            AstAs {
                value: Box::new(AstNode::new(span, AstNodeType::BigLiteral(self))),
                failure_mode: AsFailureMode::Panic,
                data_type: data_type.into(),
            }
            .lower(env, scope, span, None)
        } else {
            Ok(MiddleNode {
                node_type: MiddleNodeType::BigLiteral(MirBig {
                    value: self
                        .value
                        .text
                        .strip_suffix('g')
                        .map(Ustr::from)
                        .unwrap_or(Ustr::from(&self.value.text)),
                }),
                span,
            })
        }
    }

    fn type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        Some(MirDataType::Big)
    }
}

impl MirLowering for AstFloat {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        if let Some(data_type) = data_type
            && !matches!(data_type, MirDataType::Float)
        {
            AstAs {
                value: Box::new(AstNode::new(span, AstNodeType::FloatLiteral(self))),
                failure_mode: AsFailureMode::Panic,
                data_type: data_type.into(),
            }
            .lower(env, scope, span, None)
        } else {
            Ok(MiddleNode {
                node_type: MiddleNodeType::FloatLiteral(MirFloat { value: self.value }),
                span,
            })
        }
    }

    fn type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        Some(MirDataType::Float)
    }
}

impl MirLowering for AstChar {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        if let Some(data_type) = data_type
            && !matches!(data_type, MirDataType::Char)
        {
            AstAs {
                value: Box::new(AstNode::new(span, AstNodeType::CharLiteral(self))),
                failure_mode: AsFailureMode::Panic,
                data_type: data_type.into(),
            }
            .lower(env, scope, span, None)
        } else {
            Ok(MiddleNode {
                node_type: MiddleNodeType::CharLiteral(MirChar { value: self.value }),
                span,
            })
        }
    }

    fn type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        Some(MirDataType::Char)
    }
}
