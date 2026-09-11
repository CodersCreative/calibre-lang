use crate::{
    ast::{
        MiddleNode, MiddleNodeType, MirAggregate, MirBig, MirChar, MirEnum, MirFloat, MirInt,
        MirRange, MirString,
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
        idents::ParsedIntLiteral,
        nodes::{
            AstNode, CallArg,
            literals::{
                AstBig, AstChar, AstEnum, AstFloat, AstInt, AstRange, AstString, AstStruct,
                AstTuple,
            },
        },
        types::{ParserDataType, ParserInnerType},
    },
};
use ustr::Ustr;

impl MirLowering for AstStruct {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        let identifier = env.resolve(scope, &self.identifier, ResolutionOptions::typing())?;
        let obj = env.typing.objects.get(&identifier).cloned();

        if obj.is_none()
            && !env
                .tagging
                .tag_info
                .contains(&TagInfo::IgnoreInvalidTypeCheck)
        {
            return Err(MiddleErr::At(
                span,
                Box::new(MiddleErr::Object(identifier.to_string())),
            ));
        };

        let value = match self.value {
            ObjectType::Map(x) => {
                let mut map = Vec::new();

                for itm in x {
                    if !env.context.type_check {
                        let node_ty = itm.1.type_of(env, scope, span);
                        if let Some(obj) = &obj
                            && let MiddleTypeDefType::Struct(fields) = &obj.object_type
                            && let Some((_, (expected_ty, _))) =
                                fields.0.iter().find(|(name, _)| name == &itm.0)
                        {
                            env.compare_types_ref(
                                Some(expected_ty),
                                node_ty.as_ref(),
                                Some(&TagInfo::IgnoreInvalidTypeCheck),
                            )?;
                        }
                    }
                    map.push((itm.0, itm.1.lower_or_empty(env, scope, span)));
                }

                map
            }
            ObjectType::Tuple(x) => {
                let mut map = Vec::new();

                for (idx, itm) in x.into_iter().enumerate() {
                    if !env.context.type_check {
                        let node_ty = itm.type_of(env, scope, span);
                        if let Some(obj) = &obj
                            && let MiddleTypeDefType::Struct(fields) = &obj.object_type
                        {
                            let field_name = idx.to_string();
                            if let Some((_, (expected_ty, _))) =
                                fields.0.iter().find(|(name, _)| name == &field_name)
                            {
                                env.compare_types_ref(
                                    Some(expected_ty),
                                    node_ty.as_ref(),
                                    Some(&TagInfo::IgnoreInvalidTypeCheck),
                                )?;
                            }
                        }
                    }
                    map.push((idx.to_string(), itm.lower_or_empty(env, scope, span)));
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
    ) -> Option<ParserDataType> {
        env.resolve_to_data_type(scope, &self.identifier).ok()
    }
}

impl MirLowering for AstEnum {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        let identifier = env.resolve(scope, &self.identifier, ResolutionOptions::typing())?;

        let raw_variant = self.value.to_string();
        let obj = env.typing.objects.get(&identifier);

        let (value, data_type) = if let Some(obj) = obj
            && let MiddleTypeDefType::Enum { variants, .. } = &obj.object_type
        {
            variants
                .iter()
                .find(|(name, _)| name.eq_ignore_ascii_case(&raw_variant))
                .map(|(name, x)| (name, x.clone()))
                .ok_or(MiddleErr::At(
                    span,
                    Box::new(MiddleErr::EnumVariant(raw_variant.clone())),
                ))?
        } else {
            return Err(MiddleErr::At(
                span,
                Box::new(MiddleErr::Object(identifier.to_string())),
            ));
        };

        Ok(MiddleNode {
            node_type: MiddleNodeType::EnumExpression(MirEnum {
                identifier,
                value: *value,
                data: if let Some(data) = self.data {
                    if !env.context.type_check {
                        let node_ty = data.type_of(env, scope, span);
                        env.compare_types_ref(
                            node_ty.as_ref(),
                            data_type.as_ref(),
                            Some(&TagInfo::IgnoreInvalidTypeCheck),
                        )?;
                    }

                    Some(Box::new(data.lower(env, scope, span)?))
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
        _span: Span,
    ) -> Option<ParserDataType> {
        env.resolve_to_data_type(scope, &self.identifier).ok()
    }
}

impl MirLowering for AstTuple {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        AstNode::call(
            span,
            AstNode::identifier(span, "tuple"),
            self.values.into_iter().map(CallArg::Value).collect(),
        )
        .lower(env, scope, span)
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        let mut types = Vec::new();

        for value in &self.values {
            types.push(value.type_of(env, scope, span)?);
        }

        Some(ParserDataType::new(span, ParserInnerType::Tuple(types)))
    }
}

impl MirLowering for AstRange {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        if !env.context.type_check {
            let from_type = self.from.type_of(env, scope, span);
            let to_type = self.to.type_of(env, scope, span);

            let data_type =
                env.compare_types(from_type, to_type, Some(&TagInfo::IgnoreInvalidTypeCheck))?;

            if !data_type.clone().is_int() {
                return Err(env.context.err_at_current(MiddleErr::InvalidType {
                    expected: Box::new(ParserDataType::new(span, ParserInnerType::Int)),
                    found: Box::new(data_type),
                }));
            }
        }

        Ok(MiddleNode {
            node_type: MiddleNodeType::RangeDeclaration(MirRange {
                from: Box::new(self.from.lower_or_empty(env, scope, span)),
                to: Box::new(self.to.lower_or_empty(env, scope, span)),
                inclusive: self.inclusive,
            }),
            span,
        })
    }

    fn type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        Some(ParserDataType {
            data_type: ParserInnerType::Range,
            span,
        })
    }
}

impl MirLowering for AstString {
    fn lower(
        self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        Ok(MiddleNode {
            node_type: MiddleNodeType::StringLiteral(MirString {
                value: Ustr::from(&self.value.text),
            }),
            span,
        })
    }

    fn type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        Some(ParserDataType {
            data_type: ParserInnerType::Str,
            span,
        })
    }
}

impl MirLowering for AstInt {
    fn lower(
        self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
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

    fn type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        Some(ParserDataType {
            data_type: if self.value.ends_with('b') {
                ParserInnerType::Byte
            } else if self.value.ends_with('u') {
                ParserInnerType::UInt
            } else {
                ParserInnerType::Int
            },
            span,
        })
    }
}

impl MirLowering for AstBig {
    fn lower(
        self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
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

    fn type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        Some(ParserDataType {
            data_type: ParserInnerType::Big,
            span,
        })
    }
}

impl MirLowering for AstFloat {
    fn lower(
        self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        Ok(MiddleNode {
            node_type: MiddleNodeType::FloatLiteral(MirFloat { value: self.value }),
            span,
        })
    }

    fn type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        Some(ParserDataType {
            data_type: ParserInnerType::Float,
            span,
        })
    }
}

impl MirLowering for AstChar {
    fn lower(
        self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        Ok(MiddleNode {
            node_type: MiddleNodeType::CharLiteral(MirChar { value: self.value }),
            span,
        })
    }

    fn type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        Some(ParserDataType {
            data_type: ParserInnerType::Char,
            span,
        })
    }
}
