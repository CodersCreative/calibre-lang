use crate::{
    ast::{
        MiddleNode, MiddleNodeType, MirAggregate, MirBig, MirChar, MirEnum, MirFloat, MirInt,
        MirRange, MirString, types::MirDataType,
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
            AstNode,
            functions::CallArg,
            literals::{
                AstBig, AstChar, AstEnum, AstFloat, AstInt, AstRange, AstString, AstStruct,
                AstTuple,
            },
        },
    },
};
use tracing::instrument;
use ustr::Ustr;

// TODO Eventuall allow . syntax for automatic creation of structs and enums:
/* For example:
.{
    field1 : ...,
    field2 : ...
}
and
.EnumVariant : ...
*/

impl MirLowering for AstStruct {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        _data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        let identifier = match env
            .resolve_to_data_type(scope, &self.identifier)?
            .unwrap_all_refs()
        {
            MirDataType::Struct { identifier, .. } => identifier.clone(),
            _ => {
                return Err(MiddleErr::At(
                    span,
                    Box::new(MiddleErr::Object("gen".to_string())),
                ));
            }
        };

        let obj = env.typing.objects.get(&identifier).cloned();

        let value = match self.value {
            ObjectType::Map(x) => {
                let mut map = Vec::new();

                for itm in x {
                    let span = itm.1.span;
                    let mut data_type = None;

                    if !env.context.type_check {
                        let node_ty = itm.1.type_of(env, scope, span);
                        if let Some(obj) = &obj
                            && let MiddleTypeDefType::Struct(fields) = &obj.object_type
                            && let Some((_, (expected_ty, _))) =
                                fields.0.iter().find(|(name, _)| name == &itm.0)
                        {
                            data_type = Some(env.compare_types(
                                Some(expected_ty.clone()),
                                node_ty,
                                Some(&TagInfo::IgnoreInvalidTypeCheck),
                                span,
                            )?);
                        }
                    }

                    map.push((itm.0, itm.1.lower_or_empty(env, scope, span, data_type)));
                }

                map
            }
            ObjectType::Tuple(x) => {
                let mut map = Vec::new();

                for (idx, itm) in x.into_iter().enumerate() {
                    let span = itm.span;
                    let mut data_type = None;

                    if !env.context.type_check {
                        let node_ty = itm.type_of(env, scope, span);
                        if let Some(obj) = &obj
                            && let MiddleTypeDefType::Struct(fields) = &obj.object_type
                        {
                            let field_name = idx.to_string();
                            if let Some((_, (expected_ty, _))) =
                                fields.0.iter().find(|(name, _)| name == &field_name)
                            {
                                data_type = Some(env.compare_types(
                                    Some(expected_ty.clone()),
                                    node_ty,
                                    Some(&TagInfo::IgnoreInvalidTypeCheck),
                                    span,
                                )?);
                            }
                        }
                    }

                    map.push((
                        Ustr::from(&idx.to_string()),
                        itm.lower_or_empty(env, scope, span, data_type),
                    ));
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
        env.resolve_to_data_type(scope, &self.identifier).ok()
    }
}

impl MirLowering for AstEnum {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        _data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        let identifier = env
            .resolve(scope, &self.identifier, ResolutionOptions::typing())?
            .unwrap_typing();

        let raw_variant = self.value.to_string();
        let obj = env.typing.objects.get(&identifier);

        let (value, variant_data_type) = if let Some(obj) = obj
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
                    let mut data_type = None;
                    if !env.context.type_check {
                        let node_ty = data.type_of(env, scope, span);
                        data_type = Some(env.compare_types(
                            node_ty,
                            variant_data_type,
                            Some(&TagInfo::IgnoreInvalidTypeCheck),
                            data.span,
                        )?);
                    }

                    Some(Box::new(data.lower(env, scope, span, data_type)?))
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
    ) -> Option<MirDataType> {
        env.resolve_to_data_type(scope, &self.identifier).ok()
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
        AstNode::call(
            span,
            AstNode::identifier(span, "tuple"),
            self.values.into_iter().map(CallArg::Value).collect(),
        )
        .lower(env, scope, span, data_type)
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
        _data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        if !env.context.type_check {
            let from_type = self.from.type_of(env, scope, span);
            let to_type = self.to.type_of(env, scope, span);

            let data_type = env.compare_types(
                from_type,
                to_type,
                Some(&TagInfo::IgnoreInvalidTypeCheck),
                span,
            )?;

            if !data_type.clone().is_int() {
                return Err(env.context.err_at_span(
                    span,
                    MiddleErr::InvalidType {
                        expected: Box::new(MirDataType::Int),
                        found: Box::new(data_type),
                    },
                ));
            }
        }

        Ok(MiddleNode {
            node_type: MiddleNodeType::RangeDeclaration(MirRange {
                from: Box::new(
                    self.from
                        .lower_or_empty(env, scope, span, Some(MirDataType::Int)),
                ),
                to: Box::new(
                    self.to
                        .lower_or_empty(env, scope, span, Some(MirDataType::Int)),
                ),
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
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        span: Span,
        _data_type: Option<MirDataType>,
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
        _span: Span,
    ) -> Option<MirDataType> {
        Some(MirDataType::Str)
    }
}

impl MirLowering for AstInt {
    #[instrument(skip_all)]
    fn lower(
        self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        span: Span,
        _data_type: Option<MirDataType>,
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
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        span: Span,
        _data_type: Option<MirDataType>,
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
        _span: Span,
    ) -> Option<MirDataType> {
        Some(MirDataType::Big)
    }
}

impl MirLowering for AstFloat {
    #[instrument(skip_all)]
    fn lower(
        self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        span: Span,
        _data_type: Option<MirDataType>,
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
        _span: Span,
    ) -> Option<MirDataType> {
        Some(MirDataType::Float)
    }
}

impl MirLowering for AstChar {
    #[instrument(skip_all)]
    fn lower(
        self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        span: Span,
        _data_type: Option<MirDataType>,
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
        _span: Span,
    ) -> Option<MirDataType> {
        Some(MirDataType::Char)
    }
}
