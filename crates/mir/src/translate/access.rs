use crate::{
    ast::{MiddleNode, MiddleNodeType, MirField, MirIndex},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    symbols::resolve::{ResolutionOptions, StrOrAstNode},
    translate::MirLowering,
    typing::MiddleTypeDefType,
};
use calibre_parser::{
    Span,
    ast::{
        Operator,
        idents::PotentialDollarIdentifier,
        nodes::{
            AstNode, AstNodeType,
            access::{AstField, AstIdentifier, AstIndex, AstScope},
            functions::CallArg,
            literals::AstEnum,
        },
        types::{ParserDataType, ParserInnerType},
    },
};
use tracing::instrument;
use ustr::Ustr;

impl MiddleEnvironment {
    #[inline]
    pub(crate) fn lower_call_args(
        &mut self,
        scope: ScopeId,
        args: Vec<CallArg>,
        reverse_args: Vec<AstNode>,
    ) -> Box<[MiddleNode]> {
        args.into_iter()
            .map(AstNode::from)
            .chain(reverse_args)
            .map(|arg| {
                let span = arg.span;
                arg.lower_or_empty(self, scope, span)
            })
            .collect()
    }

    pub fn resolve_impl_member(
        &mut self,
        scope: ScopeId,
        data_type: &ParserDataType,
        member: &impl ToString,
    ) -> Option<Ustr> {
        let resolved = self
            .resolve_data_type(scope, data_type, ResolutionOptions::typing())
            .ok()?;
        self.typing
            .find_impl_member(&resolved, member)
            .map(|x| x.symbol_name)
    }
}

impl MirLowering for AstField {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        let field_name = env.resolve(
            scope,
            &self.field,
            ResolutionOptions::default().with_dollar(),
        )?;

        if let AstNodeType::Identifier(ident) = &self.base.node_type
            && let Ok(ty) = env.resolve_to_data_type(scope, &ident.value)
        {
            if let Some(member) = env.typing.find_impl_member(&ty, &field_name) {
                return Ok(MiddleNode::identifier(span, member.symbol_name));
            }

            if let Some(member) = env.resolve_impl_member(scope, &ty, &field_name) {
                return Ok(MiddleNode::identifier(span, member));
            }

            if let Some(MiddleTypeDefType::Enum { .. }) = env
                .typing
                .find_object_for_struct_name(&Ustr::from(&ty.impl_name()))
                .map(|x| &x.object_type)
            {
                return AstNode::new(
                    span,
                    AstNodeType::EnumExpression(AstEnum {
                        identifier: ident.value.clone(),
                        value: PotentialDollarIdentifier::new(span, field_name),
                        data: None,
                    }),
                )
                .lower(env, scope, span);
            }
        }

        if let Some(ty) = self.base.type_of(env, scope, span)
            && let Some(x) = env
                .typing
                .find_impl_member(&ty, &field_name)
                .map(|x| x.symbol_name)
        {
            return Ok(MiddleNode::identifier(span, x));
        }

        Ok(MiddleNode::new(
            MiddleNodeType::FieldAccess(MirField {
                base: Box::new(self.base.lower(env, scope, span)?),
                field: field_name,
            }),
            span,
        ))
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        let ty = self.base.type_of(env, scope, span).or_else(|| {
            if let AstNodeType::Identifier(id) = &self.base.node_type {
                env.resolve_to_data_type(scope, &id.value).ok()
            } else {
                None
            }
        })?;

        let member = env
            .resolve(
                scope,
                &self.field,
                ResolutionOptions::default().with_dollar(),
            )
            .unwrap_or_else(|_| Ustr::from(self.field.text()));

        if let Some(member_type) = env.resolve_member_fn_type(&ty, &member) {
            return Some(member_type);
        }

        if let Some(MiddleTypeDefType::Enum { .. }) = env
            .typing
            .find_object_for_struct_name(&Ustr::from(&ty.impl_name()))
            .map(|x| &x.object_type)
        {
            return Some(ty);
        }

        env.resolve_member_field_type(scope, &ty, &member, span)
    }
}

impl MirLowering for AstScope {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        let mut module_path = Vec::new();
        if self.base.scope_access_path(&mut module_path)
            && let Ok(new_scope) = env
                .get_scope_list(scope, &module_path)
                .or_else(|_| env.import_scope_list(scope, &module_path).map(|x| x.0))
        {
            let resolved = env.resolve(
                new_scope,
                &self.field,
                ResolutionOptions::default().with_dollar(),
            )?;

            return AstNode::identifier(span, resolved).lower(env, new_scope, span);
        }

        Err(MiddleErr::Scope(format!(
            "Unable to resolve scope expression : {}::{}",
            self.base, self.field
        )))
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        _span: Span,
    ) -> Option<ParserDataType> {
        let mut module_path = Vec::new();
        if self.base.scope_access_path(&mut module_path) {
            let member = env
                .resolve(
                    scope,
                    &self.field,
                    ResolutionOptions::default().with_dollar(),
                )
                .unwrap_or_else(|_| Ustr::from(self.field.text()));

            if let Ok(member_scope) = env
                .get_scope_list(scope, &module_path.clone())
                .or_else(|_| env.import_scope_list(scope, &module_path).map(|x| x.0))
            {
                let resolved = env
                    .resolve(member_scope, &self.field, ResolutionOptions::idents())
                    .unwrap_or(member);

                return env
                    .symbols
                    .variables
                    .get(&resolved)
                    .map(|x| x.data_type.clone());
            }
        }

        None
    }
}

impl MirLowering for AstIndex {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        if let Some(overloaded) = env.handle_operator_overloads(
            scope,
            span,
            *self.base.clone(),
            *self.index.clone(),
            Operator::Index,
        )? {
            return Ok(overloaded);
        }

        Ok(MiddleNode::new(
            MiddleNodeType::IndexAccess(MirIndex {
                base: Box::new(self.base.lower_or_empty(env, scope, span)),
                index: Box::new(self.index.lower_or_empty(env, scope, span)),
            }),
            span,
        ))
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        let base_type = self
            .base
            .type_of(env, scope, span)
            .or_else(|| {
                if let AstNodeType::Identifier(id) = &self.base.node_type {
                    env.resolve_to_data_type(scope, &id.value).ok()
                } else {
                    None
                }
            })
            .map(|x| x.data_type);

        let index_type = self
            .index
            .type_of(env, scope, span)
            .or_else(|| {
                if let AstNodeType::Identifier(id) = &self.base.node_type {
                    env.resolve_to_data_type(scope, &id.value).ok()
                } else {
                    None
                }
            })
            .map(|x| x.data_type);

        match (base_type, index_type) {
            (Some(base_type), Some(ParserInnerType::Range)) => Some(match base_type {
                ParserInnerType::List(_) => ParserDataType::new(
                    span,
                    ParserInnerType::Option(Box::new(ParserDataType::new(span, base_type))),
                ),
                ParserInnerType::Str => ParserDataType::new(
                    span,
                    ParserInnerType::Option(Box::new(ParserDataType::new(
                        span,
                        ParserInnerType::Str,
                    ))),
                ),
                ParserInnerType::Range => ParserDataType::new(
                    span,
                    ParserInnerType::Option(Box::new(ParserDataType::new(
                        span,
                        ParserInnerType::Range,
                    ))),
                ),
                _ => return None,
            }),
            (Some(base_type), _) => Some(match base_type {
                ParserInnerType::List(inner) => {
                    ParserDataType::new(span, ParserInnerType::Option(inner))
                }
                ParserInnerType::Str => ParserDataType::new(
                    span,
                    ParserInnerType::Option(Box::new(ParserDataType::new(
                        span,
                        ParserInnerType::Char,
                    ))),
                ),
                ParserInnerType::Range => ParserDataType::new(
                    span,
                    ParserInnerType::Option(Box::new(ParserDataType::new(
                        span,
                        ParserInnerType::Int,
                    ))),
                ),
                _ => return None,
            }),
            _ => None,
        }
    }
}

impl MirLowering for AstIdentifier {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        Ok(MiddleNode::identifier(
            span,
            match env.resolve_potential_node(scope, &self.value, ResolutionOptions::idents())? {
                StrOrAstNode::Str(x) => x,
                StrOrAstNode::Node(x) => return x.lower(env, scope, span),
            },
        ))
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        match env
            .resolve_potential_node(scope, &self.value, ResolutionOptions::idents())
            .ok()?
        {
            StrOrAstNode::Str(iden) => env
                .symbols
                .variables
                .get(&iden)
                .map(|x| x.data_type.clone()),
            StrOrAstNode::Node(x) => x.type_of(env, scope, span),
        }
    }
}
