use crate::{
    ast::{MiddleNode, MiddleNodeType, MirField, MirIndex, types::MirDataType},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    symbols::resolve::{KeyOrAstNode, ResolutionOptions},
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
}

impl MirLowering for AstField {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        let field_name = env
            .resolve(
                scope,
                &self.field,
                ResolutionOptions::default().with_dollar(),
            )?
            .unwrap_dollar();

        if let AstNodeType::Identifier(ident) = &self.base.node_type
            && let Ok(ty) = env.resolve_to_data_type(scope, &ident.value)
        {
            if let Some(member) = env.typing.find_impl_member(&ty, field_name) {
                return Ok(MiddleNode::identifier(span, member.symbol_name.clone()));
            }

            if let MirDataType::Struct { identifier, .. } = ty
                && let Some(MiddleTypeDefType::Enum { .. }) = env
                    .typing
                    .find_object_for_struct_name(&identifier)
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
                .find_impl_member(&ty, field_name)
                .map(|x| x.symbol_name.clone())
        {
            return Ok(MiddleNode::identifier(span, x));
        }

        Ok(MiddleNode::new(
            MiddleNodeType::FieldAccess(MirField {
                base: Box::new(self.base.clone().lower(env, scope, span).map_err(|_| {
                    env.context.err_at_span(span, MiddleErr::FieldAccess(self.base.to_string(), field_name.to_string()))
                })?),
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
    ) -> Option<MirDataType> {
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
            .map(|x| x.unwrap_dollar())
            .unwrap_or_else(|_| Ustr::from(self.field.text()));

        if let Some(member_type) = env.resolve_member_fn_type(&ty, &member) {
            return Some(member_type);
        }

        if let MirDataType::Struct { identifier, .. } = &ty
            && let Some(MiddleTypeDefType::Enum { .. }) = env
                .typing
                .find_object_for_struct_name(identifier)
                .map(|x| &x.object_type)
        {
            return Some(ty);
        }

        env.resolve_member_field_type(scope, &ty, &member)
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
            let resolved = env
                .resolve(
                    new_scope,
                    &self.field,
                    ResolutionOptions::default().with_dollar(),
                )?
                .unwrap_dollar();

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
    ) -> Option<MirDataType> {
        let mut module_path = Vec::new();
        if self.base.scope_access_path(&mut module_path)
            && let Ok(member_scope) = env
                .get_scope_list(scope, &module_path.clone())
                .or_else(|_| env.import_scope_list(scope, &module_path).map(|x| x.0))
        {
            let resolved = env
                .resolve(member_scope, &self.field, ResolutionOptions::idents())
                .map(|x| x.unwrap_variable())
                .ok()?;

            return env
                .symbols
                .variables
                .get(&resolved)
                .map(|x| x.data_type.clone());
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
    ) -> Option<MirDataType> {
        let base_type = self.base.type_of(env, scope, span).map(|x| match x {
            MirDataType::Option(x) => *x,
            x => x,
        });

        let index_type = self.index.type_of(env, scope, span);

        let ref_mutability = base_type.as_ref().and_then(|x| match x {
            MirDataType::Ref(_, x) => Some(*x),
            _ => None,
        });

        let data_type = match (base_type, index_type) {
            (Some(base_type), Some(MirDataType::Range)) => {
                Some(match base_type.unwrap_all_refs() {
                    MirDataType::List(_) => base_type,
                    MirDataType::Str => MirDataType::Str,
                    MirDataType::Range => MirDataType::Range,
                    _ => return None,
                })
            }
            (Some(base_type), _) => Some(match base_type.unwrap_all_refs() {
                MirDataType::List(inner) => *inner.clone(),
                MirDataType::Str => MirDataType::Char,
                MirDataType::Range => MirDataType::Int,
                _ => return None,
            }),
            _ => None,
        };

        data_type.map(|data_type| {
            if let Some(ref_mutability) = ref_mutability {
                MirDataType::Option(Box::new(MirDataType::Ref(
                    Box::new(data_type),
                    ref_mutability,
                )))
            } else {
                MirDataType::Option(Box::new(data_type))
            }
        })
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
                KeyOrAstNode::Key(x) => x.unwrap_variable(),
                KeyOrAstNode::Node(x) => return x.lower(env, scope, span),
            },
        ))
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<MirDataType> {
        match env
            .resolve_potential_node(scope, &self.value, ResolutionOptions::idents())
            .ok()?
        {
            KeyOrAstNode::Key(iden) => env
                .symbols
                .variables
                .get(&iden.unwrap_variable())
                .map(|x| x.data_type.clone()),
            KeyOrAstNode::Node(x) => x.type_of(env, scope, span),
        }
    }
}
