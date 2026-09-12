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
        idents::{ParsedIntLiteral, PotentialDollarIdentifier},
        nodes::{
            AstNode, AstNodeType,
            access::{AstField, AstIdentifier, AstIndex, AstScope},
            functions::CallArg,
            literals::AstEnum,
        },
        types::{ParserDataType, ParserInnerType},
    },
};
use ustr::Ustr;

impl MiddleEnvironment {
    #[inline]
    pub(crate) fn lower_call_args(
        &mut self,
        scope: ScopeId,
        args: Vec<CallArg>,
        reverse_args: Vec<AstNode>,
    ) -> Vec<MiddleNode> {
        let mut lowered = Vec::with_capacity(args.len() + reverse_args.len());

        for arg in args {
            lowered.push(self.evaluate(scope, arg.into()));
        }

        for arg in reverse_args {
            lowered.push(self.evaluate(scope, arg));
        }

        lowered
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
        let base_type = self.base.type_of(env, scope, span).or_else(|| {
            if let AstNodeType::Identifier(id) = &self.base.node_type {
                env.resolve_to_data_type(scope, &id.value).ok()
            } else {
                None
            }
        });

        if let Some(base_type) = base_type {
            let resolved_type =
                match env.resolve_data_type(scope, &base_type, ResolutionOptions::typing()) {
                    Ok(ty) => ty.unwrap_all_refs(),
                    Err(_) => return Some(ParserDataType::auto(span)),
                };

            let index_type = match resolved_type.data_type {
                ParserInnerType::List(inner)
                | ParserInnerType::Option(inner)
                | ParserInnerType::Ptr(inner) => *inner,
                ParserInnerType::Tuple(values) => match &self.index.node_type {
                    AstNodeType::IntLiteral(i) => ParsedIntLiteral::parse(&i.value)
                        .and_then(|idx| values.get(idx.value as usize).cloned())
                        .unwrap_or_else(|| ParserDataType::new(span, ParserInnerType::Auto(None))),
                    _ => ParserDataType::new(span, ParserInnerType::Auto(None)),
                },
                ParserInnerType::Result { ok, err } => {
                    if ok.data_type == err.data_type {
                        *ok
                    } else {
                        ParserDataType::new(span, ParserInnerType::Dynamic)
                    }
                }
                _ => ParserDataType::new(span, ParserInnerType::Auto(None)),
            };
            Some(index_type)
        } else {
            Some(ParserDataType::auto(span))
        }
    }
}

impl MirLowering for AstIdentifier {
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
