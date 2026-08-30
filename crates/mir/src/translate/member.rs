use crate::{
    ast::{MiddleNode, MiddleNodeType, MirField, MirIndex},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
    typing::MiddleTypeDefType,
};
use calibre_parser::{
    Span,
    ast::{
        Operator,
        idents::PotentialDollarIdentifier,
        nodes::{AstNode, AstNodeType, CallArg},
        types::ParserDataType,
    },
};

impl MiddleEnvironment {
    pub(crate) fn evaluate_field_access(
        &mut self,
        scope: ScopeId,
        span: Span,
        base: AstNode,
        field: PotentialDollarIdentifier,
    ) -> Result<MiddleNode, MiddleErr> {
        let field_name = self.resolve(scope, &field, ResolutionOptions::default().with_dollar())?;

        if let AstNodeType::Identifier(ident) = &base.node_type
            && let Ok(ty) = self.resolve_to_data_type(scope, ident)
        {
            if let Some(member) = self.typing.find_impl_member(&ty, &field_name) {
                return Ok(MiddleNode::identifier(span, member.symbol_name.clone()));
            }

            if let Some(member) = self.resolve_impl_member(scope, &ty, &field_name) {
                return Ok(MiddleNode::identifier(span, member));
            }

            if let Some(MiddleTypeDefType::Enum { .. }) = self
                .typing
                .find_object_for_struct_name(&ty.impl_name())
                .map(|x| &x.object_type)
            {
                return self.evaluate_inner(
                    scope,
                    AstNode::new(
                        span,
                        AstNodeType::EnumExpression {
                            identifier: ident.clone(),
                            value: PotentialDollarIdentifier::new(span, field_name),
                            data: None,
                        },
                    ),
                );
            }
        }

        if let Some(ty) = self.resolve_type_from_node(scope, &base)
            && let Some(x) = self
                .typing
                .find_impl_member(&ty, &field_name)
                .map(|x| x.symbol_name.clone())
        {
            return Ok(MiddleNode::identifier(span, x));
        }

        Ok(MiddleNode::new(
            MiddleNodeType::FieldAccess(MirField {
                base: Box::new(self.evaluate_inner(scope, base)?),
                field: field_name.into(),
            }),
            span,
        ))
    }

    pub(crate) fn evaluate_scope_access(
        &mut self,
        scope: ScopeId,
        span: Span,
        base: AstNode,
        field: PotentialDollarIdentifier,
    ) -> Result<MiddleNode, MiddleErr> {
        let mut module_path = Vec::new();
        if base.scope_access_path(&mut module_path)
            && let Ok(new_scope) = self
                .get_scope_list(scope, module_path.clone())
                .or_else(|_| self.import_scope_list(scope, module_path).map(|x| x.0))
        {
            let resolved = self.resolve(new_scope, &field, ResolutionOptions::all())?;

            return Ok(self.evaluate(new_scope, AstNode::identifier(span, &resolved)));
        }

        Err(MiddleErr::Scope(format!(
            "Unable to resolve scope expression : {}::{}",
            base, field
        )))
    }

    pub(crate) fn evaluate_index_access(
        &mut self,
        scope: ScopeId,
        span: Span,
        base: AstNode,
        index: AstNode,
    ) -> Result<MiddleNode, MiddleErr> {
        if let Some(overloaded) = self.handle_operator_overloads(
            scope,
            span,
            base.clone(),
            index.clone(),
            Operator::Index,
        )? {
            return Ok(overloaded);
        }

        Ok(MiddleNode::new(
            MiddleNodeType::IndexAccess(MirIndex {
                base: Box::new(self.evaluate(scope, base)),
                index: Box::new(self.evaluate(scope, index)),
            }),
            span,
        ))
    }

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
    ) -> Option<String> {
        let resolved = self
            .resolve_data_type(scope, data_type, ResolutionOptions::typing())
            .ok()?;
        self.typing
            .find_impl_member(&resolved, member)
            .map(|x| x.symbol_name.clone())
    }
}
