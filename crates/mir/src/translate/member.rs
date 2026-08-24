use crate::{
    ast::{MiddleNode, MiddleNodeType},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    typing::MiddleTypeDefType,
};
use calibre_parser::{
    Span,
    ast::{
        Operator,
        idents::PotentialDollarIdentifier,
        nodes::{CallArg, Node, NodeType},
        types::ParserDataType,
    },
};

impl MiddleEnvironment {
    pub(crate) fn scope_member_call(span: Span, path: &[&str], args: Vec<CallArg>) -> Node {
        let mut result = Node::identifier(span, path.first().unwrap());
        for segment in path.iter().skip(1) {
            result = Node::new(
                span,
                NodeType::ScopeAccess {
                    base: Box::new(result),
                    field: PotentialDollarIdentifier::new(span, segment),
                },
            );
        }

        Node::call_full(span, result, vec![], args, vec![], None)
    }

    pub(crate) fn evaluate_field_access(
        &mut self,
        scope: &u64,
        span: Span,
        base: Node,
        field: PotentialDollarIdentifier,
    ) -> Result<MiddleNode, MiddleErr> {
        let field_name = self
            .resolve_dollar_ident_only(scope, &field)
            .map(|x| x.text)
            .unwrap_or(field.text().clone());

        if let NodeType::Identifier(ident) = &base.node_type
            && let Some(ty) = self.resolve_potential_generic_ident_to_data_type(scope, &ident)
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
                    Node::new(
                        span,
                        NodeType::EnumExpression {
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
            MiddleNodeType::FieldAccess {
                base: Box::new(self.evaluate_inner(scope, base)?),
                field: field_name.into(),
            },
            span,
        ))
    }

    pub(crate) fn evaluate_scope_access(
        &mut self,
        scope: &u64,
        span: Span,
        base: Node,
        field: PotentialDollarIdentifier,
    ) -> Result<MiddleNode, MiddleErr> {
        let field_name = self
            .resolve_dollar_ident_only(scope, &field)
            .map(|x| x.text)
            .unwrap_or(field.text().clone());

        let mut module_path = Vec::new();
        if base.scope_access_path(&mut module_path) {
            if let Ok(new_scope) = self
                .get_scope_list(*scope, module_path.clone())
                .or_else(|_| self.import_scope_list(*scope, module_path).map(|x| x.0))
            {
                let resolved = self
                    .resolve_potential_dollar_ident(&new_scope, &field)
                    .unwrap_or(field_name.into());
                return Ok(self.evaluate(&new_scope, Node::identifier(span, &resolved.text)));
            }
        }

        Ok(MiddleNode::new(
            MiddleNodeType::ScopeAccess {
                base: Box::new(self.evaluate(scope, base)),
                field: field_name.into(),
            },
            span,
        ))
    }

    pub(crate) fn evaluate_index_access(
        &mut self,
        scope: &u64,
        span: Span,
        base: Node,
        index: Node,
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
            MiddleNodeType::IndexAccess {
                base: Box::new(self.evaluate(scope, base)),
                index: Box::new(self.evaluate(scope, index)),
            },
            span,
        ))
    }

    #[inline]
    pub(crate) fn lower_call_args(
        &mut self,
        scope: &u64,
        args: Vec<CallArg>,
        reverse_args: Vec<Node>,
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
        scope: &u64,
        data_type: &ParserDataType,
        member: &impl ToString,
    ) -> Option<String> {
        let resolved = self.resolve_data_type(scope, data_type.clone());
        self.typing
            .find_impl_member(&resolved, member)
            .map(|x| x.symbol_name.clone())
    }
}
