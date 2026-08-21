use std::str::FromStr;

use crate::{
    ast::{MiddleNode, MiddleNodeType},
    environment::MiddleEnvironment,
    errors::MiddleErr,
};
use calibre_parser::{
    Span,
    ast::{
        Operator,
        idents::{PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::{CallArg, Node, NodeType},
        types::{ParserDataType, ParserInnerType},
    },
};

impl MiddleEnvironment {
    pub(crate) fn scope_member_call(span: Span, path: &[&str], args: Vec<CallArg>) -> Node {
        let mut result = Node::identifier(span, path.first().unwrap());
        for segment in path.iter().skip(1) {
            result = Node::new(
                span,
                NodeType::FieldAccess {
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

        if let Some(ty) = self.resolve_type_from_node(scope, &base)
            && let Some(x) = self.resolve_impl_member(scope, &ty, &field_name)
        {
            return Ok(MiddleNode::identifier(span, x));
        }

        if let NodeType::Identifier(generic_ident) = &base.node_type
            && let Some(ident) = self.resolve_dollar_ident_only(scope, generic_ident.get_ident())
        {
            let ty = self.resolve_str(scope, &ident.text).unwrap_or(ident.text);
            let mut ty = ParserInnerType::from_str(&ty).unwrap();

            if let PotentialGenericTypeIdentifier::Generic {
                identifier: _,
                generic_types,
            } = generic_ident
            {
                let mut generic_types = generic_types
                    .clone()
                    .into_iter()
                    .map(|x| self.resolve_data_type(scope, x))
                    .collect::<Vec<_>>();
                ty = match ty {
                    ParserInnerType::List(_) if !generic_types.is_empty() => {
                        ParserInnerType::List(Box::new(generic_types.pop().unwrap()))
                    }
                    ParserInnerType::Ptr(_) if !generic_types.is_empty() => {
                        ParserInnerType::Ptr(Box::new(generic_types.pop().unwrap()))
                    }
                    ParserInnerType::StructWithGenerics {
                        identifier,
                        generic_types: _,
                    }
                    | ParserInnerType::Struct(identifier)
                        if !generic_types.is_empty() =>
                    {
                        ParserInnerType::StructWithGenerics {
                            identifier,
                            generic_types,
                        }
                    }
                    x => x,
                }
            }

            if let Some(x) = self.resolve_impl_member(scope, &ParserDataType::from(ty), &field_name)
            {
                return Ok(MiddleNode::identifier(span, x));
            }
        }

        Ok(MiddleNode::new(
            MiddleNodeType::FieldAccess {
                base: Box::new(self.evaluate(scope, base)),
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

        if let NodeType::Identifier(module_name) = &base.node_type {
            let module_path = vec![module_name.get_ident().text().clone(), field_name.clone()];

            if let Ok(new_scope) = self.get_scope_list(*scope, module_path) {
                let resolved = self
                    .resolve_potential_dollar_ident(&new_scope, &field)
                    .unwrap_or(field_name.into());
                return Ok(MiddleNode::new(MiddleNodeType::Identifier(resolved), span));
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
