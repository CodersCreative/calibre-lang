use crate::{
    ast::{MiddleNode, MiddleNodeType, MirCall},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    symbols::MiddleOverload,
};
use calibre_parser::{
    Span,
    ast::{
        Operator,
        nodes::AstNode,
        types::{ParserDataType, ParserInnerType},
    },
};

impl MiddleEnvironment {
    #[inline]
    pub fn resolve_operator_or_bool(
        &mut self,
        scope: ScopeId,
        left: &AstNode,
        right: &AstNode,
        operator: Operator,
        span: Span,
    ) -> Option<ParserDataType> {
        self.get_operator_overload(scope, left, right, &operator)
            .map(|x| x.return_type.clone())
            .or_else(|| Some(ParserDataType::new(span, ParserInnerType::Bool)))
    }

    pub fn handle_operator_overloads(
        &mut self,
        scope: ScopeId,
        span: Span,
        left: AstNode,
        right: AstNode,
        operator: Operator,
    ) -> Result<Option<MiddleNode>, MiddleErr> {
        if matches!(operator, Operator::As) {
            return Ok(None);
        }
        if let (Some(left_ty), Some(right_ty)) = (
            self.resolve_type_from_node(scope, &left),
            self.resolve_type_from_node(scope, &right),
        ) {
            let matches_overload = |overload: &MiddleOverload| {
                overload.parameters.len() == 2
                    && overload.operator == operator
                    && overload.parameters[0].data_type.matches(
                        &left_ty.data_type,
                        &overload
                            .generic_params
                            .iter()
                            .map(|x| x.as_str())
                            .collect::<Vec<_>>(),
                    )
                    && overload.parameters[1].data_type.matches(
                        &right_ty.data_type,
                        &overload
                            .generic_params
                            .iter()
                            .map(|x| x.as_str())
                            .collect::<Vec<_>>(),
                    )
            };
            if let Some(overload) = self
                .symbols
                .overloads
                .iter()
                .find(|x| matches_overload(x))
                .cloned()
            {
                return Ok(Some(MiddleNode {
                    node_type: MiddleNodeType::CallExpression(MirCall {
                        caller: Box::new(self.evaluate_inner(scope, overload.func.clone())?),
                        args: vec![
                            self.evaluate_inner(scope, left)?,
                            self.evaluate_inner(scope, right)?,
                        ],
                    }),
                    span,
                }));
            }
        }

        Ok(None)
    }

    pub fn handle_as_overload(
        &mut self,
        scope: ScopeId,
        span: Span,
        value: AstNode,
        target: ParserDataType,
    ) -> Result<Option<MiddleNode>, MiddleErr> {
        let Some(left_ty) = self.resolve_type_from_node(scope, &value) else {
            return Ok(None);
        };
        let overload = self
            .symbols
            .overloads
            .iter()
            .filter(|x| matches!(x.operator, Operator::As))
            .filter(|x| x.parameters.len() == 1)
            .find(|x| {
                if x.parameters[0].data_type.matches(
                    &left_ty.data_type,
                    &x.generic_params
                        .iter()
                        .map(|x| x.as_str())
                        .collect::<Vec<_>>(),
                ) && let Some(t) = x.return_type.data_type.unwrap_one_result()
                    && t.matches(
                        &target.data_type,
                        &x.generic_params
                            .iter()
                            .map(|x| x.as_str())
                            .collect::<Vec<_>>(),
                    )
                {
                    true
                } else {
                    false
                }
            })
            .cloned();

        if let Some(overload) = overload {
            return Ok(Some(MiddleNode {
                node_type: MiddleNodeType::CallExpression(MirCall {
                    caller: Box::new(self.evaluate_inner(scope, overload.func.clone())?),
                    args: vec![self.evaluate_inner(scope, value)?],
                }),
                span,
            }));
        }

        Ok(None)
    }

    pub fn handle_as_overload_exists(
        &mut self,
        scope: ScopeId,
        value: AstNode,
        target: ParserDataType,
    ) -> Result<bool, MiddleErr> {
        let Some(left_ty) = self.resolve_type_from_node(scope, &value) else {
            return Ok(false);
        };
        let overload = self
            .symbols
            .overloads
            .iter()
            .filter(|x| matches!(x.operator, Operator::As))
            .filter(|x| x.parameters.len() == 1)
            .find(|x| {
                if x.parameters[0].data_type.matches(
                    &left_ty.data_type,
                    &x.generic_params
                        .iter()
                        .map(|x| x.as_str())
                        .collect::<Vec<_>>(),
                ) && let Some(t) = x.return_type.data_type.unwrap_one_result()
                    && t.matches(
                        &target.data_type,
                        &x.generic_params
                            .iter()
                            .map(|x| x.as_str())
                            .collect::<Vec<_>>(),
                    )
                {
                    true
                } else {
                    false
                }
            });

        Ok(overload.is_some())
    }

    pub fn handle_index_assign_overload(
        &mut self,
        scope: ScopeId,
        span: Span,
        base: AstNode,
        index: AstNode,
        value: AstNode,
    ) -> Result<Option<MiddleNode>, MiddleErr> {
        let (Some(base_ty), Some(index_ty), Some(value_ty)) = (
            self.resolve_type_from_node(scope, &base),
            self.resolve_type_from_node(scope, &index),
            self.resolve_type_from_node(scope, &value),
        ) else {
            return Ok(None);
        };

        let overload = self
            .symbols
            .overloads
            .iter()
            .filter(|x| matches!(x.operator, Operator::IndexAssign))
            .filter(|x| x.parameters.len() == 3)
            .find(|x| {
                x.parameters[0].data_type.matches(
                    &base_ty.data_type,
                    &x.generic_params
                        .iter()
                        .map(|x| x.as_str())
                        .collect::<Vec<_>>(),
                ) && x.parameters[1].data_type.matches(
                    &index_ty.data_type,
                    &x.generic_params
                        .iter()
                        .map(|x| x.as_str())
                        .collect::<Vec<_>>(),
                ) && x.parameters[2].data_type.matches(
                    &value_ty.data_type,
                    &x.generic_params
                        .iter()
                        .map(|x| x.as_str())
                        .collect::<Vec<_>>(),
                )
            })
            .cloned();

        if let Some(overload) = overload {
            return Ok(Some(MiddleNode {
                node_type: MiddleNodeType::CallExpression(MirCall {
                    caller: Box::new(self.evaluate_inner(scope, overload.func.clone())?),
                    args: vec![
                        self.evaluate_inner(scope, base)?,
                        self.evaluate_inner(scope, index)?,
                        self.evaluate_inner(scope, value)?,
                    ],
                }),
                span,
            }));
        }

        Ok(None)
    }

    pub fn get_operator_overload(
        &mut self,
        scope: ScopeId,
        left: &AstNode,
        right: &AstNode,
        operator: &Operator,
    ) -> Option<&MiddleOverload> {
        if let (Some(left_ty), Some(right_ty)) = (
            self.resolve_type_from_node(scope, left),
            self.resolve_type_from_node(scope, right),
        ) && let Some(overload) = self
            .symbols
            .overloads
            .iter()
            .filter(|x| x.parameters.len() == 2 && &x.operator == operator)
            .find(|x| {
                x.parameters[0].data_type.matches(
                    &left_ty.data_type,
                    &x.generic_params
                        .iter()
                        .map(|x| x.as_str())
                        .collect::<Vec<_>>(),
                ) && x.parameters[1].data_type.matches(
                    &right_ty.data_type,
                    &x.generic_params
                        .iter()
                        .map(|x| x.as_str())
                        .collect::<Vec<_>>(),
                )
            })
        {
            return Some(overload);
        }

        None
    }
}
