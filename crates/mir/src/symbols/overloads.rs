use crate::{
    ast::{MiddleNode, MiddleNodeType, MirCall, types::MirDataType},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    symbols::MiddleOverload,
    translate::MirLowering,
};
use calibre_parser::{
    Span,
    ast::{Operator, nodes::AstNode},
};
use tracing::instrument;

impl MiddleEnvironment {
    #[inline]
    pub fn resolve_operator_or_bool(
        &mut self,
        scope: ScopeId,
        left: &AstNode,
        right: &AstNode,
        data_type: Option<&MirDataType>,
        operator: Operator,
    ) -> Option<MirDataType> {
        self.get_operator_overload(scope, left, right, data_type, &operator)
            .map(|x| x.return_type.clone())
            .or(Some(MirDataType::Bool))
    }

    #[instrument(skip_all)]
    pub fn handle_operator_overloads(
        &mut self,
        scope: ScopeId,
        span: Span,
        left: AstNode,
        right: AstNode,
        data_type: Option<&MirDataType>,
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
                    && overload.parameters[0].matches(
                        &left_ty,
                        &overload
                            .generic_params
                            .iter()
                            .map(|x| x.as_str())
                            .collect::<Vec<_>>(),
                    )
                    && overload.parameters[1].matches(
                        &right_ty,
                        &overload
                            .generic_params
                            .iter()
                            .map(|x| x.as_str())
                            .collect::<Vec<_>>(),
                    )
                    && data_type.is_none_or(|x| {
                        overload.return_type.matches(
                            x,
                            &overload
                                .generic_params
                                .iter()
                                .map(|x| x.as_str())
                                .collect::<Vec<_>>(),
                        )
                    })
            };

            if let Some(overload) = self
                .symbols
                .overloads
                .iter()
                .find(|x| matches_overload(x))
                .cloned()
            {
                if overload.generic_params.is_empty() {
                    return Ok(Some(MiddleNode {
                        node_type: MiddleNodeType::CallExpression(MirCall {
                            caller: Box::new(MiddleNode::identifier(span, overload.func.clone())),
                            args: Box::new([
                                left.lower(
                                    self,
                                    scope,
                                    span,
                                    overload.parameters.first().cloned(),
                                )?,
                                right.lower(
                                    self,
                                    scope,
                                    span,
                                    overload.parameters.last().cloned(),
                                )?,
                            ]),
                        }),
                        span,
                    }));
                }

                let arg_types = vec![left_ty.clone(), right_ty.clone()];
                let key = self.monomorphize_overload(scope, &overload, arg_types, data_type)?;

                return Ok(Some(MiddleNode {
                    node_type: MiddleNodeType::CallExpression(MirCall {
                        caller: Box::new(MiddleNode::identifier(span, key)),
                        args: Box::new([
                            left.lower(self, scope, span, overload.parameters.first().cloned())?,
                            right.lower(self, scope, span, overload.parameters.last().cloned())?,
                        ]),
                    }),
                    span,
                }));
            }
        }

        Ok(None)
    }

    #[instrument(skip_all)]
    pub fn handle_as_overload(
        &mut self,
        scope: ScopeId,
        span: Span,
        value: AstNode,
        target: &MirDataType,
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
                if x.parameters[0].matches(
                    &left_ty,
                    &x.generic_params
                        .iter()
                        .map(|x| x.as_str())
                        .collect::<Vec<_>>(),
                ) && let Some(t) = x.return_type.unwrap_one_result()
                    && t.matches(
                        target,
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
            if overload.generic_params.is_empty() {
                return Ok(Some(MiddleNode {
                    node_type: MiddleNodeType::CallExpression(MirCall {
                        caller: Box::new(MiddleNode::identifier(span, overload.func.clone())),
                        args: Box::new([value.lower(
                            self,
                            scope,
                            span,
                            overload.parameters.first().cloned(),
                        )?]),
                    }),
                    span,
                }));
            }

            let arg_types = vec![left_ty.clone()];
            let key = self.monomorphize_overload(scope, &overload, arg_types, Some(target))?;

            return Ok(Some(MiddleNode {
                node_type: MiddleNodeType::CallExpression(MirCall {
                    caller: Box::new(MiddleNode::identifier(span, key)),
                    args: Box::new([value.lower(
                        self,
                        scope,
                        span,
                        overload.parameters.first().cloned(),
                    )?]),
                }),
                span,
            }));
        }

        Ok(None)
    }

    #[instrument(skip_all)]
    pub fn handle_as_overload_exists(
        &mut self,
        scope: ScopeId,
        value: AstNode,
        target: &MirDataType,
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
                if x.parameters[0].matches(
                    &left_ty,
                    &x.generic_params
                        .iter()
                        .map(|x| x.as_str())
                        .collect::<Vec<_>>(),
                ) && let Some(t) = x.return_type.unwrap_one_result()
                    && t.matches(
                        target,
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

    #[instrument(skip_all)]
    pub fn handle_index_assign_overload(
        &mut self,
        scope: ScopeId,
        span: Span,
        base: AstNode,
        index: AstNode,
        value: AstNode,
        data_type: Option<&MirDataType>,
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
            .find(|overload| {
                overload.parameters[0].matches(
                    &base_ty,
                    &overload
                        .generic_params
                        .iter()
                        .map(|x| x.as_str())
                        .collect::<Vec<_>>(),
                ) && overload.parameters[1].matches(
                    &index_ty,
                    &overload
                        .generic_params
                        .iter()
                        .map(|x| x.as_str())
                        .collect::<Vec<_>>(),
                ) && overload.parameters[2].matches(
                    &value_ty,
                    &overload
                        .generic_params
                        .iter()
                        .map(|x| x.as_str())
                        .collect::<Vec<_>>(),
                ) && data_type.is_none_or(|x| {
                    overload.return_type.matches(
                        x,
                        &overload
                            .generic_params
                            .iter()
                            .map(|x| x.as_str())
                            .collect::<Vec<_>>(),
                    )
                })
            })
            .cloned();

        if let Some(overload) = overload {
            if overload.generic_params.is_empty() {
                return Ok(Some(MiddleNode {
                    node_type: MiddleNodeType::CallExpression(MirCall {
                        caller: Box::new(MiddleNode::identifier(span, overload.func.clone())),
                        args: Box::new([
                            base.lower(self, scope, span, overload.parameters.first().cloned())?,
                            index.lower(self, scope, span, overload.parameters.get(1).cloned())?,
                            value.lower(self, scope, span, overload.parameters.last().cloned())?,
                        ]),
                    }),
                    span,
                }));
            }

            let arg_types = vec![base_ty.clone(), index_ty.clone(), value_ty.clone()];
            let key = self.monomorphize_overload(scope, &overload, arg_types, data_type)?;

            return Ok(Some(MiddleNode {
                node_type: MiddleNodeType::CallExpression(MirCall {
                    caller: Box::new(MiddleNode::identifier(span, key)),
                    args: Box::new([
                        base.lower(self, scope, span, overload.parameters.first().cloned())?,
                        index.lower(self, scope, span, overload.parameters.get(1).cloned())?,
                        value.lower(self, scope, span, overload.parameters.last().cloned())?,
                    ]),
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
        data_type: Option<&MirDataType>,
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
            .find(|overload| {
                overload.parameters[0].matches(
                    &left_ty,
                    &overload
                        .generic_params
                        .iter()
                        .map(|x| x.as_str())
                        .collect::<Vec<_>>(),
                ) && overload.parameters[1].matches(
                    &right_ty,
                    &overload
                        .generic_params
                        .iter()
                        .map(|x| x.as_str())
                        .collect::<Vec<_>>(),
                ) && data_type.is_none_or(|x| {
                    overload.return_type.matches(
                        x,
                        &overload
                            .generic_params
                            .iter()
                            .map(|x| x.as_str())
                            .collect::<Vec<_>>(),
                    )
                })
            })
        {
            return Some(overload);
        }

        None
    }
}
