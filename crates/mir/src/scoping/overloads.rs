use calibre_parser::{
    Span,
    ast::{Node, ParserDataType},
};

use crate::{
    ast::{MiddleNode, MiddleNodeType},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    symbols::{MiddleOverload, Operator},
};

impl MiddleEnvironment {
    pub fn handle_operator_overloads(
        &mut self,
        scope: &u64,
        span: Span,
        left: Node,
        right: Node,
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
                    && self.impl_type_matches(
                        &overload.parameters[0].data_type,
                        &left_ty.data_type,
                        &overload.generic_params,
                    )
                    && self.impl_type_matches(
                        &overload.parameters[1].data_type,
                        &right_ty.data_type,
                        &overload.generic_params,
                    )
            };
            if let Some(overload) = self.overloads.iter().find(|x| matches_overload(x)).cloned() {
                return Ok(Some(MiddleNode {
                    node_type: MiddleNodeType::CallExpression {
                        caller: Box::new(self.evaluate_inner(scope, overload.func.clone())?),
                        args: vec![
                            self.evaluate_inner(scope, left)?,
                            self.evaluate_inner(scope, right)?,
                        ],
                    },
                    span,
                }));
            }
        }

        Ok(None)
    }

    pub fn handle_as_overload(
        &mut self,
        scope: &u64,
        span: Span,
        value: Node,
        target: ParserDataType,
    ) -> Result<Option<MiddleNode>, MiddleErr> {
        let Some(left_ty) = self.resolve_type_from_node(scope, &value) else {
            return Ok(None);
        };
        let overload = self
            .overloads
            .iter()
            .filter(|x| matches!(x.operator, Operator::As))
            .filter(|x| x.parameters.len() == 1)
            .find(|x| {
                if self.impl_type_matches(
                    &x.parameters[0].data_type,
                    &left_ty.data_type,
                    &x.generic_params,
                ) && let Some(t) = x.return_type.data_type.unwrap_one_result()
                    && self.impl_type_matches(t, &target.data_type, &x.generic_params)
                {
                    true
                } else {
                    false
                }
            })
            .cloned();

        if let Some(overload) = overload {
            return Ok(Some(MiddleNode {
                node_type: MiddleNodeType::CallExpression {
                    caller: Box::new(self.evaluate_inner(scope, overload.func.clone())?),
                    args: vec![self.evaluate_inner(scope, value)?],
                },
                span,
            }));
        }

        Ok(None)
    }

    pub fn handle_as_overload_exists(
        &mut self,
        scope: &u64,
        value: Node,
        target: ParserDataType,
    ) -> Result<bool, MiddleErr> {
        let Some(left_ty) = self.resolve_type_from_node(scope, &value) else {
            return Ok(false);
        };
        let overload = self
            .overloads
            .iter()
            .filter(|x| matches!(x.operator, Operator::As))
            .filter(|x| x.parameters.len() == 1)
            .find(|x| {
                if self.impl_type_matches(
                    &x.parameters[0].data_type,
                    &left_ty.data_type,
                    &x.generic_params,
                ) && let Some(t) = x.return_type.data_type.unwrap_one_result()
                    && self.impl_type_matches(t, &target.data_type, &x.generic_params)
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
        scope: &u64,
        span: Span,
        base: Node,
        index: Node,
        value: Node,
    ) -> Result<Option<MiddleNode>, MiddleErr> {
        let (Some(base_ty), Some(index_ty), Some(value_ty)) = (
            self.resolve_type_from_node(scope, &base),
            self.resolve_type_from_node(scope, &index),
            self.resolve_type_from_node(scope, &value),
        ) else {
            return Ok(None);
        };

        let overload = self
            .overloads
            .iter()
            .filter(|x| matches!(x.operator, Operator::IndexAssign))
            .filter(|x| x.parameters.len() == 3)
            .find(|x| {
                self.impl_type_matches(
                    &x.parameters[0].data_type,
                    &base_ty.data_type,
                    &x.generic_params,
                ) && self.impl_type_matches(
                    &x.parameters[1].data_type,
                    &index_ty.data_type,
                    &x.generic_params,
                ) && self.impl_type_matches(
                    &x.parameters[2].data_type,
                    &value_ty.data_type,
                    &x.generic_params,
                )
            })
            .cloned();

        if let Some(overload) = overload {
            return Ok(Some(MiddleNode {
                node_type: MiddleNodeType::CallExpression {
                    caller: Box::new(self.evaluate_inner(scope, overload.func.clone())?),
                    args: vec![
                        self.evaluate_inner(scope, base)?,
                        self.evaluate_inner(scope, index)?,
                        self.evaluate_inner(scope, value)?,
                    ],
                },
                span,
            }));
        }

        Ok(None)
    }

    pub fn get_operator_overload(
        &mut self,
        scope: &u64,
        left: &Node,
        right: &Node,
        operator: &Operator,
    ) -> Option<&MiddleOverload> {
        if let (Some(left_ty), Some(right_ty)) = (
            self.resolve_type_from_node(scope, left),
            self.resolve_type_from_node(scope, right),
        ) {
            if let Some(overload) = self
                .overloads
                .iter()
                .filter(|x| x.parameters.len() == 2 && &x.operator == operator)
                .find(|x| {
                    self.impl_type_matches(
                        &x.parameters[0].data_type,
                        &left_ty.data_type,
                        &x.generic_params,
                    ) && self.impl_type_matches(
                        &x.parameters[1].data_type,
                        &right_ty.data_type,
                        &x.generic_params,
                    )
                })
            {
                return Some(overload);
            }
        }

        None
    }
}
