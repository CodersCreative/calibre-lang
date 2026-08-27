/*
This file handles :
AssignmentExpression,
DebugExpression,
AggregateExpression,
EnumExpression
*/

use crate::{
    ast::{
        LirAggregate, LirAssign, LirDeclare, LirDeref, LirEnum, LirIndex, LirLValue, LirLoad,
        LirMember, LirNodeType,
    },
    environment::{LirEnvironment, LirId},
    translate::LirLowering,
};
use calibre_mir::{
    ast::{
        MiddleNode, MiddleNodeType, MirAggregate, MirAssignment, MirDebug, MirDeref, MirEnum,
        MirField, MirIdentifier, MirIndex,
    },
    typing::MiddleTypeDefType,
};
use calibre_parser::{
    Span,
    ast::{ObjectMap, types::ParserDataType},
};

impl LirLowering for MirAssignment {
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirId {
        let rhs: LirId = env.lower_node(*self.value);
        let ident_span = self.identifier.span;

        let (lhs, old_expr) = match self.identifier.node_type {
            MiddleNodeType::Identifier(MirIdentifier { identifier }) => {
                let value = identifier.to_string().into_boxed_str();
                (
                    Some(LirLValue::Var(value.clone())),
                    Some(env.add(LirNodeType::Load(LirLoad { value }), ident_span)),
                )
            }
            MiddleNodeType::DerefStatement(MirDeref { value }) => {
                let value = env.lower_node(*value);
                let dest = env.get_temp().into_boxed_str();

                let node = env.add_with_children(
                    LirNodeType::Declare(LirDeclare {
                        dest: dest.clone(),
                        data_type: ParserDataType::auto(ident_span),
                        value,
                    }),
                    std::iter::once(value),
                    ident_span,
                );
                env.add_instr(node);

                let ptr_load = env.add(LirNodeType::Load(LirLoad { value: dest }), ident_span);

                (
                    Some(LirLValue::Ptr(ptr_load)),
                    Some(env.add(LirNodeType::Deref(LirDeref { value: ptr_load }), ident_span)),
                )
            }
            MiddleNodeType::FieldAccess(MirField { base, field }) => {
                let base = env.lower_node(*base);
                let dest = env.get_temp();

                let base = env.add_with_children(
                    LirNodeType::Declare(LirDeclare {
                        dest: dest.clone().into_boxed_str(),
                        data_type: ParserDataType::auto(ident_span),
                        value: base,
                    }),
                    std::iter::once(base),
                    ident_span,
                );

                env.add_instr(base);
                let base_load = env.add(
                    LirNodeType::Load(LirLoad {
                        value: dest.into_boxed_str(),
                    }),
                    ident_span,
                );

                (
                    Some(LirLValue::Ptr(env.add(
                        LirNodeType::Member(LirMember {
                            base: base_load,
                            field: field.text.clone().into_boxed_str(),
                        }),
                        ident_span,
                    ))),
                    Some(env.add(
                        LirNodeType::Member(LirMember {
                            base: base_load,
                            field: field.text.into_boxed_str(),
                        }),
                        ident_span,
                    )),
                )
            }
            MiddleNodeType::IndexAccess(MirIndex { base, index }) => {
                let base = env.lower_node(*base);
                let dest = env.get_temp().into_boxed_str();

                let base = env.add_with_children(
                    LirNodeType::Declare(LirDeclare {
                        dest: dest.clone(),
                        data_type: ParserDataType::auto(ident_span),
                        value: base,
                    }),
                    std::iter::once(base),
                    ident_span,
                );

                env.add_instr(base);
                let base_load = env.add(LirNodeType::Load(LirLoad { value: dest }), ident_span);

                let index = env.lower_node(*index);
                let dest = env.get_temp();
                let index = env.add_with_children(
                    LirNodeType::Declare(LirDeclare {
                        dest: dest.clone().into_boxed_str(),
                        data_type: ParserDataType::auto(ident_span),
                        value: index,
                    }),
                    std::iter::once(index),
                    ident_span,
                );

                env.add_instr(index);
                let index_load = env.add(
                    LirNodeType::Load(LirLoad {
                        value: dest.into_boxed_str(),
                    }),
                    ident_span,
                );

                (
                    Some(LirLValue::Ptr(env.add(
                        LirNodeType::Index(LirIndex {
                            base: base_load,
                            index: index_load,
                        }),
                        ident_span,
                    ))),
                    Some(env.add(
                        LirNodeType::Index(LirIndex {
                            base: base_load,
                            index: index_load,
                        }),
                        ident_span,
                    )),
                )
            }
            other => (
                Some(env.lower_lvalue(MiddleNode::new(other, ident_span))),
                Some(env.null()),
            ),
        };

        let old_expr = old_expr.unwrap_or_else(|| env.null());
        let temp = env.get_temp();
        let decl = env.add_with_children(
            LirNodeType::Declare(LirDeclare {
                dest: temp.clone().into_boxed_str(),
                data_type: ParserDataType::auto(ident_span),
                value: old_expr,
            }),
            std::iter::once(old_expr),
            ident_span,
        );

        env.add_instr(decl);
        if let Some(lhs) = lhs {
            let assign_id = env.add_with_children(
                LirNodeType::Assign(LirAssign {
                    dest: lhs,
                    value: rhs,
                }),
                std::iter::once(rhs),
                ident_span,
            );
            env.add_instr(assign_id);
        }

        env.add(
            LirNodeType::Load(LirLoad {
                value: temp.into_boxed_str(),
            }),
            span,
        )
    }
}

impl LirLowering for MirDebug {
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirId {
        env.lower_node(*self.value)
    }
}

impl LirLowering for MirAggregate {
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirId {
        let fields: Vec<(String, LirId)> = self
            .value
            .0
            .into_iter()
            .map(|(field_name, field_node)| (field_name.to_string(), env.lower_node(field_node)))
            .collect();

        let children: Vec<LirId> = fields.iter().map(|(_, id)| *id).collect();

        env.add_with_children(
            LirNodeType::Aggregate(LirAggregate {
                name: self.identifier.map(|i| i.to_string()),
                fields: ObjectMap(fields.into_iter().collect()),
            }),
            children.into_iter(),
            span,
        )
    }
}

impl LirLowering for MirEnum {
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirId {
        let variant = if let Some(obj) = env.env.typing.objects.get(&self.identifier.to_string())
            && let MiddleTypeDefType::Enum { variants, .. } = &obj.object_type
        {
            variants
                .iter()
                .position(|(name, _)| name.text == self.value.to_string())
                .unwrap_or(0) as u32
        } else {
            0
        };

        if let Some(payload) = self.data.map(|d| env.lower_node(*d)) {
            env.add_with_children(
                LirNodeType::Enum(LirEnum {
                    variant,
                    name: self.identifier.text.into_boxed_str(),
                    payload: Some(payload),
                }),
                std::iter::once(payload),
                span,
            )
        } else {
            env.add(
                LirNodeType::Enum(LirEnum {
                    variant,
                    name: self.identifier.text.into_boxed_str(),
                    payload: None,
                }),
                span,
            )
        }
    }
}
