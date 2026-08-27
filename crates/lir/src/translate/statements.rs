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
        LirMember, LirNode, LirNodeType,
    },
    environment::LirEnvironment,
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
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        let rhs = env.lower_node(*self.value);
        let ident_span = self.identifier.span;

        let (lhs, old_expr) = match self.identifier.node_type {
            MiddleNodeType::Identifier(MirIdentifier { identifier }) => {
                let value = identifier.to_string().into_boxed_str();
                (
                    Some(LirLValue::Var(value.clone())),
                    Some(LirNodeType::Load(LirLoad { value })),
                )
            }
            MiddleNodeType::DerefStatement(MirDeref { value }) => {
                let ptr_expr = env.lower_node(*value);
                let ptr_tmp = env.get_temp();
                env.add_instr(LirNode::new(
                    ident_span,
                    LirNodeType::Declare(LirDeclare {
                        dest: ptr_tmp.clone().into_boxed_str(),
                        data_type: ParserDataType::auto(ident_span),
                        value: Box::new(ptr_expr),
                    }),
                ));
                let ptr_load = LirNodeType::Load(LirLoad {
                    value: ptr_tmp.into_boxed_str(),
                });
                (
                    Some(LirLValue::Ptr(Box::new(ptr_load.clone()))),
                    Some(LirNodeType::Deref(LirDeref {
                        value: Box::new(ptr_load),
                    })),
                )
            }
            MiddleNodeType::FieldAccess(MirField { base, field }) => {
                let base_expr = env.lower_node(*base);
                let base_tmp = env.get_temp();
                env.add_instr(LirNode::new(
                    ident_span,
                    LirNodeType::Declare(LirDeclare {
                        dest: base_tmp.clone().into_boxed_str(),
                        data_type: ParserDataType::auto(ident_span),
                        value: Box::new(base_expr),
                    }),
                ));
                let base_load = LirNodeType::Load(LirLoad {
                    value: base_tmp.into_boxed_str(),
                });

                (
                    Some(LirLValue::Ptr(Box::new(LirNodeType::Member(LirMember {
                        base: Box::new(base_load.clone()),
                        field: field.text.clone().into_boxed_str(),
                    })))),
                    Some(LirNodeType::Member(LirMember {
                        base: Box::new(base_load),
                        field: field.text.into_boxed_str(),
                    })),
                )
            }
            MiddleNodeType::IndexAccess(MirIndex { base, index }) => {
                let base_expr = env.lower_node(*base);
                let base_tmp = env.get_temp();
                env.add_instr(LirNode::new(
                    ident_span,
                    LirNodeType::Declare(LirDeclare {
                        dest: base_tmp.clone().into_boxed_str(),
                        data_type: ParserDataType::auto(ident_span),
                        value: Box::new(base_expr),
                    }),
                ));
                let base_load = LirNodeType::Load(LirLoad {
                    value: base_tmp.into_boxed_str(),
                });

                let index_expr = env.lower_node(*index);
                let index_tmp = env.get_temp();
                env.add_instr(LirNode::new(
                    ident_span,
                    LirNodeType::Declare(LirDeclare {
                        dest: index_tmp.clone().into_boxed_str(),
                        data_type: ParserDataType::auto(ident_span),
                        value: Box::new(index_expr),
                    }),
                ));

                let index_load = LirNodeType::Load(LirLoad {
                    value: index_tmp.into_boxed_str(),
                });

                (
                    Some(LirLValue::Ptr(Box::new(LirNodeType::Index(LirIndex {
                        base: Box::new(base_load.clone()),
                        index: Box::new(index_load.clone()),
                    })))),
                    Some(LirNodeType::Index(LirIndex {
                        base: Box::new(base_load),
                        index: Box::new(index_load),
                    })),
                )
            }
            other => (
                Some(env.lower_lvalue(MiddleNode::new(other, ident_span))),
                Some(LirNodeType::null()),
            ),
        };

        let old_expr = old_expr.unwrap_or_else(LirNodeType::null);
        let temp = env.get_temp();
        env.add_instr(LirNode::new(
            ident_span,
            LirNodeType::Declare(LirDeclare {
                dest: temp.clone().into_boxed_str(),
                data_type: ParserDataType::auto(ident_span),
                value: Box::new(old_expr),
            }),
        ));

        if let Some(lhs) = lhs {
            env.add_instr(LirNode::new(
                ident_span,
                LirNodeType::Assign(LirAssign {
                    dest: lhs,
                    value: Box::new(rhs),
                }),
            ));
        }

        LirNodeType::Load(LirLoad {
            value: temp.into_boxed_str(),
        })
    }
}

impl LirLowering for MirDebug {
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        env.lower_node(*self.value)
    }
}

impl LirLowering for MirAggregate {
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        LirNodeType::Aggregate(LirAggregate {
            name: self.identifier.map(|i| i.to_string()),
            fields: ObjectMap(
                self.value
                    .0
                    .into_iter()
                    .map(|(field_name, field_node)| {
                        (field_name.to_string(), env.lower_node(field_node))
                    })
                    .collect(),
            ),
        })
    }
}

impl LirLowering for MirEnum {
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        LirNodeType::Enum(LirEnum {
            variant: if let Some(obj) = env.env.typing.objects.get(&self.identifier.to_string())
                && let MiddleTypeDefType::Enum { variants, .. } = &obj.object_type
            {
                variants
                    .iter()
                    .position(|(name, _)| name.text == self.value.to_string())
                    .unwrap_or(0) as u32
            } else {
                0
            },
            name: self.identifier.text.into_boxed_str(),
            payload: self.data.map(|d| Box::new(env.lower_node(*d))),
        })
    }
}
