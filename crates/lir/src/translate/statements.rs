/*
This file handles :
AssignmentExpression,
DebugExpression,
AggregateExpression,
EnumExpression
*/

use crate::{
    ast::{
        LirAggregate, LirAssign, LirDeclare, LirEnum, LirIndex, LirLValue, LirLoad, LirMember,
        LirNode, LirNodeType,
    },
    environment::LirEnvironment,
    translate::LirLowering,
};
use calibre_mir::{
    ast::{
        MiddleNode, MiddleNodeType, MirAggregate, MirAssignment, MirDeref, MirEnum, MirField,
        MirIdentifier, MirIndex,
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

        let lhs = match self.identifier.node_type {
            MiddleNodeType::Identifier(MirIdentifier { identifier }) => {
                Some(LirLValue::Var(identifier))
            }
            MiddleNodeType::DerefStatement(MirDeref { value }) => {
                let ptr_expr = env.lower_node(*value);
                let ptr_tmp = env.get_temp();
                env.add_instr(LirNode::new(
                    ident_span,
                    LirNodeType::Declare(LirDeclare {
                        dest: ptr_tmp,
                        data_type: ParserDataType::auto(ident_span),
                        value: Box::new(ptr_expr),
                        is_referenced: false,
                    }),
                ));
                let ptr_load = LirNodeType::Load(LirLoad { value: ptr_tmp });

                Some(LirLValue::Ptr(Box::new(ptr_load.clone())))
            }
            MiddleNodeType::FieldAccess(MirField { base, field }) => {
                let base_expr = env.lower_node(*base);
                let base_tmp = env.get_temp();
                env.add_instr(LirNode::new(
                    ident_span,
                    LirNodeType::Declare(LirDeclare {
                        dest: base_tmp,
                        data_type: ParserDataType::auto(ident_span),
                        value: Box::new(base_expr),
                        is_referenced: false,
                    }),
                ));
                let base_load = LirNodeType::Load(LirLoad { value: base_tmp });

                Some(LirLValue::Ptr(Box::new(LirNodeType::Member(LirMember {
                    base: Box::new(base_load.clone()),
                    field,
                }))))
            }
            MiddleNodeType::IndexAccess(MirIndex { base, index }) => {
                let base_load = if let MiddleNodeType::Identifier(MirIdentifier { identifier }) =
                    &base.node_type
                {
                    LirNodeType::Load(LirLoad { value: *identifier })
                } else {
                    let base_expr = env.lower_node(*base);
                    let base_tmp = env.get_temp();
                    env.add_instr(LirNode::new(
                        ident_span,
                        LirNodeType::Declare(LirDeclare {
                            dest: base_tmp,
                            data_type: ParserDataType::auto(ident_span),
                            value: Box::new(base_expr),
                            is_referenced: false,
                        }),
                    ));
                    LirNodeType::Load(LirLoad { value: base_tmp })
                };

                let index = env.lower_node(*index);

                Some(LirLValue::Ptr(Box::new(LirNodeType::Index(LirIndex {
                    base: Box::new(base_load.clone()),
                    index: Box::new(index.clone()),
                }))))
            }
            other => Some(env.lower_lvalue(MiddleNode::new(other, ident_span))),
        };

        if let Some(lhs) = lhs {
            LirNodeType::Assign(LirAssign {
                dest: lhs,
                value: Box::new(rhs),
            })
        } else {
            LirNodeType::null()
        }
    }
}

impl LirLowering for MirAggregate {
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        LirNodeType::Aggregate(LirAggregate {
            name: self.identifier,
            fields: ObjectMap(
                self.value
                    .0
                    .into_iter()
                    .map(|(field_name, field_node)| (field_name, env.lower_node(field_node)))
                    .collect(),
            ),
        })
    }
}

impl LirLowering for MirEnum {
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        LirNodeType::Enum(LirEnum {
            variant: if let Some(obj) = env.env.typing.objects.get(&self.identifier)
                && let MiddleTypeDefType::Enum { variants, .. } = &obj.object_type
            {
                variants
                    .iter()
                    .position(|(name, _)| name == &self.value)
                    .unwrap_or(0) as u32
            } else {
                0
            },
            name: self.identifier,
            payload: self.data.map(|d| Box::new(env.lower_node(*d))),
        })
    }
}
