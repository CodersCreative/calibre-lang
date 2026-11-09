/*
This file handles :
AssignmentExpression,
DebugExpression,
AggregateExpression,
EnumExpression
*/

use crate::{
    ast::{LirAggregate, LirAssign, LirClone, LirEnum, LirNodeType},
    environment::LirEnvironment,
    translate::LirLowering,
};
use calibre_mir::{
    ast::{MirAggregate, MirAssignment, MirEnum},
    typing::MiddleTypeDefType,
};
use calibre_parser::{Span, ast::ObjectMap};

impl LirLowering for MirAssignment {
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        let mut rhs = env.lower_node(*self.value);

        if !self.data_type.is_ref() {
            rhs = LirNodeType::Clone(LirClone {
                value: Box::new(rhs),
            });
        }

        let lhs = env
            .lower_lvalue(*self.identifier)
            .expect("Unable to get L-Value");

        LirNodeType::Assign(LirAssign {
            dest: lhs,
            value: Box::new(rhs),
        })
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
            variant: if let Some(ident) = &self.identifier
                && let Some(obj) = env.env.typing.objects.get(ident)
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
