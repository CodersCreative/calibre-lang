use crate::{
    translator::{FunctionTranslator, layout::GetLayoutInfo, memory::MemoryLoc},
    values::{MemberType, RuntimeType, RuntimeValue},
};
use calibre_lir::LirNodeType;
use calibre_mir::ast::{MiddleNode, MiddleNodeType};
use calibre_mir::environment::MiddleTypeDefType;
use calibre_parser::ast::{
    Node, NodeType, ObjectMap, ObjectType, PotentialNewType, TypeDefType,
    comparison::ComparisonOperator,
};
use cranelift::{codegen::ir::BlockArg, prelude::*};

impl<'a> FunctionTranslator<'a> {
    pub fn translate_enum_expression(
        &mut self,
        enum_name: String,
        variant_index: u32,
        data: Option<LirNodeType>,
    ) -> RuntimeValue {
        let type_def = self.objects.get(&enum_name);
        if type_def.is_none() {
            let slot = self.builder.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                1,
                0,
            ));
            let memory = MemoryLoc::from_stack(slot, 0);
            let zero = self.builder.ins().iconst(types::I8, 0);
            memory.write_val(&mut self.builder, zero, 0);
            return RuntimeValue::new(
                memory.into_value(&mut self.builder, self.types.ptr()),
                RuntimeType::Enum {
                    name: enum_name,
                    variant: 0,
                },
            );
        }
        let td = type_def.unwrap();

        let variants = match td.object_type.clone() {
            MiddleTypeDefType::Enum(v) => v,
            _ => {
                let slot = self.builder.create_sized_stack_slot(StackSlotData::new(
                    StackSlotKind::ExplicitSlot,
                    1,
                    0,
                ));
                let memory = MemoryLoc::from_stack(slot, 0);
                let zero = self.builder.ins().iconst(types::I8, 0);
                memory.write_val(&mut self.builder, zero, 0);
                return RuntimeValue::new(
                    memory.into_value(&mut self.builder, self.types.ptr()),
                    RuntimeType::Enum {
                        name: enum_name,
                        variant: 0,
                    },
                );
            }
        };

        let mut max_payload_size: u32 = 0;
        let mut max_align: u32 = 1;

        let mut variant_member_types: Vec<Option<RuntimeType>> = Vec::new();
        let mut variant_sizes: Vec<u32> = Vec::new();

        for (_vname, vdata) in variants.iter() {
            match vdata {
                None => {
                    variant_member_types.push(None);
                    variant_sizes.push(0);
                }
                Some(data_type) => {
                    let data_type: RuntimeType = RuntimeType::from(data_type.clone());
                    let size = data_type.size();
                    max_align = max_align.max(data_type.align());
                    max_payload_size = max_payload_size.max(size);
                    variant_sizes.push(size);
                    variant_member_types.push(Some(data_type));
                }
            }
        }

        let enum_size = max_payload_size + 1;

        let slot = self.builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            enum_size,
            0,
        ));
        let memory = MemoryLoc::from_stack(slot, 0);

        if let Some(provided) = data {
            let value = self.translate(provided);
            let _ = memory.write_all(
                Some(value.value),
                value.data_type.clone(),
                self.module,
                &mut self.builder,
            );
        }

        let discr_offset = max_payload_size;
        let discr_val = self.builder.ins().iconst(types::I8, variant_index as i64);
        memory.write_val(&mut self.builder, discr_val, discr_offset as i32);

        RuntimeValue::new(
            memory.into_value(&mut self.builder, self.types.ptr()),
            RuntimeType::Enum {
                name: enum_name,
                variant: variant_index as usize,
            },
        )
    }

    pub fn get_aggregate_member(&mut self, left: RuntimeValue, index: String) -> RuntimeValue {
        let RuntimeType::Aggregate { members, .. } = left.data_type.clone() else {
            panic!()
        };

        let layouts = left.data_type.aggregate_layout().unwrap();
        let offsets = layouts.offsets();

        let index = match index.parse::<usize>() {
            Ok(x) => x,
            _ => members.iter().position(|x| x.name == index).unwrap(),
        };

        let val = self.builder.ins().load(
            self.types.get_type_from_runtime_type(&members[index].ty),
            MemFlags::new().with_aligned(),
            left.value,
            offsets.get(index).unwrap().clone() as i32,
        );

        RuntimeValue::new(val, members[index].ty.clone())
    }

    pub fn translate_aggregate_expression(
        &mut self,
        name: Option<String>,
        obj: ObjectMap<LirNodeType>,
    ) -> RuntimeValue {
        let mut members: Vec<MemberType> = Vec::new();
        let mut values: Vec<RuntimeValue> = Vec::new();

        for (k, v) in obj.0.into_iter() {
            let val = self.translate(v);
            members.push(MemberType {
                name: k,
                ty: val.data_type.clone(),
            });
            values.push(val);
        }

        let member_types: Vec<RuntimeType> = members.iter().map(|m| m.ty.clone()).collect();
        let total_size: u32 = member_types.iter().map(|x| x.stride()).sum();

        let slot = self.builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            total_size,
            0,
        ));

        let memory = MemoryLoc::from_stack(slot, 0);

        let rt = RuntimeType::Aggregate { name, members };
        let layouts = rt.aggregate_layout().unwrap();
        let offsets = layouts.offsets();

        for (i, val) in values.iter().enumerate() {
            let _ = memory.with_offset(offsets[i]).write_all(
                Some(val.value),
                member_types[i].clone(),
                self.module,
                &mut self.builder,
            );
        }

        RuntimeValue::new(memory.into_value(&mut self.builder, self.types.ptr()), rt)
    }
}
