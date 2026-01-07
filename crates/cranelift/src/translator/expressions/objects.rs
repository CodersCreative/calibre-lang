use crate::{
    translator::{FunctionTranslator, layout::GetLayoutInfo, memory::MemoryLoc},
    values::{MemberType, RuntimeType, RuntimeValue},
};
use calibre_mir::ast::{MiddleNode, MiddleNodeType};
use calibre_parser::ast::{Node, NodeType, ObjectType, TypeDefType, comparison::Comparison};
use cranelift::{codegen::ir::BlockArg, prelude::*};

impl<'a> FunctionTranslator<'a> {
    pub fn translate_enum_expression(
        &mut self,
        enum_name: String,
        value: String,
        data: Option<ObjectType<Option<MiddleNode>>>,
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
                RuntimeType::Enum { name: enum_name },
            );
        }
        let td = type_def.unwrap();

        let variants = match td.object_type.clone() {
            TypeDefType::Enum(v) => v,
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
                    RuntimeType::Enum { name: enum_name },
                );
            }
        };

        let mut max_payload_size: u32 = 0;
        let mut max_align: u32 = 1;

        let mut variant_member_types: Vec<Option<Vec<MemberType>>> = Vec::new();
        let mut variant_offsets: Vec<Vec<u32>> = Vec::new();
        let mut variant_sizes: Vec<u32> = Vec::new();

        for (_vname, vdata) in variants.iter() {
            match vdata {
                None => {
                    variant_member_types.push(None);
                    variant_offsets.push(vec![]);
                    variant_sizes.push(0);
                }
                Some(ObjectType::Tuple(pars)) => {
                    let members_rt: Vec<MemberType> = pars
                        .iter()
                        .map(|p| MemberType {
                            name: String::new(),
                            ty: RuntimeType::from(p.clone()),
                        })
                        .collect();

                    let rt = RuntimeType::Struct {
                        name: None,
                        members: members_rt.clone(),
                    };
                    let struct_layout = rt.struct_layout().unwrap();
                    let size = rt.size();
                    let align = rt.align();
                    max_payload_size = max_payload_size.max(size);
                    max_align = max_align.max(align);
                    variant_member_types.push(Some(members_rt));
                    variant_offsets.push(struct_layout.offsets().to_vec());
                    variant_sizes.push(size);
                }
                Some(ObjectType::Map(map)) => {
                    let mut members: Vec<MemberType> = Vec::new();
                    for (k, v) in map.iter() {
                        members.push(MemberType {
                            name: k.clone(),
                            ty: RuntimeType::from(v.clone()),
                        });
                    }
                    let rt = RuntimeType::Struct {
                        name: None,
                        members: members.clone(),
                    };
                    let struct_layout = rt.struct_layout().unwrap();
                    let size = rt.size();
                    let align = rt.align();
                    max_payload_size = max_payload_size.max(size);
                    max_align = max_align.max(align);
                    variant_member_types.push(Some(members));
                    variant_offsets.push(struct_layout.offsets().to_vec());
                    variant_sizes.push(size);
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

        let variant_name = value.to_string();
        let variant_index = variants
            .iter()
            .position(|(n, _)| n.to_string() == variant_name)
            .unwrap_or(0) as i64;

        if let Some(provided) = data {
            match provided {
                ObjectType::Tuple(vals) => {
                    let idx = variant_index as usize;
                    if let Some(Some(member_types)) = variant_member_types.get(idx) {
                        let mut runtime_vals: Vec<RuntimeValue> = Vec::new();
                        for v in vals {
                            let rv = if let Some(node) = v {
                                self.translate(node)
                            } else {
                                self.translate(MiddleNode::new(
                                    MiddleNodeType::IntLiteral(0),
                                    Default::default(),
                                ))
                            };
                            runtime_vals.push(rv);
                        }

                        let offsets = &variant_offsets[idx];
                        for (i, rv) in runtime_vals.iter().enumerate() {
                            let offset = offsets[i];
                            let _ = memory.with_offset(offset).write_all(
                                Some(rv.value),
                                member_types[i].ty.clone(),
                                self.module,
                                &mut self.builder,
                            );
                        }
                    }
                }
                ObjectType::Map(map) => {
                    let idx = variant_index as usize;
                    if let Some(Some(member_types)) = variant_member_types.get(idx) {
                        let mut provided_vals: std::collections::HashMap<String, RuntimeValue> =
                            std::collections::HashMap::new();
                        for (k, v) in map {
                            let rv = if let Some(node) = v {
                                self.translate(node)
                            } else {
                                self.translate(MiddleNode::new(
                                    MiddleNodeType::IntLiteral(0),
                                    Default::default(),
                                ))
                            };
                            provided_vals.insert(k.clone(), rv);
                        }

                        let offsets = &variant_offsets[idx];
                        for (i, member) in member_types.iter().enumerate() {
                            let offset = offsets[i];
                            if let Some(rv) = provided_vals.get(&member.name) {
                                let _ = memory.with_offset(offset).write_all(
                                    Some(rv.value),
                                    member.ty.clone(),
                                    self.module,
                                    &mut self.builder,
                                );
                            } else {
                                panic!("Missing enum property {}", member.name);
                            }
                        }
                    }
                }
            }
        }

        let discr_offset = max_payload_size;
        let discr_val = self.builder.ins().iconst(types::I8, variant_index as i64);
        memory.write_val(&mut self.builder, discr_val, discr_offset as i32);

        RuntimeValue::new(
            memory.into_value(&mut self.builder, self.types.ptr()),
            RuntimeType::Enum { name: enum_name },
        )
    }

    pub fn get_struct_member(&mut self, left: RuntimeValue, index: String) -> RuntimeValue {
        let RuntimeType::Struct { members, .. } = left.data_type.clone() else {
            panic!()
        };

        let layouts = left.data_type.struct_layout().unwrap();
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

    pub fn translate_struct_expression(
        &mut self,
        obj: ObjectType<Option<MiddleNode>>,
    ) -> RuntimeValue {
        match obj {
            calibre_parser::ast::ObjectType::Tuple(items) => {
                let items: Vec<RuntimeValue> = items
                    .into_iter()
                    .map(|x| x.map(|n| self.translate(n)).unwrap())
                    .collect();

                let item_runtime_types: Vec<RuntimeType> =
                    items.iter().map(|x| x.data_type.clone()).collect();

                let total_size: u32 = item_runtime_types.iter().map(|x| x.stride()).sum();
                let slot = self.builder.create_sized_stack_slot(StackSlotData::new(
                    StackSlotKind::ExplicitSlot,
                    total_size,
                    0,
                ));

                let memory = MemoryLoc::from_stack(slot, 0);

                for (i, item) in items.iter().enumerate() {
                    let offset = if i <= 0 {
                        0
                    } else {
                        item_runtime_types[0..i]
                            .iter()
                            .map(|x| x.stride() as u32)
                            .sum()
                    };

                    let _ = memory.with_offset(offset).write_all(
                        Some(item.value),
                        item_runtime_types[i].clone(),
                        self.module,
                        &mut self.builder,
                    );
                }

                RuntimeValue::new(
                    memory.into_value(&mut self.builder, self.types.ptr()),
                    RuntimeType::Tuple(item_runtime_types),
                )
            }
            calibre_parser::ast::ObjectType::Map(props) => {
                let mut members: Vec<MemberType> = Vec::new();
                let mut values: Vec<RuntimeValue> = Vec::new();

                for (k, v) in props.into_iter() {
                    let val = if let Some(node) = v {
                        self.translate(node)
                    } else {
                        self.translate(MiddleNode::new(
                            MiddleNodeType::IntLiteral(0),
                            Default::default(),
                        ))
                    };
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

                let rt = RuntimeType::Struct {
                    name: None,
                    members,
                };
                let layouts = rt.struct_layout().unwrap();
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
    }
}
