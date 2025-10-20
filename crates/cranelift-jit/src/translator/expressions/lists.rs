use crate::{
    translator::{FunctionTranslator, layout::GetLayoutInfo, memory::MemoryLoc},
    values::{RuntimeType, RuntimeValue},
};
use calibre_parser::ast::{Node, NodeType, comparison::Comparison};
use cranelift::{
    codegen::ir::{BlockArg, StackSlot},
    prelude::*,
};
use cranelift_module::{Linkage, Module};

impl<'a> FunctionTranslator<'a> {
    pub fn translate_array_compare(
        &mut self,
        left: RuntimeValue,
        right: RuntimeValue,
        operator: Comparison,
    ) -> Value {
        let RuntimeType::List(left_r_type) = left.data_type else {
            panic!()
        };
        let RuntimeType::List(right_r_type) = right.data_type else {
            panic!()
        };

        let lhs_len = self
            .builder
            .ins()
            .load(self.types.ptr(), MemFlags::trusted(), left.value, 0);
        let rhs_len =
            self.builder
                .ins()
                .load(self.types.ptr(), MemFlags::trusted(), right.value, 0);

        let lhs = self.builder.ins().load(
            self.types.ptr(),
            MemFlags::trusted(),
            left.value,
            self.types.ptr().bytes() as i32,
        );

        let rhs = self.builder.ins().load(
            self.types.ptr(),
            MemFlags::trusted(),
            right.value,
            self.types.ptr().bytes() as i32,
        );

        let false_val = self.builder.ins().iconst(types::I8, 0);
        let true_val = self.builder.ins().iconst(types::I8, 1);

        let idx = self.builder.declare_var(self.types.ptr());
        let zero = self.builder.ins().iconst(self.types.ptr(), 0);
        self.builder.def_var(idx, zero);

        let header = self.builder.create_block();
        let header_idx = self.builder.append_block_param(header, self.types.ptr());
        let body = self.builder.create_block();
        let body_idx = self.builder.append_block_param(body, self.types.ptr());
        let exit = self.builder.create_block();
        let exit_bool = self.builder.append_block_param(exit, types::I8);

        self.builder.ins().jump(header, &[BlockArg::Value(zero)]);

        self.builder.switch_to_block(header);
        let cond = self
            .builder
            .ins()
            .icmp(IntCC::UnsignedLessThan, header_idx, lhs_len);
        self.builder.ins().brif(
            cond,
            body,
            &[BlockArg::Value(header_idx)],
            exit,
            &[match operator {
                Comparison::Equal => BlockArg::Value(true_val),
                Comparison::NotEqual => BlockArg::Value(false_val),
                _ => unimplemented!(),
            }],
        );

        self.builder.switch_to_block(body);
        self.builder.seal_block(body);
        let left_ty = self.types.get_type_from_runtime_type(&left_r_type);
        let right_ty = self.types.get_type_from_runtime_type(&right_r_type);
        let lhs_offset = self
            .builder
            .ins()
            .imul_imm(body_idx, left_ty.bytes() as i64);
        let rhs_offset = self
            .builder
            .ins()
            .imul_imm(body_idx, right_ty.bytes() as i64);
        let lhs_addr = self.builder.ins().iadd(lhs, lhs_offset);
        let rhs_addr = self.builder.ins().iadd(rhs, rhs_offset);

        let res = if false {
            // Add complex type comparisons
            // self.compile_complex_compare(lhs_addr, rhs_addr, sub_ty, hir_op)
            unreachable!()
        } else {
            let lhs_item = self
                .builder
                .ins()
                .load(left_ty, MemFlags::trusted(), lhs_addr, 0);
            let rhs_item = self
                .builder
                .ins()
                .load(right_ty, MemFlags::trusted(), rhs_addr, 0);

            self.translate_comparisson_expression(
                RuntimeValue::new(lhs_item, *left_r_type),
                RuntimeValue::new(rhs_item, *right_r_type),
                operator.clone(),
            )
        };

        let idx_plus_one = self.builder.ins().iadd_imm(body_idx, 1);

        // if the check was successful, check the next item of the array, otherwise exit
        // with false

        match operator {
            Comparison::Equal => {
                self.builder.ins().brif(
                    res.value,
                    header,
                    &[BlockArg::Value(idx_plus_one)],
                    exit,
                    &[BlockArg::Value(false_val)],
                );
            }
            Comparison::NotEqual => {
                self.builder.ins().brif(
                    res.value,
                    exit,
                    &[BlockArg::Value(true_val)],
                    header,
                    &[BlockArg::Value(idx_plus_one)],
                );
            }
            _ => unimplemented!(),
        }

        self.builder.switch_to_block(exit);
        self.builder.seal_block(header);
        self.builder.seal_block(exit);

        exit_bool
    }

    pub fn get_array_member(&mut self, left: RuntimeValue, index: Value) -> RuntimeValue {
        let RuntimeType::List(left_r_type) = left.data_type else {
            panic!()
        };

        let lhs = self
            .builder
            .ins()
            .load(self.types.ptr(), MemFlags::trusted(), left.value, 0);

        let lhs_offset = self
            .builder
            .ins()
            .imul_imm(index, left_r_type.stride() as i64);

        let lhs_addr = self.builder.ins().iadd(lhs, lhs_offset);

        let lhs = self.builder.ins().load(
            self.types.get_type_from_runtime_type(&left_r_type),
            MemFlags::trusted(),
            lhs_addr,
            0,
        );

        RuntimeValue::new(lhs, *left_r_type)
    }

    pub fn get_tuple_member(&mut self, left: RuntimeValue, index: usize) -> RuntimeValue {
        let RuntimeType::Tuple(types) = left.data_type else {
            panic!()
        };

        let lhs = self
            .builder
            .ins()
            .load(self.types.ptr(), MemFlags::trusted(), left.value, 0);

        let lhs = self.builder.ins().load(
            self.types.get_type_from_runtime_type(&types[index]),
            MemFlags::new().with_aligned(),
            lhs,
            types[0..index]
                .iter()
                .map(|x| x.stride() as i32)
                .sum::<i32>(),
        );

        RuntimeValue::new(lhs, types[index].clone())
    }

    pub fn translate_tuple_expression(&mut self, items: Vec<Node>) -> RuntimeValue {
        let items: Vec<RuntimeValue> = items.into_iter().map(|x| self.translate(x)).collect();
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

    pub fn translate_array_expression(&mut self, items: Vec<Node>) -> RuntimeValue {
        let items: Vec<RuntimeValue> = items.into_iter().map(|x| self.translate(x)).collect();

        let element_type = items
            .get(0)
            .map(|v| v.data_type.clone())
            .unwrap_or(RuntimeType::Int);
        let stride = element_type.stride();

        let slot = self.builder.create_sized_stack_slot(StackSlotData {
            kind: StackSlotKind::ExplicitSlot,
            size: stride * 32,
            align_shift: element_type.align_shift(),
        });

        let memory = MemoryLoc::from_stack(slot, 0);

        for (i, item) in items.iter().enumerate() {
            let offset = (i as u32) * stride;
            let _ = memory.with_offset(offset).write_all(
                Some(item.value),
                element_type.clone(),
                self.module,
                &mut self.builder,
            );
        }

        RuntimeValue::new(
            memory.into_value(&mut self.builder, self.types.ptr()),
            RuntimeType::List(Box::new(element_type)),
        )
    }
}
