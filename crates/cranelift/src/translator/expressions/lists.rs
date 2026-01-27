use crate::{
    translator::{FunctionTranslator, layout::GetLayoutInfo, memory::MemoryLoc},
    values::{RuntimeType, RuntimeValue},
};
use calibre_lir::LirNodeType;
use calibre_mir_ty::MiddleNode;
use calibre_parser::ast::{Node, comparison::ComparisonOperator};
use cranelift::{codegen::ir::BlockArg, prelude::*};

impl<'a> FunctionTranslator<'a> {
    pub fn translate_array_compare(
        &mut self,
        left: RuntimeValue,
        right: RuntimeValue,
        operator: ComparisonOperator,
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
        let _rhs_len =
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
                ComparisonOperator::Equal => BlockArg::Value(true_val),
                ComparisonOperator::NotEqual => BlockArg::Value(false_val),
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

        match operator {
            ComparisonOperator::Equal => {
                self.builder.ins().brif(
                    res.value,
                    header,
                    &[BlockArg::Value(idx_plus_one)],
                    exit,
                    &[BlockArg::Value(false_val)],
                );
            }
            ComparisonOperator::NotEqual => {
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

        let data_ptr = self.builder.ins().load(
            self.types.ptr(),
            MemFlags::trusted(),
            left.value,
            self.types.ptr().bytes() as i32,
        );

        let elem_offset = self
            .builder
            .ins()
            .imul_imm(index, left_r_type.stride() as i64);

        let elem_addr = self.builder.ins().iadd(data_ptr, elem_offset);

        let val = self.builder.ins().load(
            self.types.get_type_from_runtime_type(&left_r_type),
            MemFlags::trusted(),
            elem_addr,
            0,
        );

        RuntimeValue::new(val, *left_r_type)
    }

    pub fn translate_array_expression(
        &mut self,
        element_type: RuntimeType,
        items: Vec<LirNodeType>,
    ) -> RuntimeValue {
        let items: Vec<RuntimeValue> = items.into_iter().map(|x| self.translate(x)).collect();

        let stride = element_type.stride();

        let data_size = stride * (items.len() as u32);
        let data_slot = self.builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            data_size,
            element_type.align_shift(),
        ));
        let data_memory = MemoryLoc::from_stack(data_slot, 0);

        for (i, item) in items.iter().enumerate() {
            let offset = (i as u32) * stride;
            let _ = data_memory.with_offset(offset).write_all(
                Some(item.value),
                element_type.clone(),
                self.module,
                &mut self.builder,
            );
        }

        let header_size = (self.types.ptr().bytes() * 2) as u32;
        let header_slot = self.builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            header_size,
            0,
        ));
        let header_memory = MemoryLoc::from_stack(header_slot, 0);

        let len_val = self
            .builder
            .ins()
            .iconst(self.types.ptr(), items.len() as i64);
        header_memory.write_val(&mut self.builder, len_val, 0);

        let data_ptr = data_memory.into_value(&mut self.builder, self.types.ptr());
        header_memory.write_val(&mut self.builder, data_ptr, self.types.ptr().bytes() as i32);

        RuntimeValue::new(
            header_memory.into_value(&mut self.builder, self.types.ptr()),
            RuntimeType::List(Box::new(element_type)),
        )
    }
}
