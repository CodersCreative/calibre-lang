// Code "inpsired" by Capy Lang (https://github.com/capy-language)

use cranelift::{
    codegen::ir::StackSlot,
    prelude::{FunctionBuilder, InstBuilder, MemFlags, Type, Value},
};
use cranelift_module::Module;

use crate::{translator::layout::GetLayoutInfo, values::RuntimeType};

#[derive(Debug, Clone, Copy)]
enum Location {
    Stack(StackSlot),
    Addr(Value),
}

#[derive(Debug, Clone, Copy)]
pub struct MemoryLoc {
    addr: Location,
    offset: u32,
}

impl MemoryLoc {
    pub fn from_stack(stack_slot: StackSlot, offset: u32) -> MemoryLoc {
        MemoryLoc {
            addr: Location::Stack(stack_slot),
            offset,
        }
    }

    pub fn from_addr(addr: Value, offset: u32) -> MemoryLoc {
        MemoryLoc {
            addr: Location::Addr(addr),
            offset,
        }
    }

    pub fn with_offset(self, offset: u32) -> MemoryLoc {
        MemoryLoc {
            addr: self.addr,
            offset: self.offset + offset,
        }
    }

    /// This converts the address + offset into a single address if needed
    pub fn into_value(self, builder: &mut FunctionBuilder, ptr_ty: Type) -> Value {
        match self.addr {
            Location::Stack(slot) => builder.ins().stack_addr(ptr_ty, slot, self.offset as i32),
            Location::Addr(addr) => {
                if self.offset > 0 {
                    builder.ins().iadd_imm(addr, self.offset as i64)
                } else {
                    addr
                }
            }
        }
    }

    /// As opposed to `write_all`, this writes a single `Value` to an `offset`
    pub fn write_val(&self, builder: &mut FunctionBuilder, x: Value, offset: i32) {
        match self.addr {
            Location::Stack(slot) => {
                builder
                    .ins()
                    .stack_store(x, slot, offset + self.offset as i32)
            }
            Location::Addr(addr) => {
                builder
                    .ins()
                    .store(MemFlags::trusted(), x, addr, offset + self.offset as i32)
            }
        };
    }

    /// Does a simple write or a memmove if necessary
    /// Unlike `write_val`, this writes an *entire* object into the memory
    ///
    /// By using this function, you promise that the bytes of val can fit inside
    /// the given MemoryLoc.
    ///
    /// You also promise that the alignment of val matches the alignment of the
    /// the given MemoryLoc.
    pub fn write_all(
        self,
        val: Option<Value>,
        ty: RuntimeType,
        module: &mut dyn Module,
        builder: &mut FunctionBuilder,
    ) {
        let Some(val) = val else {
            return;
        };

        if ty.is_aggregate() {
            match self.addr {
                Location::Addr(mut addr) => {
                    if self.offset != 0 {
                        addr = builder.ins().iadd_imm(addr, self.offset as i64);
                    }
                    builder.emit_small_memory_copy(
                        module.target_config(),
                        addr,
                        val,
                        // this has to be stride for some reason, it can't be size
                        ty.stride() as u64,
                        ty.align() as u8,
                        ty.align() as u8,
                        true,
                        MemFlags::trusted(),
                    )
                }
                Location::Stack(slot) => {
                    // be very explicit to cranelift what we are doing here
                    // since there is no `emit_stack_memcpy`, do it ourselves
                    let mut off = 0;
                    macro_rules! mem_cpy_loop {
                        ($width:expr) => {
                            while (off + $width) <= (ty.stride() as i32 / $width) * $width {
                                let bytes = builder.ins().load(
                                    cranelift::codegen::ir::Type::int_with_byte_size($width)
                                        .unwrap(),
                                    MemFlags::trusted(),
                                    val,
                                    off,
                                );
                                builder
                                    .ins()
                                    .stack_store(bytes, slot, off + self.offset as i32);
                                off += $width;
                            }
                        };
                    }
                    mem_cpy_loop!(8);
                    mem_cpy_loop!(4);
                    mem_cpy_loop!(2);
                    mem_cpy_loop!(1);
                }
            }
        } else {
            match self.addr {
                Location::Stack(slot) => builder.ins().stack_store(val, slot, self.offset as i32),
                Location::Addr(addr) => {
                    builder
                        .ins()
                        .store(MemFlags::trusted(), val, addr, self.offset as i32)
                }
            };
        }
    }

    /// writes the byte `val` into every byte of `ty`
    fn memset(
        self,
        module: &mut dyn Module,
        builder: &mut FunctionBuilder,
        val: u8,
        ty: RuntimeType,
    ) {
        match self.addr {
            Location::Addr(mut addr) => {
                if self.offset != 0 {
                    addr = builder.ins().iadd_imm(addr, self.offset as i64);
                }
                builder.emit_small_memset(
                    module.target_config(),
                    addr,
                    val,
                    ty.size() as u64,
                    ty.align() as u8,
                    MemFlags::trusted(),
                );
            }
            Location::Stack(slot) => {
                // be very explicit to cranelift what we are doing here
                // since there is no `emit_stack_memcpy`, do it ourselves
                let mut off = 0;
                macro_rules! mem_cpy_loop {
                    ($width:expr) => {
                        while (off + $width) <= (ty.stride() as i32 / $width) * $width {
                            let val = builder.ins().iconst(
                                cranelift::codegen::ir::Type::int_with_byte_size(8).unwrap(),
                                val as i64,
                            );
                            builder
                                .ins()
                                .stack_store(val, slot, off + self.offset as i32);
                            off += $width;
                        }
                    };
                }
                mem_cpy_loop!(8);
                mem_cpy_loop!(4);
                mem_cpy_loop!(2);
                mem_cpy_loop!(1);
            }
        }
    }
}
