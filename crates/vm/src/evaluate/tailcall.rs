use super::*;
use crate::conversion::{VMBlock, VMFunction, VMInstruction};
use crate::value::RuntimeValue;
use std::sync::Arc;

impl VM {
    fn is_tail_position(block: &VMBlock, ip: u32, dst: u16) -> bool {
        let Some(VMInstruction::Return {
            value: Some(ret_reg),
        }) = block.instructions.get((ip as usize).saturating_add(1))
        else {
            return false;
        };

        *ret_reg == dst
    }

    fn prepare_frame_for_tail_call(&mut self, func: &VMFunction) {
        let start = self.current_frame().reg_start;
        let reg_count = func.reg_count as usize;
        let frame_end = start + reg_count;

        if frame_end > self.reg_arena.len() {
            self.reg_arena.resize(frame_end, RuntimeValue::Null);
        }

        for slot in &mut self.reg_arena[start..frame_end] {
            *slot = RuntimeValue::Null;
        }

        self.reg_top = frame_end;
        {
            let frame = self.current_frame_mut();
            frame.reg_count = reg_count;
            frame.acc = RuntimeValue::Null;
            frame.func_ptr = func as *const VMFunction as usize;
            frame.func_name = Some(func.name.clone());
        }
    }

    fn setup_tail_call_args(&mut self, args: &[u16], func: &VMFunction) {
        let caller_frame = self.frames.len().saturating_sub(1);
        let call_args: Vec<RuntimeValue> = args
            .iter()
            .map(|reg| self.call_arg_from_frame_reg(caller_frame, *reg))
            .collect();

        let start = self.current_frame().reg_start;
        let reg_count = func.reg_count as usize;

        for (reg, arg) in func.param_regs.iter().zip(call_args) {
            let idx = *reg as usize;
            if idx < reg_count {
                self.reg_arena[start + idx] = arg;
            }
        }

        for (name, reg) in func.params.iter().zip(func.param_regs.iter().copied()) {
            let value = self.get_reg_value(reg).clone();
            let _ = self.variables.insert(*name, value);
        }
    }

    pub(crate) fn try_tail_call(
        &mut self,
        block: &VMBlock,
        ip: u32,
        dst: u16,
        args: &[u16],
        func: &VMFunction,
        captures: &Arc<Vec<(Ustr, RuntimeValue)>>,
    ) -> Option<TerminateValue> {
        if !Self::is_tail_position(block, ip, dst) {
            return None;
        }

        if !captures.is_empty() {
            return None;
        }

        if args.len() != func.param_regs.len() {
            return None;
        }

        self.prepare_frame_for_tail_call(func);
        self.setup_tail_call_args(args, func);

        Some(TerminateValue::Jump(func.entry))
    }
}
