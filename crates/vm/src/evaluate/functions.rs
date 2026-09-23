use crate::{
    VM,
    conversion::{
        VMBlock, VMFunction,
        instructions::functions::{VMCall, VMCallSelf, VMSpawn},
    },
    error::RuntimeError,
    evaluate::{calling::RegisterCall, instruction::VMEvaluation},
    value::{HashKey, RuntimeValue, TerminateValue, WaitGroupInner},
};
use calibre_lir::ast::BlockId;
use rustc_hash::FxHashMap;
use std::sync::Arc;
use ustr::Ustr;
use wasm_sync::Mutex;

impl VMEvaluation for VMCall {
    fn run(
        &self,
        vm: &mut VM,
        block: &VMBlock,
        ip: u32,
        prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        if let Some(step) = vm.call_registers(RegisterCall {
            dst: self.dst,
            callee: self.callee,
            args: &self.args,
            block,
            ip,
            prev_block,
        })? {
            return Ok(step);
        }

        Ok(TerminateValue::None)
    }
}

impl VMEvaluation for VMCallSelf {
    fn run(
        &self,
        vm: &mut VM,
        _block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let func_ptr = vm.current_frame().func_ptr as *const VMFunction;
        if func_ptr.is_null() {
            return Err(RuntimeError::InvalidBytecode(
                "missing current function frame".to_string(),
            ));
        }

        let func = unsafe { &*func_ptr };

        if func.pure && (self.dst.is_none() || !func.returns_value) {
            return Ok(TerminateValue::None);
        }

        if let Some(dst) = &self.dst
            && func.memo
        {
            let caller_frame = vm.frames.len().saturating_sub(1);
            let mut key: Option<Vec<HashKey>> = Some(Vec::with_capacity(self.args.len()));

            for (i, reg) in self.args.iter().enumerate() {
                if func.memo_params == 0 || func.memo_params & (1 << i) != 0 {
                    let val = vm.get_reg_value_in_frame(caller_frame, *reg).clone();
                    match HashKey::try_from(val) {
                        Ok(k) => key.as_mut().unwrap().push(k),
                        Err(_) => {
                            key = None;
                            break;
                        }
                    }
                }
            }

            if let Some(k) = key {
                if let Some(val) = {
                    let cache_entry = vm
                        .caches
                        .memo
                        .entry(func.name)
                        .or_insert_with(|| Arc::new(Mutex::new(FxHashMap::default())));

                    let guard = cache_entry.lock().unwrap();
                    guard.get(&k).cloned()
                } {
                    vm.set_reg_value(*dst, val);
                    return Ok(TerminateValue::None);
                }

                let value = vm.run_function_from_regs(
                    func,
                    self.args.iter().copied(),
                    VM::empty_captures(),
                    true,
                )?;

                {
                    let cache_entry = vm
                        .caches
                        .memo
                        .entry(func.name)
                        .or_insert_with(|| Arc::new(Mutex::new(FxHashMap::default())));

                    cache_entry.lock().unwrap().insert(k, value.clone());
                }

                vm.set_reg_value(*dst, value);
                return Ok(TerminateValue::None);
            }
        }

        let value = vm.run_function_from_regs(
            func,
            self.args.iter().copied(),
            VM::empty_captures(),
            self.dst.is_some(),
        )?;

        if let Some(dst) = &self.dst {
            vm.set_reg_value(*dst, value);
        }

        Ok(TerminateValue::None)
    }
}

impl VMEvaluation for VMSpawn {
    fn run(
        &self,
        vm: &mut VM,
        _block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        let resolved = vm.resolve_value_ref(vm.get_reg_value(self.callee))?;

        let to_spawn = match resolved {
            RuntimeValue::Function { name, captures } => {
                let resolved_caps: Vec<(Ustr, RuntimeValue)> = captures
                    .as_ref()
                    .iter()
                    .map(|(k, v)| {
                        let resolved = vm
                            .resolve_value_ref(v)
                            .unwrap_or_else(|_| RuntimeValue::Null);
                        let resolved = vm.convert_runtime_var_into_saveable(resolved);
                        (*k, resolved)
                    })
                    .collect();
                RuntimeValue::Function {
                    name,
                    captures: Arc::new(resolved_caps),
                }
            }
            other => other,
        };

        let wg = Arc::new(WaitGroupInner::default());
        wg.count.store(1, std::sync::atomic::Ordering::Release);
        vm.spawn_async_task(to_spawn, Some(wg.clone()));
        vm.set_reg_value(self.dst, RuntimeValue::WaitGroup(wg));

        Ok(TerminateValue::None)
    }
}
