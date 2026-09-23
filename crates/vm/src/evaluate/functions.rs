use crate::{
    VM,
    conversion::{
        VMBlock,
        instructions::binary::{VMAs, VMBinary, VMBoolean, VMComparison, VMIs},
    },
    error::RuntimeError,
    evaluate::instruction::VMEvaluation,
    value::{
        RuntimeValue, TerminateValue,
        operation::{binary, boolean, comparison},
    },
};
use calibre_lir::ast::BlockId;
use calibre_parser::ast::{comparison::BooleanOperator, nodes::binary::AsFailureMode};
use dumpster::sync::Gc;
use ustr::Ustr;

impl VMEvaluation for VMCall {
    fn run(
        &self,
        vm: &mut VM,
        _block: &VMBlock,
        _ip: u32,
        _prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        if let Some(step) = self.call_registers(RegisterCall {
            dst: *dst,
            callee: *callee,
            args,
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
        let func_ptr = self.current_frame().func_ptr as *const VMFunction;
        if func_ptr.is_null() {
            return Err(RuntimeError::InvalidBytecode(
                "missing current function frame".to_string(),
            ));
        }

        let func = unsafe { &*func_ptr };

        if func.pure && (dst.is_none() || !func.returns_value) {
            return Ok(TerminateValue::None);
        }

        if let Some(dst) = dst
            && func.memo
        {
            let caller_frame = self.frames.len().saturating_sub(1);
            let mut key: Option<Vec<HashKey>> = Some(Vec::with_capacity(args.len()));

            for (i, reg) in args.iter().enumerate() {
                if func.memo_params == 0 || func.memo_params & (1 << i) != 0 {
                    let val = self.get_reg_value_in_frame(caller_frame, *reg).clone();
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
                    let cache_entry = self
                        .caches
                        .memo
                        .entry(func.name)
                        .or_insert_with(|| Arc::new(Mutex::new(FxHashMap::default())));

                    let guard = cache_entry.lock().unwrap();
                    guard.get(&k).cloned()
                } {
                    self.set_reg_value(*dst, val);
                    return Ok(TerminateValue::None);
                }

                let value = self.run_function_from_regs(
                    func,
                    args.iter().copied(),
                    Self::empty_captures(),
                    true,
                )?;

                {
                    let cache_entry = self
                        .caches
                        .memo
                        .entry(func.name)
                        .or_insert_with(|| Arc::new(Mutex::new(FxHashMap::default())));

                    cache_entry.lock().unwrap().insert(k, value.clone());
                }

                self.set_reg_value(*dst, value);
                return Ok(TerminateValue::None);
            }
        }

        let value = self.run_function_from_regs(
            func,
            args.iter().copied(),
            Self::empty_captures(),
            dst.is_some(),
        )?;

        if let Some(dst) = dst {
            self.set_reg_value(*dst, value);
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
        let resolved = self.resolve_value_ref(self.get_reg_value(*callee))?;
        let to_spawn = match resolved {
            RuntimeValue::Function { name, captures } => {
                let resolved_caps: Vec<(Ustr, RuntimeValue)> = captures
                    .as_ref()
                    .iter()
                    .map(|(k, v)| {
                        let resolved = self
                            .resolve_value_ref(v)
                            .unwrap_or_else(|_| RuntimeValue::Null);
                        let resolved = self.convert_runtime_var_into_saveable(resolved);
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
        self.spawn_async_task(to_spawn, Some(wg.clone()));
        self.set_reg_value(*dst, RuntimeValue::WaitGroup(wg));

        Ok(TerminateValue::None)
    }
}
