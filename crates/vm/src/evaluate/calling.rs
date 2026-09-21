use super::{super::VM, write_back::Propagation};
use crate::{
    conversion::{Reg, VMFunction},
    error::RuntimeError,
    value::{HashKey, RuntimeValue, TerminateValue},
};
use calibre_lir::ast::BlockId;
use rustc_hash::FxHashMap;
use std::sync::Arc;
use tracing::{instrument, trace};
use ustr::{Ustr, UstrSet};
use wasm_sync::Mutex;

impl VM {
    #[instrument(skip_all, fields(args_count = args.len(), callsite_block = callsite_block))]
    pub(crate) fn call_runtime_callable_at(
        &mut self,
        callable: RuntimeValue,
        args: Vec<RuntimeValue>,
        callsite_block: usize,
        callsite_tag: u32,
        get_result: bool,
    ) -> Result<RuntimeValue, RuntimeError> {
        match self.resolve_value_ref(&callable)? {
            RuntimeValue::Function { name, captures } => {
                let callsite = (self.current_frame().func_ptr, callsite_block, callsite_tag);
                
                let Some(func) = self.resolve_callable_cached(name, callsite) else {
                    return Err(RuntimeError::FunctionNotFound(name.to_string()));
                };
                
                if func.pure && (!get_result || !func.returns_value) {
                    return Ok(RuntimeValue::Null);
                }
                
                let mut seen = UstrSet::default();
                let mut refreshed_caps = Vec::with_capacity(captures.len());
                let mut seen_names = UstrSet::default();
                
                for (cap_name, old_value) in captures.iter() {
                    if !seen_names.insert(*cap_name) {
                        continue;
                    }
                    
                    let value = self.capture_value(cap_name, &mut seen);
                    refreshed_caps.push((
                        *cap_name,
                        if matches!(value, RuntimeValue::Null)
                            && !matches!(old_value, RuntimeValue::Null)
                        {
                            old_value.clone()
                        } else {
                            value
                        },
                    ));
                }

                let refreshed = Arc::new(refreshed_caps);
                if func.memo {
                    let mut key: Option<Vec<HashKey>> = Some(Vec::with_capacity(args.len()));
                    
                    for (i, arg) in args.iter().enumerate() {
                        if func.memo_params == 0 || func.memo_params & (1 << i) != 0 {
                            match HashKey::try_from(arg.clone()) {
                                Ok(hash) => key.as_mut().unwrap().push(hash),
                                Err(_) => {
                                    key = None;
                                    break;
                                }
                            }
                        }
                    }
                    
                    if let Some(key) = key {
                        let cache = Arc::clone(
                            self.caches
                                .memo
                                .entry(name)
                                .or_insert_with(|| Arc::new(Mutex::new(FxHashMap::default()))),
                        );
                        
                        if let Some(value) = cache.lock().unwrap().get(&key) {
                            return Ok(value.clone());
                        }
                        
                        let result =
                            self.run_function(func.as_ref(), args, refreshed, get_result)?;
                        cache.lock().unwrap().insert(key, result.clone());
                        
                        return Ok(result);
                    }
                }
                
                self.run_function(func.as_ref(), args, refreshed, get_result)
            }
            RuntimeValue::NativeFunction(func) => func.run(self, args),
            #[cfg(feature = "native")]
            RuntimeValue::ExternFunction(func) => func.call(self, &args),
            RuntimeValue::BoundMethod { callee, receiver } => {
                let mut full_args = vec![receiver.as_ref().clone()];
                full_args.extend(args);
                
                if full_args.len() >= 2 {
                    let same_identity = matches!((&full_args[0], &full_args[1]), (RuntimeValue::List(a), RuntimeValue::List(b)) if std::ptr::eq(a.as_ref(), b.as_ref()))
                        || matches!((&full_args[0], &full_args[1]), (RuntimeValue::HashMap(a), RuntimeValue::HashMap(b)) if std::ptr::eq(a.as_ref(), b.as_ref()))
                        || matches!((&full_args[0], &full_args[1]), (RuntimeValue::HashSet(a), RuntimeValue::HashSet(b)) if std::ptr::eq(a.as_ref(), b.as_ref()));
                    if same_identity {
                        full_args.truncate(1);
                    }
                }

                self.call_runtime_callable_at(
                    *callee,
                    full_args,
                    callsite_block,
                    callsite_tag.saturating_sub(1),
                    get_result,
                )
            }
            other => Err(RuntimeError::InvalidFunctionCallValue(Box::new(other))),
        }
    }
}

pub enum FunctionArgs<'a> {
    Values(&'a [RuntimeValue]),
    Regs(&'a [Reg]),
}

impl VM {
    pub fn run_function<I>(
        &mut self,
        function: &VMFunction,
        args: I,
        captures: Arc<Vec<(Ustr, RuntimeValue)>>,
        get_result: bool,
    ) -> Result<RuntimeValue, RuntimeError>
    where
        I: IntoIterator<Item = RuntimeValue>,
    {
        self.in_global = false;
        let mut state = crate::TaskState::default();
        let args = args.into_iter().collect::<Vec<_>>();

        match self.run_function_with_budget(
            function,
            FunctionArgs::Values(&args),
            captures,
            usize::MAX,
            &mut state,
            get_result,
        )? {
            Some(value) => Ok(value),
            None => Ok(RuntimeValue::Null),
        }
    }

    #[instrument(skip_all, fields(function = %function.name))]
    #[inline]
    pub(crate) fn run_function_from_regs<I>(
        &mut self,
        function: &VMFunction,
        args: I,
        captures: Arc<Vec<(Ustr, RuntimeValue)>>,
        get_result: bool,
    ) -> Result<RuntimeValue, RuntimeError>
    where
        I: IntoIterator<Item = Reg>,
    {
        self.in_global = false;
        let mut state = crate::TaskState::default();
        let args = args.into_iter().collect::<Vec<_>>();

        match self.run_function_with_budget(
            function,
            FunctionArgs::Regs(&args),
            captures,
            usize::MAX,
            &mut state,
            get_result,
        )? {
            Some(value) => Ok(value),
            None => Ok(RuntimeValue::Null),
        }
    }

    #[instrument(skip_all, fields(function = %function.name, budget = budget))]
    pub fn run_function_with_budget<'b>(
        &mut self,
        function: &VMFunction,
        args: FunctionArgs<'b>,
        captures: Arc<Vec<(Ustr, RuntimeValue)>>,
        budget: usize,
        state: &mut crate::TaskState,
        get_result: bool,
    ) -> Result<Option<RuntimeValue>, RuntimeError> {
        trace!("running function with budget");
        let caller_frame = self.frames.len().saturating_sub(1);
        state.yielded = None;
        let mut propagateable_args = None;

        let prev_vars = if state.block.is_none() {
            let func_ptr = function as *const VMFunction as usize;
            self.push_frame(function.reg_count as usize, func_ptr, Some(function.name));

            match args {
                FunctionArgs::Values(args) => {
                    for (reg, arg) in function.param_regs.iter().zip(args) {
                        self.set_reg_value(*reg, arg.clone());
                    }
                }
                FunctionArgs::Regs(args) => {
                    propagateable_args = Some(args.to_vec());
                    for (reg, arg_reg) in function.param_regs.iter().zip(args.iter().copied()) {
                        let arg = self.get_reg_value_in_frame(caller_frame, arg_reg).clone();
                        self.set_reg_value(*reg, arg);
                    }
                }
            }

            for (name, reg) in function
                .params
                .iter()
                .zip(function.param_regs.iter().copied())
            {
                let value = self.get_reg_value(reg).clone();
                let _ = self.variables.insert(*name, value);
            }

            let filtered_captures: Vec<(Ustr, RuntimeValue)> = captures
                .iter()
                .filter(|(name, _)| {
                    Self::should_install_capture(name) && !function.param_names.contains(name)
                })
                .cloned()
                .collect();

            state.block = Some(function.entry);
            state.ip = 0;
            state.prev_block = None;
            self.install_captures(filtered_captures.as_slice())
        } else {
            self.install_captures(captures.as_ref())
        };

        let mut block_id = state.block.unwrap_or(function.entry);
        let mut block = function
            .blocks
            .get(*function.block_map.get(&block_id).unwrap_or(&0))
            .ok_or_else(|| RuntimeError::InvalidBytecode("function has no blocks".to_string()))?;
        let mut prev_block: Option<BlockId> = state.prev_block;
        let mut result = RuntimeValue::Null;
        let mut returned = false;

        loop {
            let slice_budget = if budget == usize::MAX {
                None
            } else {
                Some(budget.max(1))
            };

            match self.run_block_with_budget(block, prev_block, state.ip, slice_budget)? {
                TerminateValue::Jump(target) => {
                    prev_block = Some(block.id);
                    block_id = target;
                    block = function
                        .blocks
                        .get(*function.block_map.get(&block_id).unwrap_or(&0))
                        .ok_or_else(|| {
                            RuntimeError::InvalidBytecode(format!(
                                "invalid function block {}",
                                target.0
                            ))
                        })?;
                    state.ip = 0;
                    state.block = Some(block_id);
                    state.prev_block = prev_block;
                }
                TerminateValue::Return(x) => {
                    result = x;
                    returned = true;
                    break;
                }
                TerminateValue::Yield {
                    block,
                    ip,
                    prev_block,
                    yielded,
                } => {
                    state.block = Some(block);
                    state.ip = ip;
                    state.prev_block = prev_block;
                    state.yielded = yielded;
                    return Ok(None);
                }
                TerminateValue::None => break,
            }
        }

        if function.returns_value && !returned && get_result {
            result = self.get_reg_value(function.ret_reg).clone();
        }

        if let RuntimeValue::RegRef { frame, reg } = result {
            result = self.get_reg_value_in_frame(frame, reg).clone();
        }

        if let Some(args) = propagateable_args {
            self.propagate_member_source_args(&args, caller_frame)?;
        }

        self.pop_frame();
        self.restore_captures(prev_vars);

        for name in function.params.iter() {
            self.variables.remove(name);
        }

        Ok(Some(result))
    }
}
