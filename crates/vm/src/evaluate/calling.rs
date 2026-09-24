use super::{super::VM, write_back::Propagation};
use crate::{
    VarName,
    conversion::{Reg, VMBlock, VMFunction},
    error::RuntimeError,
    value::{RuntimeValue, TerminateValue},
};
use calibre_lir::ast::BlockId;
use calibre_parser::ast::idents::ParserText;
use dumpster::sync::Gc;
use rustc_hash::FxHashMap;
use std::sync::Arc;
use tracing::{instrument, trace};
use ustr::{Ustr, UstrSet};
use wasm_sync::Mutex;

impl VM {
    #[inline]
    fn store_call_result(&mut self, destination: Option<Reg>, value: RuntimeValue) {
        if let Some(destination) = destination {
            self.set_reg_value(destination, value);
        }
    }

    pub(crate) fn capture_value(&self, name: &Ustr, seen: &mut UstrSet) -> RuntimeValue {
        match self.resolve_var_name(*name) {
            Some(VarName::Var(var)) => {
                if let Some(value) = self.variables.get(&var) {
                    self.resolve_saveable_runtime_value_ref(value)
                } else {
                    unreachable!()
                }
            }
            Some(VarName::Func(func)) => self
                .registry
                .functions
                .get(&func)
                .map(|f| self.make_runtime_function_inner(f, seen))
                .unwrap_or_else(|| RuntimeValue::Null),
            _ => RuntimeValue::Null,
        }
    }

    fn refresh_captures(
        &mut self,
        captures: &[(Ustr, RuntimeValue)],
    ) -> Arc<Vec<(Ustr, RuntimeValue)>> {
        let mut seen = UstrSet::default();
        let mut names = UstrSet::default();
        let mut refreshed = Vec::with_capacity(captures.len());

        for (name, old_value) in captures {
            if !names.insert(*name) {
                continue;
            }
            let value = self.capture_value(name, &mut seen);
            refreshed.push((
                *name,
                if value.is_null() && !old_value.is_null() {
                    old_value.clone()
                } else {
                    value
                },
            ));
        }

        Arc::new(refreshed)
    }

    fn resolve_vm_function(
        &mut self,
        name: Ustr,
        site: CallSite,
    ) -> Result<Arc<VMFunction>, RuntimeError> {
        let callsite = (self.current_frame().func_ptr, site.block, site.tag);
        self.resolve_callable_cached(name, callsite)
            .ok_or_else(|| RuntimeError::FunctionNotFound(name.to_string()))
    }

    #[instrument(skip_all, fields(args_count = args.len(), callsite = ?site))]
    pub(crate) fn call_runtime_callable_at(
        &mut self,
        callable: RuntimeValue,
        args: Vec<RuntimeValue>,
        site: CallSite,
        get_result: bool,
    ) -> Result<RuntimeValue, RuntimeError> {
        match self.resolve_value_ref(&callable)? {
            RuntimeValue::Function { name, captures } => {
                let func = self.resolve_vm_function(name, site)?;

                if func.pure && (!get_result || !func.returns_value) {
                    return Ok(RuntimeValue::Null);
                }

                let refreshed = self.refresh_captures(captures.as_ref());
                if func.memo
                    && let Some(key) = func.memo_key(&args)
                {
                    let cache = Arc::clone(
                        self.caches
                            .memo
                            .entry(name)
                            .or_insert_with(|| Arc::new(Mutex::new(FxHashMap::default()))),
                    );

                    if let Some(value) = cache.lock().unwrap().get(&key) {
                        return Ok(value.clone());
                    }

                    let result = self.run_function(func.as_ref(), args, refreshed, get_result)?;
                    cache.lock().unwrap().insert(key, result.clone());

                    return Ok(result);
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
                        || matches!((&full_args[0], &full_args[1]), (RuntimeValue::HashMap(a), RuntimeValue::HashMap(b)) if std::ptr::eq(a.map.as_ref(), b.map.as_ref()))
                        || matches!((&full_args[0], &full_args[1]), (RuntimeValue::HashSet(a), RuntimeValue::HashSet(b)) if std::ptr::eq(a.set.as_ref(), b.set.as_ref()));
                    if same_identity {
                        full_args.truncate(1);
                    }
                }

                self.call_runtime_callable_at(
                    *callee,
                    full_args,
                    CallSite {
                        block: site.block,
                        tag: site.tag.saturating_sub(1),
                    },
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

#[derive(Debug, Clone, Copy)]
pub(crate) struct CallSite {
    pub block: usize,
    pub tag: u32,
}

pub(crate) struct RegisterCall<'a> {
    pub dst: Option<Reg>,
    pub callee: Reg,
    pub args: &'a [Reg],
    pub block: &'a VMBlock,
    pub ip: u32,
    pub prev_block: Option<BlockId>,
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

    fn run_bound_method_call(
        &mut self,
        callee: RuntimeValue,
        receiver: RuntimeValue,
        args: &[u16],
        block: &VMBlock,
        ip: u32,
        get_result: bool,
    ) -> Result<RuntimeValue, RuntimeError> {
        let _ = self.resolve_value_ref(&receiver)?;

        let receiver_reg = if let RuntimeValue::RegRef { frame, reg } = &receiver {
            Some((*frame, *reg))
        } else {
            None
        };

        let mut full_args = vec![receiver];
        full_args.extend(self.collect_call_args_vec(args));
        let out = self.call_runtime_callable_at(
            callee,
            full_args,
            CallSite {
                block: block.id.0 as usize,
                tag: ip,
            },
            get_result,
        )?;

        if let Some((frame_idx, reg)) = receiver_reg
            && frame_idx == self.frames.len().saturating_sub(1)
        {
            let mut source = self.current_frame().member_sources.get(&reg).cloned();

            if source.is_none()
                && let RuntimeValue::List(target_list) = self.get_reg_value(reg)
            {
                for (candidate_reg, candidate_source) in self.current_frame().member_sources.iter()
                {
                    if let RuntimeValue::List(other_list) = self.get_reg_value(*candidate_reg)
                        && std::ptr::eq(other_list.as_ref(), target_list.as_ref())
                    {
                        source = Some(*candidate_source);
                        break;
                    }
                }
            }

            if let Some((parent_reg, member_name)) = source {
                let updated_field = self.get_reg_value(reg).clone();
                let parent_raw = self.get_reg_value(parent_reg);
                let parent_resolved = self.resolve_value_ref(parent_raw)?;
                if let RuntimeValue::Aggregate(type_name, mut map) = parent_resolved
                    && let Some(entry) = Gc::make_mut(&mut map)
                        .0
                        .0
                        .iter_mut()
                        .find(|(field, _)| field == &member_name)
                {
                    entry.1 = updated_field;

                    let updated_parent = RuntimeValue::Aggregate(type_name, map);

                    match parent_raw.clone() {
                        RuntimeValue::RegRef { frame, reg } => {
                            self.set_reg_value_in_frame(frame, reg, updated_parent);
                        }
                        RuntimeValue::Ref(name) => {
                            self.variables.insert(name, updated_parent);
                        }
                        RuntimeValue::VarRef(id) => {
                            let _ = self.variables.set_by_id(id, updated_parent);
                        }
                        _ => {
                            self.set_reg_value(parent_reg, updated_parent);
                        }
                    }
                }
            }
        }
        Ok(out)
    }

    fn handle_call_result(
        &mut self,
        dst: Option<u16>,
        func: &Arc<dyn crate::NativeFunction>,
        args: &[u16],
        block: &VMBlock,
        ip: u32,
        prev_block: Option<BlockId>,
    ) -> Result<Option<TerminateValue>, RuntimeError> {
        let result = func.run(self, self.collect_call_args_vec(args))?;

        if let RuntimeValue::GeneratorSuspend(value) = result {
            let yielded = *value;
            if let Some(dst) = dst {
                self.set_reg_value(dst, yielded.clone());
            }
            let frame_idx = self.frames.len().saturating_sub(1);
            self.propagate_member_source_args(args, frame_idx)?;

            return Ok(Some(TerminateValue::Yield {
                block: block.id,
                ip: ip as usize + 1,
                prev_block,
                yielded: Some(yielded),
            }));
        }
        self.store_call_result(dst, result);
        Ok(None)
    }

    pub(crate) fn call_registers(
        &mut self,
        RegisterCall {
            dst,
            callee,
            args,
            block,
            ip,
            prev_block,
        }: RegisterCall<'_>,
    ) -> Result<Option<TerminateValue>, RuntimeError> {
        let func = {
            let value = self.get_reg_value(callee);
            if value.is_callable() {
                value.clone()
            } else {
                self.resolve_value_ref(value)?
            }
        };

        let func = if func.is_callable() {
            func
        } else if let Some((source_reg, member_name)) =
            self.current_frame().member_sources.get(&callee).cloned()
        {
            let (short_name, _) = Self::member_parts(&member_name);
            let raw_receiver = self.get_reg_value(source_reg).clone();
            let resolved_receiver = self
                .resolve_value_ref(&raw_receiver)
                .unwrap_or(func.clone());
            let resolved = match &resolved_receiver {
                RuntimeValue::Aggregate(Some(type_name), _) => self
                    .resolve_associated_member_value(type_name, &member_name, short_name)
                    .map(|callee| {
                        self.bind_member_receiver_if_callable(
                            callee,
                            &member_name,
                            &raw_receiver,
                            resolved_receiver.clone(),
                        )
                    }),
                RuntimeValue::Ref(owner) => self
                    .resolve_associated_member_value(owner, &member_name, short_name)
                    .or_else(|| {
                        let owner_short =
                            ParserText::get_temp_name_suffix(owner).unwrap_or(owner.to_string());
                        if &owner_short != owner {
                            self.resolve_associated_member_value(
                                &owner_short,
                                &member_name,
                                short_name,
                            )
                        } else {
                            None
                        }
                    }),
                _ => None,
            };
            resolved.unwrap_or(func)
        } else {
            func
        };

        let func = if let RuntimeValue::Function { name, .. } = &func
            && let Some((owner, member)) = name.rsplit_once(".")
            && let Some(first) = args.first()
            && let Ok(receiver) = self.resolve_value_ref(self.get_reg_value(*first))
            && let Some(receiver_type) = receiver.impl_name()
        {
            if self.callee_expects_receiver(&func)
                && !ParserText::temp_name_suffix_matches(&receiver_type, &owner)
                && let Some(resolved) =
                    self.resolve_associated_member_value(&receiver_type, member, Some(member))
                && resolved.is_callable()
            {
                resolved
            } else {
                func
            }
        } else {
            func
        };

        match func {
            RuntimeValue::BoundMethod { callee, receiver } => {
                let value = self.run_bound_method_call(
                    *callee,
                    receiver.as_ref().clone(),
                    args,
                    block,
                    ip,
                    dst.is_some(),
                )?;

                self.store_call_result(dst, value);
            }
            RuntimeValue::Function { name, captures } => {
                let func = self.resolve_vm_function(
                    name,
                    CallSite {
                        block: block.id.0 as usize,
                        tag: ip,
                    },
                )?;

                let refreshed = self.refresh_captures(captures.as_ref());
                let value = self.run_function_from_regs(
                    func.as_ref(),
                    args.iter().copied(),
                    refreshed,
                    dst.is_some(),
                )?;

                self.store_call_result(dst, value);

                return Ok(None);
            }
            RuntimeValue::NativeFunction(func) => {
                if let Some(step) =
                    self.handle_call_result(dst, &func, args, block, ip, prev_block)?
                {
                    return Ok(Some(step));
                }
            }
            #[cfg(feature = "native")]
            RuntimeValue::ExternFunction(func) => {
                let args = self.collect_call_args_vec(args);
                let value = func.call(self, &args)?;
                self.store_call_result(dst, value);
            }
            other => return Err(RuntimeError::InvalidFunctionCallValue(Box::new(other))),
        }

        let frame_idx = self.frames.len().saturating_sub(1);
        self.propagate_member_source_args(args, frame_idx)?;
        Ok(None)
    }

    #[inline]
    pub(crate) fn member_parts(name: &str) -> (Option<&str>, Option<usize>) {
        let short_name = name.rsplit_once(".").map(|(_, short)| short);
        let tuple_index = name.parse::<usize>().ok();
        (short_name, tuple_index)
    }

    #[inline]
    pub(crate) fn bind_member_receiver_if_callable(
        &mut self,
        callee: RuntimeValue,
        member_name: &str,
        raw_receiver: &RuntimeValue,
        resolved_receiver: RuntimeValue,
    ) -> RuntimeValue {
        if !self.callee_expects_receiver(&callee) {
            return callee;
        }

        if ParserText::is_temp_name(&member_name) {
            return callee.bind_if_callable(resolved_receiver);
        }

        let receiver = match raw_receiver {
            RuntimeValue::Ref(_) | RuntimeValue::VarRef(_) | RuntimeValue::RegRef { .. } => {
                raw_receiver.clone()
            }

            _ => resolved_receiver,
        };

        callee.bind_if_callable(receiver)
    }

    pub(crate) fn callee_expects_receiver(&mut self, callee: &RuntimeValue) -> bool {
        match callee {
            RuntimeValue::Function { name, .. } => {
                if let Some(func) = self.resolve_function_by_name(name) {
                    func.params
                        .first()
                        .map(|first| first == "self" || first.ends_with(":self"))
                        .unwrap_or(true)
                } else {
                    true
                }
            }
            RuntimeValue::BoundMethod { .. } => true,
            _ => true,
        }
    }
}
