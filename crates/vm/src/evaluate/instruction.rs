use super::*;
use crate::native::stdlib::generator::{GeneratorResumeFn, GeneratorState};
use calibre_parser::ast::{idents::ParserText, nodes::AsFailureMode};

impl VM {
    pub fn sync_local_reg_value(&mut self, frame_idx: usize, reg: u16, value: RuntimeValue) {
        let Some(frame) = self.frames.get(frame_idx) else {
            return;
        };

        let mut names = Vec::new();

        for (name, mapped) in frame.local_map.iter() {
            if *mapped == reg {
                names.push(name.clone());
            }
        }

        if let Some(base) = frame.local_map_base.as_ref() {
            for (name, mapped) in base.iter() {
                if *mapped == reg {
                    names.push(name.clone());
                }
            }
        }

        for name in names {
            if let Some(id) = self.global_id_cached(name.as_ref()) {
                let _ = self.variables.set_by_id(id, value.clone());
            } else {
                self.variables.insert(name.as_ref(), value.clone());
            }
        }
    }

    fn find_local_name_for_reg(&self, reg: u16) -> Option<Arc<str>> {
        let frame = self.current_frame();

        if let Some((name, _)) = frame.local_map.iter().find(|(_, mapped)| **mapped == reg) {
            return Some(name.clone());
        }

        if let Some(base) = frame.local_map_base.as_ref()
            && let Some((name, _)) = base.iter().find(|(_, mapped)| **mapped == reg)
        {
            return Some(name.clone());
        }

        None
    }

    fn find_local_reg_by_name(&self, name: &str) -> Option<(usize, u16)> {
        for (frame_idx, frame) in self.frames.iter().enumerate().rev() {
            if let Some(reg) = frame.local_map.get(name) {
                return Some((frame_idx, *reg));
            }

            if let Some(base) = frame.local_map_base.as_ref()
                && let Some(reg) = base.get(name)
            {
                return Some((frame_idx, *reg));
            }
        }

        None
    }
    #[inline]
    fn propagate_member_source_alias(&mut self, src: u16, dst: u16) {
        let source = self.current_frame().member_sources.get(&src).cloned();
        match source {
            Some(v) => {
                self.current_frame_mut().member_sources.insert(dst, v);
            }
            None => {
                self.current_frame_mut().member_sources.remove(&dst);
            }
        }
    }

    fn eval_branch_condition(
        &mut self,
        cond: u16,
        block: &VMBlock,
        ip: u32,
    ) -> Result<bool, RuntimeError> {
        if let RuntimeValue::Bool(v) = self.get_reg_value(cond) {
            return Ok(*v);
        }

        let resolved = self.resolve_value_for_op_ref(self.get_reg_value(cond))?;
        let value = if resolved.is_callable() {
            let mut callee = resolved;
            if let Some((source_reg, member_name)) =
                self.current_frame().member_sources.get(&cond).cloned()
            {
                let raw_receiver = self.get_reg_value(source_reg).clone();
                let resolved_receiver = self.resolve_value_for_op_ref(&raw_receiver)?;
                callee = self.bind_member_receiver_if_callable(
                    callee,
                    &member_name,
                    &raw_receiver,
                    resolved_receiver,
                    source_reg,
                );
            }
            self.call_runtime_callable_at(callee, Vec::new(), block.id.0 as usize, ip)?
        } else {
            resolved
        };

        match value {
            RuntimeValue::Bool(v) => Ok(v),
            other => Err(RuntimeError::UnexpectedType(other)),
        }
    }

    #[inline]
    fn member_parts(name: &str) -> (Option<&str>, Option<usize>) {
        let short_name = name.rsplit_once("::").map(|(_, short)| short);
        let tuple_index = name.parse::<usize>().ok();
        (short_name, tuple_index)
    }

    #[inline]
    fn bind_receiver_if_callable(callee: RuntimeValue, receiver: RuntimeValue) -> RuntimeValue {
        match callee {
            RuntimeValue::Function { .. }
            | RuntimeValue::NativeFunction(_)
            | RuntimeValue::ExternFunction(_) => RuntimeValue::BoundMethod {
                callee: Box::new(callee),
                receiver: Gc::new(receiver),
            },
            other => other,
        }
    }

    #[inline]
    fn reg_is_named_local(&self, reg: u16) -> bool {
        let frame = self.current_frame();
        frame
            .local_map
            .iter()
            .any(|(name, r)| *r == reg && ParserText::is_temp_name(name))
            || frame.local_map_base.as_ref().is_some_and(|m| {
                m.iter()
                    .any(|(name, r)| *r == reg && ParserText::is_temp_name(name))
            })
    }

    #[inline]
    fn bind_member_receiver_if_callable(
        &mut self,
        callee: RuntimeValue,
        member_name: &str,
        raw_receiver: &RuntimeValue,
        resolved_receiver: RuntimeValue,
        src_reg: u16,
    ) -> RuntimeValue {
        if !self.callee_expects_receiver(&callee) {
            return callee;
        }

        if let RuntimeValue::Ref(name) = raw_receiver
            && (!name.contains(':') || name.contains("->"))
            && let Some(ty) = self.concrete_runtime_type_name(&resolved_receiver)
            && ParserText::temp_name_prefix_matches(&ty, name)
        {
            return callee;
        }

        let frame_idx = self.frames.len().saturating_sub(1);

        if ParserText::is_temp_name(&member_name) {
            return Self::bind_receiver_if_callable(callee, resolved_receiver);
        }

        let receiver = match raw_receiver {
            RuntimeValue::RegRef { reg, .. } if !self.reg_is_named_local(*reg) => {
                if let Some(reg) = self.find_local_reg_for_value(&resolved_receiver) {
                    RuntimeValue::RegRef {
                        frame: frame_idx,
                        reg,
                    }
                } else {
                    resolved_receiver
                }
            }
            RuntimeValue::Ref(_) | RuntimeValue::VarRef(_) | RuntimeValue::RegRef { .. } => {
                raw_receiver.clone()
            }
            value if value.should_pass_by_reg_ref() => {
                if self.reg_is_named_local(src_reg) {
                    let local_name = self
                        .current_frame()
                        .local_map
                        .iter()
                        .find(|(name, reg)| **reg == src_reg && name.as_ref().contains(':'))
                        .map(|(name, _)| name.clone());
                    if let Some(name) = local_name
                        && let Some(id) = self.global_id_cached(name.as_ref())
                    {
                        RuntimeValue::VarRef(id)
                    } else {
                        RuntimeValue::RegRef {
                            frame: frame_idx,
                            reg: src_reg,
                        }
                    }
                } else {
                    RuntimeValue::RegRef {
                        frame: frame_idx,
                        reg: src_reg,
                    }
                }
            }
            value => {
                if let Some(reg) = self.find_local_reg_for_value(value) {
                    RuntimeValue::RegRef {
                        frame: frame_idx,
                        reg,
                    }
                } else {
                    resolved_receiver
                }
            }
        };
        Self::bind_receiver_if_callable(callee, receiver)
    }

    fn find_local_reg_for_value(&self, value: &RuntimeValue) -> Option<u16> {
        let frame = self.current_frame();
        let matches_reg = |reg: u16| match (value, self.get_reg_value(reg)) {
            (RuntimeValue::List(left), RuntimeValue::List(right)) => {
                std::ptr::eq(left.as_ref(), right.as_ref())
            }
            (RuntimeValue::Aggregate(_, left), RuntimeValue::Aggregate(_, right)) => {
                std::ptr::eq(left.as_ref(), right.as_ref())
            }
            (RuntimeValue::Str(left), RuntimeValue::Str(right)) => Arc::ptr_eq(left, right),
            _ => false,
        };

        if let Some((_, reg)) = frame.local_map.iter().find(|(_, reg)| matches_reg(**reg)) {
            return Some(*reg);
        }

        if let Some(base) = frame.local_map_base.as_ref()
            && let Some((_, reg)) = base.iter().find(|(_, reg)| matches_reg(**reg))
        {
            return Some(*reg);
        }

        None
    }

    fn callee_expects_receiver(&mut self, callee: &RuntimeValue) -> bool {
        match callee {
            RuntimeValue::Function { name, .. } => {
                if let Some(func) = self.resolve_function_by_name(name.as_str()) {
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

    #[inline]
    fn resolve_index(len: usize, idx: i64) -> Option<usize> {
        if len == 0 {
            return None;
        }

        let resolved = if idx < 0 { len as i64 + idx } else { idx };

        if resolved < 0 || resolved as usize >= len {
            None
        } else {
            Some(resolved as usize)
        }
    }

    #[inline]
    fn resolve_index_or_err(len: usize, idx: i64) -> Result<usize, RuntimeError> {
        Self::resolve_index(len, idx).ok_or(RuntimeError::StackUnderflow)
    }

    #[inline]
    fn resolve_slice_range(len: usize, start: i64, end: i64) -> (usize, usize) {
        let mut s = start;
        let mut e = end;
        if s < 0 {
            s = len as i64 + s;
        }
        if e < 0 {
            e = len as i64 + e;
        }
        if s < 0 {
            s = 0;
        }
        if e < 0 {
            e = 0;
        }
        let s = s.min(len as i64) as usize;
        let e = e.min(len as i64) as usize;
        if e < s { (s, s) } else { (s, e) }
    }

    fn materialize_callable_for_reg(&self, value: &RuntimeValue) -> Option<RuntimeValue> {
        match value {
            RuntimeValue::Function { name, captures } => Some(RuntimeValue::Function {
                name: name.clone(),
                captures: captures.clone(),
            }),
            RuntimeValue::NativeFunction(func) => Some(RuntimeValue::NativeFunction(func.clone())),
            RuntimeValue::ExternFunction(func) => Some(RuntimeValue::ExternFunction(func.clone())),
            RuntimeValue::BoundMethod { callee, receiver } => Some(RuntimeValue::BoundMethod {
                callee: callee.clone(),
                receiver: receiver.clone(),
            }),
            _ => None,
        }
    }

    fn run_bound_method_call(
        &mut self,
        callee: RuntimeValue,
        mut receiver: RuntimeValue,
        args: &[u16],
        block: &VMBlock,
        ip: u32,
    ) -> Result<RuntimeValue, RuntimeError> {
        let _ = self.resolve_value_for_op_ref(&receiver)?;

        if !receiver.is_ref_like()
            && receiver.should_pass_by_reg_ref()
            && let Some(reg) = self.find_local_reg_for_value(&receiver)
        {
            receiver = RuntimeValue::RegRef {
                frame: self.frames.len().saturating_sub(1),
                reg,
            };
        }

        let receiver_reg = if let RuntimeValue::RegRef { frame, reg } = &receiver {
            Some((*frame, *reg))
        } else {
            None
        };

        let mut full_args = vec![receiver];
        full_args.extend(self.collect_call_args_vec(args));
        let out = self.call_runtime_callable_at(callee, full_args, block.id.0 as usize, ip)?;

        if let Some((frame_idx, reg)) = receiver_reg {
            let current = self.frames.len().saturating_sub(1);
            if frame_idx == current {
                let mut source = self.current_frame().member_sources.get(&reg).cloned();

                if source.is_none()
                    && let RuntimeValue::List(target_list) = self.get_reg_value(reg).clone()
                {
                    let candidates: Vec<(u16, (u16, String))> = self
                        .current_frame()
                        .member_sources
                        .iter()
                        .map(|(k, v)| (*k, v.clone()))
                        .collect();

                    for (candidate_reg, candidate_source) in candidates {
                        if let RuntimeValue::List(other_list) =
                            self.get_reg_value(candidate_reg).clone()
                            && std::ptr::eq(other_list.as_ref(), target_list.as_ref())
                        {
                            source = Some(candidate_source);
                            break;
                        }
                    }
                }

                if let Some((parent_reg, member_name)) = source {
                    let updated_field = self.get_reg_value(reg).clone();
                    let parent_raw = self.get_reg_value(parent_reg).clone();
                    let parent_resolved = self.resolve_value_for_op_ref(&parent_raw)?;
                    if let RuntimeValue::Aggregate(type_name, mut map) = parent_resolved {
                        if let Some(entry) = Gc::make_mut(&mut map)
                            .0
                            .0
                            .iter_mut()
                            .find(|(field, _)| field == &member_name)
                        {
                            entry.1 = updated_field;
                            let updated_parent = RuntimeValue::Aggregate(type_name, map);
                            match parent_raw {
                                RuntimeValue::RegRef { frame, reg } => {
                                    self.set_reg_value_in_frame(frame, reg, updated_parent);
                                }
                                RuntimeValue::Ref(name) => {
                                    self.variables.insert(&name, updated_parent);
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
            }
        }
        Ok(out)
    }

    fn handle_call_result(
        &mut self,
        dst: u16,
        func: &Arc<dyn crate::NativeFunction>,
        args: &[u16],
        block: &VMBlock,
        ip: u32,
        prev_block: Option<BlockId>,
    ) -> Result<Option<TerminateValue>, RuntimeError> {
        let result = func.run(self, self.collect_call_args_vec(args))?;

        if let RuntimeValue::GeneratorSuspend(value) = result {
            let yielded = *value;
            self.set_reg_value(dst, yielded.clone());
            let frame_idx = self.frames.len().saturating_sub(1);
            self.propagate_member_source_args(args, frame_idx)?;

            return Ok(Some(TerminateValue::Yield {
                block: block.id,
                ip: ip as usize + 1,
                prev_block,
                yielded: Some(yielded),
            }));
        }

        self.set_reg_value(dst, result);
        Ok(None)
    }

    fn run_call_direct_instruction(
        &mut self,
        dst: u16,
        name: u16,
        args: &[u16],
        block: &VMBlock,
        ip: u32,
        prev_block: Option<BlockId>,
    ) -> Result<Option<TerminateValue>, RuntimeError> {
        let direct = self.resolve_direct_callsite_cached(block, ip, name)?;
        if let Some(PreparedDirectCall::Vm(func)) = &direct
            && let Some((owner, member)) = func.name.rsplit_once("::")
            && let Some(first) = args.first()
            && let Ok(receiver) = self.resolve_value_for_op_ref(self.get_reg_value(*first))
            && let Some(receiver_type) = self.concrete_runtime_type_name(&receiver)
        {
            if !ParserText::temp_name_prefix_matches(&receiver_type, &owner) {
                if let Some(resolved) =
                    self.resolve_associated_member_value(&receiver_type, member, Some(member))
                    && resolved.is_callable()
                {
                    let call_args = self.collect_call_args_vec(args);
                    let value = self.call_runtime_callable_at(
                        resolved,
                        call_args,
                        block.id.0 as usize,
                        ip,
                    )?;
                    self.set_reg_value(dst, value);
                    let frame_idx = self.frames.len().saturating_sub(1);
                    self.propagate_member_source_args(args, frame_idx)?;
                    return Ok(None);
                }
            }
        }

        match direct {
            Some(PreparedDirectCall::Vm(func)) => {
                let captures = if func.captures.is_empty() {
                    Self::empty_captures()
                } else {
                    let mut seen = FxHashSet::default();
                    std::sync::Arc::new(self.capture_values(&func.captures, &mut seen))
                };
                let value = self.run_function_from_regs(func.as_ref(), args, captures)?;
                self.set_reg_value(dst, value);
            }
            Some(PreparedDirectCall::Native(func)) => {
                if let Some(step) =
                    self.handle_call_result(dst, &func, args, block, ip, prev_block)?
                {
                    return Ok(Some(step));
                }
            }
            Some(PreparedDirectCall::Extern(func)) => {
                let value = func.call(self, self.collect_call_args_vec(args))?;
                self.set_reg_value(dst, value);
            }
            None => {
                let func_name = self.local_string(block, name)?;
                return Err(RuntimeError::FunctionNotFound(func_name.to_string()));
            }
        }
        let frame_idx = self.frames.len().saturating_sub(1);
        self.propagate_member_source_args(args, frame_idx)?;
        Ok(None)
    }

    fn run_call_instruction(
        &mut self,
        dst: u16,
        callee: u16,
        args: &[u16],
        block: &VMBlock,
        ip: u32,
        prev_block: Option<BlockId>,
    ) -> Result<Option<TerminateValue>, RuntimeError> {
        let func =
            if let Some(callable) = self.materialize_callable_for_reg(self.get_reg_value(callee)) {
                callable
            } else {
                self.resolve_value_for_op_ref(self.get_reg_value(callee))?
            };

        let func = if func.is_callable() {
            func
        } else if let Some((source_reg, member_name)) =
            self.current_frame().member_sources.get(&callee).cloned()
        {
            let (short_name, _) = Self::member_parts(&member_name);
            let raw_receiver = self.get_reg_value(source_reg).clone();
            let resolved_receiver = self
                .resolve_value_for_op_ref(&raw_receiver)
                .unwrap_or(func.clone());
            let resolved = match &resolved_receiver {
                RuntimeValue::Aggregate(Some(type_name), _) => {
                    if let Some(callee) =
                        self.resolve_associated_member_value(type_name, &member_name, short_name)
                    {
                        Some(self.bind_member_receiver_if_callable(
                            callee,
                            &member_name,
                            &raw_receiver,
                            resolved_receiver.clone(),
                            source_reg,
                        ))
                    } else {
                        None
                    }
                }
                RuntimeValue::Ref(owner) => self
                    .resolve_associated_member_value(owner, &member_name, short_name)
                    .or_else(|| {
                        let owner_short = ParserText::get_temp_name_prefix(owner).unwrap_or(owner.to_string());
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
            && let Some((owner, member)) = name.rsplit_once("::")
            && let Some(first) = args.first()
            && let Ok(receiver) = self.resolve_value_for_op_ref(self.get_reg_value(*first))
            && let Some(receiver_type) = self.concrete_runtime_type_name(&receiver)
        {
            if self.callee_expects_receiver(&func)
                && !ParserText::temp_name_prefix_matches(&receiver_type, &owner)
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
                )?;
                self.set_reg_value(dst, value);
            }
            RuntimeValue::Function { name, captures } => {
                let callsite = (self.current_frame().func_ptr, block.id.0 as usize, ip);

                let Some(func) = self.resolve_callable_cached(name.as_str(), callsite) else {
                    return Err(RuntimeError::FunctionNotFound(name.as_str().to_string()));
                };

                if name.ends_with("::next")
                    && let Some(first) = args.first()
                    && matches!(
                        self.resolve_value_for_op_ref(self.get_reg_value(*first))?,
                        RuntimeValue::Option(_)
                    )
                {
                    let value = self.resolve_value_for_op_ref(self.get_reg_value(*first))?;
                    self.set_reg_value(dst, value);
                    return Ok(None);
                }

                let mut owned_args: Option<Vec<u16>> = None;
                let mut use_args = args;

                if args.len() > func.params.len() {
                    let mut filtered = args.to_vec();

                    while filtered.len() > func.params.len() {
                        let drop_leading_invalid = filtered.first().is_some_and(|reg| {
                            self.current_frame()
                                .member_sources
                                .get(reg)
                                .is_some_and(|(_, name)| name == "<invalid>")
                        });

                        if drop_leading_invalid {
                            filtered.remove(0);
                        } else {
                            break;
                        }
                    }

                    if filtered.len() != args.len() {
                        owned_args = Some(filtered);
                    }
                }

                if let Some(ref vec) = owned_args {
                    use_args = vec.as_slice();
                }

                if captures.as_ref().is_empty()
                    && std::ptr::eq(
                        func.as_ref() as *const VMFunction,
                        self.current_frame().func_ptr as *const VMFunction,
                    )
                    && let Some(step) =
                        self.try_trampoline_self_tail_call(block, ip, dst, use_args, func.as_ref())
                {
                    return Ok(Some(step));
                }

                let mut seen = FxHashSet::default();
                let mut refreshed_caps = Vec::with_capacity(captures.len());
                let mut seen_names = FxHashSet::default();

                for (cap_name, old_value) in captures.iter() {
                    if !seen_names.insert(cap_name.clone()) {
                        continue;
                    }

                    let value = self.capture_value(cap_name, &mut seen);

                    let value = if value.is_null() && !old_value.is_null() {
                        old_value.clone()
                    } else {
                        value
                    };

                    refreshed_caps.push((cap_name.clone(), value));
                }

                let refreshed = Arc::new(refreshed_caps);
                let value = self.run_function_from_regs(func.as_ref(), use_args, refreshed)?;
                self.set_reg_value(dst, value);
                let frame_idx = self.frames.len().saturating_sub(1);
                self.propagate_member_source_args(use_args, frame_idx)?;
                return Ok(None);
            }
            RuntimeValue::NativeFunction(func) => {
                if let Some(step) =
                    self.handle_call_result(dst, &func, args, block, ip, prev_block)?
                {
                    return Ok(Some(step));
                }
            }
            RuntimeValue::ExternFunction(func) => {
                let value = func.call(self, self.collect_call_args_vec(args))?;
                self.set_reg_value(dst, value);
            }
            other => return Err(RuntimeError::InvalidFunctionCallValue(other)),
        }

        let frame_idx = self.frames.len().saturating_sub(1);
        self.propagate_member_source_args(args, frame_idx)?;
        Ok(None)
    }

    pub(super) fn run_instruction(
        &mut self,
        instruction: &VMInstruction,
        block: &VMBlock,
        ip: u32,
        prev_block: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        match instruction {
            VMInstruction::LoadLiteral { dst, literal } => {
                let lit = block
                    .local_literals
                    .get(*literal as usize)
                    .cloned()
                    .ok_or_else(|| RuntimeError::InvalidBytecode("missing literal".to_string()))?;
                match lit {
                    VMLiteral::Closure { label, captures } => {
                        let mut seen = FxHashSet::default();
                        let caps = self.capture_values(&captures, &mut seen);
                        self.set_reg_value(
                            *dst,
                            RuntimeValue::Function {
                                name: label.into(),
                                captures: Arc::new(caps),
                            },
                        );
                    }
                    VMLiteral::ExternFunction {
                        abi,
                        library,
                        symbol,
                        parameters,
                        return_type,
                    } => {
                        let abi_lower = abi.to_ascii_lowercase();
                        if abi_lower != "c" && abi_lower != "zig" {
                            return Err(RuntimeError::Ffi(format!("unsupported ABI \"{}\"", abi)));
                        }

                        let mut last_err = None;
                        let mut handle_opt = None;

                        for candidate in Self::resolve_library_candidates(&library) {
                            match unsafe { libloading::Library::new(&candidate) } {
                                Ok(h) => {
                                    handle_opt = Some(h);
                                    break;
                                }
                                Err(e) => last_err = Some(e.to_string()),
                            }
                        }

                        let handle = handle_opt.ok_or_else(|| {
                            RuntimeError::Ffi(format!(
                                "failed to load library {} ({})",
                                library,
                                last_err.unwrap_or_else(|| "no candidates".to_string())
                            ))
                        })?;

                        let func = crate::value::ExternFunction {
                            abi,
                            library,
                            symbol,
                            parameters,
                            return_type,
                            handle: Arc::new(handle),
                        };

                        self.set_reg_value(*dst, RuntimeValue::ExternFunction(Arc::new(func)));
                    }
                    other => {
                        self.set_reg_value(*dst, RuntimeValue::from(other));
                    }
                }
            }
            VMInstruction::LoadGlobal { dst, name } => {
                let name = self.local_string(block, *name)?;
                let is_local_style = ParserText::is_temp_name(&name);
                if is_local_style {
                    let value = match self.resolve_var_name(name) {
                        VarName::Var(var) => {
                            if let Some(id) = self.global_id_cached(&var) {
                                RuntimeValue::VarRef(id)
                            } else if let Some(v) = self.variables.get(&var) {
                                self.resolve_saveable_runtime_value_ref(v)
                            } else {
                                RuntimeValue::Null
                            }
                        }
                        VarName::Func(func) => {
                            if let Some(f) = self.get_function_ref(&func) {
                                self.make_runtime_function(f)
                            } else {
                                RuntimeValue::Null
                            }
                        }
                        VarName::Global => RuntimeValue::Null,
                    };
                    self.set_reg_value(*dst, value);
                    return Ok(TerminateValue::None);
                }

                if let Some(cache) = self
                    .caches
                    .globals_direct
                    .get(name)
                    .or_else(|| self.caches.globals.get(name))
                {
                    self.set_reg_value(*dst, cache.clone());
                    return Ok(TerminateValue::None);
                }

                let mut resolved_name: Option<String> = None;
                let mut value = if let Some((v, n)) = self.try_resolve_global_runtime_value(name) {
                    resolved_name = Some(n);
                    v
                } else {
                    match self.resolve_var_name(name) {
                        VarName::Func(func) => {
                            if let Some(f) = self.get_function_ref(&func) {
                                resolved_name = Some(func);
                                self.make_runtime_function(f)
                            } else {
                                RuntimeValue::Null
                            }
                        }
                        VarName::Var(var) => {
                            if self.variables.get(&var).is_none() {
                                let global = self.registry.globals.get(&var).cloned();
                                if let Some(global) = global {
                                    let _ = self.run_global(&global);
                                }
                            }

                            if let Some(v) = self.variables.get(&var) {
                                let resolved = var.as_str();
                                resolved_name = Some(var.clone());
                                if resolved.contains(':') || resolved.contains("->") {
                                    if let Some(id) = self.global_id_cached(resolved) {
                                        RuntimeValue::VarRef(id)
                                    } else {
                                        RuntimeValue::Ref(resolved.to_string())
                                    }
                                } else {
                                    self.resolve_saveable_runtime_value_ref(v)
                                }
                            } else {
                                RuntimeValue::Null
                            }
                        }
                        VarName::Global => RuntimeValue::Null,
                    }
                };

                if value.is_null()
                    && let Some((owner, method)) = name.rsplit_once("::")
                {
                    let owner = owner.rsplit(':').next().unwrap_or(owner);
                    let owner = ParserText::get_temp_name_prefix(&owner).unwrap_or_else(|| owner.to_string());
                    let owner = owner
                        .split_once("->")
                        .map(|(base, _)| base)
                        .unwrap_or(&owner);
                    let short_candidate = format!("{}_{}", owner.to_ascii_lowercase(), method);
                    let long_candidate = format!("async.{}", short_candidate);
                    if let Some((resolved, _)) = self
                        .try_resolve_global_runtime_value(&short_candidate)
                        .or_else(|| self.try_resolve_global_runtime_value(&long_candidate))
                    {
                        value = resolved;
                    }
                }

                if let Some(resolved_name) = resolved_name
                    && !value.is_null()
                {
                    self.caches.globals.insert(resolved_name, value.clone());
                }

                self.set_reg_value(*dst, value);
            }
            VMInstruction::MoveGlobal { dst, name } => {
                let name = self.local_string(block, *name)?;
                let resolved = self.resolve_var_name(name);
                let value =
                    self.try_move_global_runtime_value(name)
                        .unwrap_or_else(|| match &resolved {
                            VarName::Func(func) => {
                                if let Some(func) = self.take_function(func) {
                                    self.make_runtime_function(&func)
                                } else {
                                    RuntimeValue::Null
                                }
                            }
                            VarName::Var(var) => {
                                if let Some(var) = self.variables.remove(var) {
                                    self.resolve_saveable_runtime_value_ref(&var)
                                } else {
                                    RuntimeValue::Null
                                }
                            }
                            VarName::Global => RuntimeValue::Null,
                        });

                self.set_reg_value(*dst, value);
                self.invalidate_name_resolution_caches();
            }
            VMInstruction::DropGlobal { name } => {
                let name = self.local_string(block, *name)?;
                match self.resolve_var_name(name) {
                    VarName::Var(var) => {
                        self.caches.globals_id.remove(var.as_str());
                        if let Some(val) = self.variables.remove(&var) {
                            self.drop_runtime_value(val);
                        }
                    }
                    VarName::Func(func) => {
                        self.moved_functions.insert(func);
                    }
                    VarName::Global => {}
                }
                self.invalidate_name_resolution_caches();
            }
            VMInstruction::StoreGlobal { name, src } => {
                let name = self.local_string(block, *name)?;
                let mut value = self.get_reg_value(*src).clone();

                let is_local_style = ParserText::is_temp_name(&name);

                if is_local_style {
                    let interned = self.intern_name(name);
                    let frame = self.current_frame_mut();
                    frame.local_map.insert(interned, *src);
                    let frame_idx = self.frames.len().saturating_sub(1);
                    value = RuntimeValue::RegRef {
                        frame: frame_idx,
                        reg: *src,
                    };
                }

                let existed = self.variables.contains_key(name);

                if let Some(id) = self.global_id_cached(name) {
                    let _ = self.variables.set_by_id(id, value);
                } else {
                    let id = self.variables.insert_with_id(name, value);
                    self.caches.globals_id.insert(name.to_string(), id);
                }

                if !existed {
                    self.invalidate_name_resolution_caches();
                }
            }
            VMInstruction::SetLocalName { name, src } => {
                let full_name = self.local_string(block, *name)?;
                let interned = self.intern_local_string(block, *name)?;
                let frame = self.current_frame_mut();
                frame.local_map.insert(interned, *src);
                let frame_idx = self.frames.len().saturating_sub(1);
                let local_ref = RuntimeValue::RegRef {
                    frame: frame_idx,
                    reg: *src,
                };

                if let Some(id) = self.global_id_cached(full_name) {
                    let _ = self.variables.set_by_id(id, local_ref.clone());
                } else {
                    let id = self.variables.insert_with_id(full_name, local_ref.clone());
                    self.caches.globals_id.insert(full_name.to_string(), id);
                }
            }
            VMInstruction::LoadGlobalRef { dst, name } => {
                let name = self.local_string(block, *name)?;
                if let Some((frame, reg)) = self.find_local_reg_by_name(name) {
                    self.set_reg_value(*dst, RuntimeValue::RegRef { frame, reg });
                } else if let Some(id) = self.global_id_cached(name) {
                    if let Some(RuntimeValue::RegRef { frame, reg }) = self.variables.get_by_id(id)
                    {
                        self.set_reg_value(
                            *dst,
                            RuntimeValue::RegRef {
                                frame: *frame,
                                reg: *reg,
                            },
                        );
                    } else if let Some(value) = self.variables.get_by_id(id)
                        && value.should_pass_by_reg_ref()
                        && let Some(reg) = self.find_local_reg_for_value(value)
                    {
                        self.set_reg_value(
                            *dst,
                            RuntimeValue::RegRef {
                                frame: self.frames.len().saturating_sub(1),
                                reg,
                            },
                        );
                    } else {
                        self.set_reg_value(*dst, RuntimeValue::VarRef(id));
                    }
                } else {
                    self.set_reg_value(*dst, RuntimeValue::Ref(name.to_string()));
                }
            }
            VMInstruction::LoadRegRef { dst, src } => {
                let value = match self.get_reg_value(*src).clone() {
                    RuntimeValue::RegRef { frame, reg } => RuntimeValue::RegRef { frame, reg },
                    RuntimeValue::Ref(name) => RuntimeValue::Ref(name),
                    RuntimeValue::VarRef(id) => RuntimeValue::VarRef(id),
                    _ => RuntimeValue::RegRef {
                        frame: self.frames.len().saturating_sub(1),
                        reg: *src,
                    },
                };
                self.set_reg_value(*dst, value);
                self.propagate_member_source_alias(*src, *dst);
            }
            VMInstruction::Copy { dst, src } => {
                if dst == src {
                    return Ok(TerminateValue::None);
                }
                let value = self.get_reg_value(*src).clone();
                self.set_reg_value(*dst, value);
                self.propagate_member_source_alias(*src, *dst);
            }
            VMInstruction::As {
                dst,
                src,
                data_type,
                failure_mode,
            } => {
                let value = self.get_reg_value(*src).clone();
                let conversion = value.convert(self, &data_type.data_type);
                let converted = match failure_mode {
                    AsFailureMode::Panic => match conversion {
                        Ok(value) => value,
                        Err(err) => {
                            return Err(RuntimeError::Panic(Some(format!(
                                "failed `as!` conversion to {}: {}",
                                data_type, err
                            ))));
                        }
                    },
                    AsFailureMode::Option => match conversion {
                        Ok(value) => RuntimeValue::Option(Some(Gc::new(value))),
                        Err(_) => RuntimeValue::Option(None),
                    },
                    AsFailureMode::Result => match conversion {
                        Ok(value) => RuntimeValue::Result(Ok(Gc::new(value))),
                        Err(err) => RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(
                            Arc::new(err.to_string()),
                        )))),
                    },
                };
                self.set_reg_value(*dst, converted);
            }
            VMInstruction::Is {
                dst,
                src,
                data_type,
            } => {
                let value = self.get_reg_value(*src).clone();
                let resolved = self.resolve_operand_value(value)?;
                let out = self.runtime_matches_type(&resolved, &data_type.data_type);
                self.set_reg_value(*dst, RuntimeValue::Bool(out));
            }
            VMInstruction::Binary {
                dst,
                op,
                left,
                right,
            } => {
                let left = self.resolve_operand_value(self.get_reg_value(*left).clone())?;
                let right = self.resolve_operand_value(self.get_reg_value(*right).clone())?;
                let value = binary(self, op, left, right)?;
                self.set_reg_value(*dst, value);
            }
            VMInstruction::AccLoad { src } => {
                self.current_frame_mut().acc = self.get_reg_value(*src).clone();
            }
            VMInstruction::AccStore { dst } => {
                self.set_reg_value(*dst, self.current_frame().acc.clone());
            }
            VMInstruction::AccBinary { op, right } => {
                if let RuntimeValue::Int(left) = self.current_frame().acc.clone()
                    && let RuntimeValue::Int(right) = self.get_reg_value(*right).clone()
                    && let Some(value) = {
                        let out = match op {
                            BinaryOperator::Add => {
                                Some(RuntimeValue::Int(left.wrapping_add(right)))
                            }
                            BinaryOperator::Sub => {
                                Some(RuntimeValue::Int(left.wrapping_sub(right)))
                            }
                            BinaryOperator::Mul => {
                                Some(RuntimeValue::Int(left.wrapping_mul(right)))
                            }
                            BinaryOperator::Div => {
                                if right == 0 {
                                    None
                                } else {
                                    Some(RuntimeValue::Int(left / right))
                                }
                            }
                            BinaryOperator::Mod => {
                                if right == 0 {
                                    None
                                } else {
                                    Some(RuntimeValue::Int(left % right))
                                }
                            }
                            BinaryOperator::BitAnd => Some(RuntimeValue::Int(left & right)),
                            BinaryOperator::BitOr => Some(RuntimeValue::Int(left | right)),
                            BinaryOperator::BitXor => Some(RuntimeValue::Int(left ^ right)),
                            BinaryOperator::Shl => {
                                Some(RuntimeValue::Int(left.wrapping_shl(right as u32)))
                            }
                            BinaryOperator::Shr => {
                                Some(RuntimeValue::Int(left.wrapping_shr(right as u32)))
                            }
                            BinaryOperator::Pow => None,
                        };
                        out
                    }
                {
                    self.current_frame_mut().acc = value;
                    return Ok(TerminateValue::None);
                }
                let right = self.resolve_operand_value(self.get_reg_value(*right).clone())?;
                let left_raw = {
                    let frame = self.current_frame_mut();
                    std::mem::replace(&mut frame.acc, RuntimeValue::Null)
                };
                let left = self.resolve_operand_value(left_raw)?;
                let value = binary(self, op, left, right)?;
                self.current_frame_mut().acc = value;
            }
            VMInstruction::Comparison {
                dst,
                op,
                left,
                right,
            } => {
                let right = self.resolve_operand_value(self.get_reg_value(*right).clone())?;
                let left = self.resolve_operand_value(self.get_reg_value(*left).clone())?;
                let cmp_val = comparison(op, left, right)?;
                self.set_reg_value(*dst, cmp_val);
            }
            VMInstruction::Boolean {
                dst,
                op,
                left,
                right,
            } => {
                let right = self.resolve_operand_value(self.get_reg_value(*right).clone())?;
                let left = self.resolve_operand_value(self.get_reg_value(*left).clone())?;
                self.set_reg_value(*dst, boolean(op, left, right)?);
            }
            VMInstruction::Range {
                dst,
                from,
                to,
                inclusive,
            } => {
                let from = self.resolve_value_for_op_ref(self.get_reg_value(*from))?;
                let to = self.resolve_value_for_op_ref(self.get_reg_value(*to))?;
                let as_range_bound = |value: RuntimeValue| -> Result<i64, RuntimeError> {
                    match value {
                        RuntimeValue::Int(v) => Ok(v),
                        RuntimeValue::UInt(v) => Ok(v as i64),
                        RuntimeValue::Float(v) => Ok(v as i64),
                        RuntimeValue::Bool(v) => Ok(v as i64),
                        RuntimeValue::Char(v) => Ok(v as i64),
                        RuntimeValue::List(v) => Ok(v.as_ref().0.len() as i64),
                        RuntimeValue::Aggregate(_, v) => Ok(v.as_ref().0.0.len() as i64),
                        RuntimeValue::Str(v) => Ok(v.len() as i64),
                        RuntimeValue::Range(from, to) => Ok((to - from).max(0)),
                        other => Err(RuntimeError::UnexpectedType(other)),
                    }
                };
                let from = as_range_bound(from)?;
                let to = as_range_bound(to)?;
                let end = if *inclusive { to + 1 } else { to };
                self.set_reg_value(*dst, RuntimeValue::Range(from, end));
            }
            VMInstruction::List { dst, items } => {
                let values = items
                    .iter()
                    .map(|item| self.get_reg_value(*item).clone())
                    .collect();
                self.set_reg_value(
                    *dst,
                    RuntimeValue::List(Gc::new(crate::value::GcVec(values))),
                );
            }
            VMInstruction::Aggregate {
                dst,
                layout,
                fields,
            } => {
                let layout = block
                    .aggregate_layouts
                    .get(*layout as usize)
                    .ok_or_else(|| {
                        RuntimeError::InvalidBytecode("invalid aggregate layout".to_string())
                    })?;
                let mut entries = Vec::with_capacity(layout.members.len());
                for (name, reg) in layout.members.iter().zip(fields.iter()) {
                    let mut value = self.get_reg_value(*reg).clone();
                    if value.is_ref_like()
                        && let Ok(resolved) = self.resolve_value_for_op_ref(&value)
                    {
                        value = resolved;
                    }
                    entries.push((name.clone(), value));
                }
                if let Some(type_name) = layout.name.clone()
                    && Self::is_gen_type_name(&type_name)
                {
                    let next_fn = entries.iter().find_map(|(field, value)| {
                        let short = field.rsplit("::").next().unwrap_or(field.as_str());
                        (short == "data").then(|| value.clone())
                    });
                    if let Some(RuntimeValue::Function { name, captures }) = next_fn {
                        let resolved_caps: Vec<(String, RuntimeValue)> = captures
                            .iter()
                            .map(|(k, v)| {
                                let resolved = self
                                    .resolve_value_for_op_ref(v)
                                    .unwrap_or_else(|_| v.clone());
                                (k.clone(), resolved)
                            })
                            .collect();
                        let mut gen_vm = VM::new_shared(
                            self.registry.clone(),
                            self.mappings.clone(),
                            self.config.clone(),
                        );
                        gen_vm.variables = self.variables.clone();
                        gen_vm.ptr_heap = self.ptr_heap.clone();
                        gen_vm.moved_functions = self.moved_functions.clone();

                        self.set_reg_value(
                            *dst,
                            RuntimeValue::Generator {
                                type_name: Arc::new(type_name),
                                state: Arc::new(std::sync::Mutex::new(GeneratorState {
                                    vm: gen_vm,
                                    function_name: name,
                                    captures: std::sync::Arc::new(resolved_caps),
                                    task_state: crate::TaskState::default(),
                                    index: 0,
                                    completed: false,
                                })),
                            },
                        );
                        return Ok(TerminateValue::None);
                    }
                }

                self.set_reg_value(
                    *dst,
                    RuntimeValue::Aggregate(
                        layout.name.clone(),
                        Gc::new(crate::value::GcMap(ObjectMap(entries))),
                    ),
                );
            }
            VMInstruction::Enum {
                dst,
                name,
                variant,
                payload,
            } => {
                let name = self.local_string(block, *name)?;
                let payload = payload.map(|reg| Gc::new(self.get_reg_value(reg).clone()));
                self.set_reg_value(
                    *dst,
                    RuntimeValue::Enum(name.to_string(), *variant as usize, payload),
                );
            }
            VMInstruction::CallDirect { dst, name, args } => {
                if let Some(step) =
                    self.run_call_direct_instruction(*dst, *name, args, block, ip, prev_block)?
                {
                    return Ok(step);
                }
            }
            VMInstruction::CallSelf { dst, args } => {
                let func_ptr = self.current_frame().func_ptr as *const VMFunction;
                if func_ptr.is_null() {
                    return Err(RuntimeError::InvalidBytecode(
                        "missing current function frame".to_string(),
                    ));
                }
                let func = unsafe { &*func_ptr };
                if let Some(step) = self.try_trampoline_self_tail_call(block, ip, *dst, args, func)
                {
                    return Ok(step);
                }
                let value = self.run_function_from_regs(func, args, Self::empty_captures())?;
                self.set_reg_value(*dst, value);
            }
            VMInstruction::Call { dst, callee, args } => {
                if let Some(step) =
                    self.run_call_instruction(*dst, *callee, args, block, ip, prev_block)?
                {
                    return Ok(step);
                }
            }
            VMInstruction::Spawn { dst, callee } => {
                let resolved = self.resolve_value_for_op_ref(self.get_reg_value(*callee))?;
                let to_spawn = match resolved {
                    RuntimeValue::Function { name, captures } => {
                        let resolved_caps: Vec<(String, RuntimeValue)> = captures
                            .as_ref()
                            .iter()
                            .map(|(k, v)| {
                                let resolved = self
                                    .resolve_value_for_op_ref(&v)
                                    .unwrap_or_else(|_| RuntimeValue::Null);
                                let resolved = self.convert_runtime_var_into_saveable(resolved);
                                (k.clone(), resolved)
                            })
                            .collect();
                        RuntimeValue::Function {
                            name,
                            captures: std::sync::Arc::new(resolved_caps),
                        }
                    }
                    other => other,
                };
                let wg = Arc::new(WaitGroupInner::new());
                wg.count.store(1, std::sync::atomic::Ordering::Release);
                self.spawn_async_task(to_spawn, Some(wg.clone()));
                self.set_reg_value(*dst, RuntimeValue::WaitGroup(wg));
            }
            VMInstruction::LoadMember { dst, value, member } => {
                let source_reg = *value;
                let name = self.local_string(block, *member)?;
                let raw_receiver = self.get_reg_value(*value).clone();
                if name == "<invalid>" {
                    let resolved = self.resolve_value_for_op_ref(&raw_receiver)?;
                    self.set_reg_value(*dst, resolved);
                    self.current_frame_mut()
                        .member_sources
                        .insert(*dst, (source_reg, name.to_string()));
                    return Ok(TerminateValue::None);
                }
                let (short_name, tuple_index) = Self::member_parts(name);
                let mut resolved = self.resolve_value_for_op_ref(&raw_receiver)?;
                if matches!(resolved, RuntimeValue::Null) {
                    if let Some(owner) = self.find_local_name_for_reg(source_reg)
                        && !owner.contains(':')
                        && !owner.contains("->")
                        && let Some(callee) =
                            self.resolve_associated_member_value(owner.as_ref(), name, short_name)
                    {
                        self.set_reg_value(*dst, callee);
                        self.current_frame_mut()
                            .member_sources
                            .insert(*dst, (source_reg, name.to_string()));
                        return Ok(TerminateValue::None);
                    }
                    if let RuntimeValue::Ref(owner) = &raw_receiver {
                        if let Some(callee) =
                            self.resolve_associated_member_value(owner, name, short_name)
                        {
                            self.set_reg_value(*dst, callee);
                            self.current_frame_mut()
                                .member_sources
                                .insert(*dst, (source_reg, name.to_string()));
                            return Ok(TerminateValue::None);
                        }
                    }
                }
                let member_short = short_name.unwrap_or(name);
                let bind_assoc = |vm: &mut VM,
                                  type_name: &str,
                                  value: RuntimeValue|
                 -> Result<RuntimeValue, RuntimeError> {
                    if let Some(callee) =
                        vm.resolve_associated_member_value(type_name, name, short_name)
                    {
                        Ok(vm.bind_member_receiver_if_callable(
                            callee,
                            name,
                            &raw_receiver,
                            value,
                            source_reg,
                        ))
                    } else {
                        Err(RuntimeError::MissingMember {
                            target: value,
                            member: name.to_string(),
                        })
                    }
                };
                for _ in 0..4 {
                    match &resolved {
                        RuntimeValue::Result(Ok(inner)) if member_short == "next" => {
                            self.set_reg_value(*dst, inner.as_ref().clone());
                            return Ok(TerminateValue::None);
                        }
                        RuntimeValue::Result(Ok(inner)) => {
                            resolved = inner.as_ref().clone();
                        }
                        _ => break,
                    }
                }
                let mut member_source: Option<(u16, String)> = None;
                let val = match resolved {
                    RuntimeValue::Generator { type_name, state } => match member_short {
                        "data" | "next" => {
                            RuntimeValue::NativeFunction(Arc::new(GeneratorResumeFn {
                                state: state.clone(),
                            }))
                        }
                        "index" => {
                            let guard = state
                                .lock()
                                .map_err(|_| RuntimeError::UnexpectedType(RuntimeValue::Null))?;
                            RuntimeValue::Int(guard.index)
                        }
                        "done" => {
                            let guard = state
                                .lock()
                                .map_err(|_| RuntimeError::UnexpectedType(RuntimeValue::Null))?;
                            RuntimeValue::Bool(guard.completed)
                        }
                        _ => match self.resolve_associated_member_value(
                            type_name.as_str(),
                            name,
                            short_name,
                        ) {
                            Some(value) => value,
                            None => {
                                return Err(RuntimeError::MissingMember {
                                    target: RuntimeValue::Generator { type_name, state },
                                    member: name.to_string(),
                                });
                            }
                        },
                    },
                    RuntimeValue::DynObject {
                        type_name,
                        value,
                        vtable,
                        constraints,
                    } => {
                        let member_short = short_name.unwrap_or(name);
                        if let Some(callee_name) =
                            vtable.get(member_short).or_else(|| vtable.get(name))
                        {
                            let mapped = callee_name.rsplit_once("::").and_then(|(owner, _)| {
                                if ParserText::temp_name_prefix_matches(&owner, &type_name)
                                {
                                    Some(callee_name.as_str())
                                } else {
                                    None
                                }
                            });
                            let Some(callee) = self.resolve_dyn_method_callable(
                                type_name.as_str(),
                                member_short,
                                mapped,
                            ) else {
                                return Err(RuntimeError::FunctionNotFound(callee_name.clone()));
                            };
                            Self::bind_receiver_if_callable(callee, value.as_ref().clone())
                        } else if let Some(callee) =
                            self.resolve_dyn_method_callable(type_name.as_str(), member_short, None)
                        {
                            Self::bind_receiver_if_callable(callee, value.as_ref().clone())
                        } else if member_short == "type" {
                            RuntimeValue::Str(type_name)
                        } else if member_short == "traits" {
                            RuntimeValue::List(Gc::new(crate::value::GcVec(
                                constraints
                                    .iter()
                                    .map(|x| RuntimeValue::Str(Arc::new(x.clone())))
                                    .collect(),
                            )))
                        } else {
                            return Err(RuntimeError::MissingMember {
                                target: RuntimeValue::DynObject {
                                    type_name,
                                    constraints,
                                    value,
                                    vtable,
                                },
                                member: name.to_string(),
                            });
                        }
                    }
                    RuntimeValue::Aggregate(None, map) => {
                        let idx =
                            tuple_index.ok_or(RuntimeError::UnexpectedType(RuntimeValue::Null))?;
                        if let Some((_, value)) = map.as_ref().0.0.get(idx) {
                            value.clone()
                        } else {
                            return Err(RuntimeError::MissingMember {
                                target: RuntimeValue::Aggregate(None, map),
                                member: name.to_string(),
                            });
                        }
                    }
                    RuntimeValue::Aggregate(Some(type_name), map) => {
                        if let Some(idx) =
                            self.resolve_aggregate_member_slot(&type_name, &map, name, short_name)
                        {
                            member_source = Some((source_reg, map.0.0[idx].0.clone()));
                            let field_value = map.0.0[idx].1.clone();
                            field_value
                        } else {
                            match self.resolve_associated_member_value(
                                type_name.as_str(),
                                name,
                                short_name,
                            ) {
                                Some(value) => {
                                    let resolved_receiver = RuntimeValue::Aggregate(
                                        Some(type_name.clone()),
                                        map.clone(),
                                    );
                                    self.bind_member_receiver_if_callable(
                                        value,
                                        name,
                                        &raw_receiver,
                                        resolved_receiver,
                                        source_reg,
                                    )
                                }
                                None => {
                                    return Err(RuntimeError::MissingMember {
                                        target: RuntimeValue::Aggregate(Some(type_name), map),
                                        member: name.to_string(),
                                    });
                                }
                            }
                        }
                    }
                    RuntimeValue::Enum(_, _, Some(x)) if name == "next" || name == "0" => {
                        x.as_ref().clone()
                    }
                    RuntimeValue::Enum(_, _, Some(x)) => x.as_ref().clone(),
                    RuntimeValue::Enum(_, _, None) if name == "next" || name == "0" => {
                        RuntimeValue::Null
                    }
                    RuntimeValue::Option(Some(x)) if name == "next" || name == "0" => {
                        x.as_ref().clone()
                    }
                    RuntimeValue::Option(Some(inner)) if !(name == "next" || name == "0") => {
                        if let Some(callee) =
                            self.resolve_associated_member_value("T?", name, short_name)
                        {
                            self.bind_member_receiver_if_callable(
                                callee,
                                name,
                                &raw_receiver,
                                RuntimeValue::Option(Some(inner.clone())),
                                source_reg,
                            )
                        } else {
                            let mut inner_value =
                                self.resolve_value_for_op_ref(&inner.as_ref().clone())?;
                            loop {
                                match inner_value.clone() {
                                    RuntimeValue::Option(Some(nested)) => {
                                        inner_value = self
                                            .resolve_value_for_op_ref(&nested.as_ref().clone())?;
                                    }
                                    _ => break,
                                }
                            }
                            match inner_value.clone() {
                                RuntimeValue::Aggregate(type_name, map) => {
                                    if let Some(idx) = self.resolve_aggregate_member_slot(
                                        type_name.as_deref().unwrap_or_default(),
                                        &map,
                                        name,
                                        short_name,
                                    ) {
                                        map.0.0[idx].1.clone()
                                    } else if let Some(callee) = self
                                        .resolve_associated_member_value(
                                            type_name.as_deref().unwrap_or("T"),
                                            name,
                                            short_name,
                                        )
                                    {
                                        self.bind_member_receiver_if_callable(
                                            callee,
                                            name,
                                            &inner_value,
                                            inner_value.clone(),
                                            source_reg,
                                        )
                                    } else {
                                        return Err(RuntimeError::MissingMember {
                                            target: RuntimeValue::Option(Some(inner)),
                                            member: name.to_string(),
                                        });
                                    }
                                }
                                other => {
                                    return Err(RuntimeError::MissingMember {
                                        target: RuntimeValue::Option(Some(Gc::new(other))),
                                        member: name.to_string(),
                                    });
                                }
                            }
                        }
                    }
                    RuntimeValue::Option(None) if name == "next" || name == "0" => {
                        RuntimeValue::Null
                    }
                    option @ RuntimeValue::Option(_) => {
                        if let Some(callee) =
                            self.resolve_associated_member_value("T?", name, short_name)
                        {
                            self.bind_member_receiver_if_callable(
                                callee,
                                name,
                                &raw_receiver,
                                option,
                                source_reg,
                            )
                        } else {
                            return Err(RuntimeError::MissingMember {
                                target: option,
                                member: name.to_string(),
                            });
                        }
                    }
                    RuntimeValue::Result(Ok(x)) if name == "next" || name == "0" => {
                        x.as_ref().clone()
                    }
                    RuntimeValue::Result(Err(x)) if name == "next" || name == "0" => {
                        x.as_ref().clone()
                    }
                    result @ RuntimeValue::Result(_) => {
                        if let Some(callee) =
                            self.resolve_associated_member_value("E!T", name, short_name)
                        {
                            self.bind_member_receiver_if_callable(
                                callee,
                                name,
                                &raw_receiver,
                                result,
                                source_reg,
                            )
                        } else {
                            return Err(RuntimeError::MissingMember {
                                target: result,
                                member: name.to_string(),
                            });
                        }
                    }
                    RuntimeValue::Ptr(id) if name == "next" || name == "0" => {
                        self.ptr_heap.get(&id).cloned().unwrap_or_default()
                    }
                    RuntimeValue::Channel(_) if member_short == "raw_send" => {
                        RuntimeValue::BoundMethod {
                            callee: Box::new(RuntimeValue::NativeFunction(Arc::new(
                                crate::native::stdlib::r#async::ChannelSend(),
                            ))),
                            receiver: Gc::new(raw_receiver.clone()),
                        }
                    }
                    RuntimeValue::Char(value) => {
                        bind_assoc(self, "char", RuntimeValue::Char(value))?
                    }
                    RuntimeValue::Str(value) => bind_assoc(self, "str", RuntimeValue::Str(value))?,
                    RuntimeValue::List(value) => {
                        bind_assoc(self, "list", RuntimeValue::List(value))?
                    }
                    RuntimeValue::Int(value) => bind_assoc(self, "int", RuntimeValue::Int(value))?,
                    RuntimeValue::UInt(value) => {
                        bind_assoc(self, "uint", RuntimeValue::UInt(value))?
                    }
                    RuntimeValue::Float(value) => {
                        bind_assoc(self, "float", RuntimeValue::Float(value))?
                    }
                    RuntimeValue::Bool(value) => {
                        bind_assoc(self, "bool", RuntimeValue::Bool(value))?
                    }
                    RuntimeValue::Null => {
                        return Err(RuntimeError::MissingMember {
                            target: RuntimeValue::Null,
                            member: name.to_string(),
                        });
                    }
                    other => {
                        if let Some(type_name) = self.concrete_runtime_type_name(&other) {
                            bind_assoc(self, type_name.as_str(), other)?
                        } else {
                            return Err(RuntimeError::UnexpectedType(other));
                        }
                    }
                };
                self.set_reg_value(*dst, val);
                match member_source {
                    Some((parent, field)) => {
                        self.current_frame_mut()
                            .member_sources
                            .insert(*dst, (parent, field));
                    }
                    None => {
                        self.current_frame_mut()
                            .member_sources
                            .insert(*dst, (source_reg, name.to_string()));
                    }
                }
            }
            VMInstruction::SetMember {
                target,
                member,
                value,
            } => {
                let name = self.local_string(block, *member)?;
                let value = self.get_reg_value(*value).clone();
                let (short_name, tuple_index) = Self::member_parts(name);

                let update_aggregate =
                    |agg_name: &Option<String>, mut map: Gc<crate::value::GcMap>| {
                        let entries = &mut Gc::make_mut(&mut map).0.0;
                        match (agg_name.as_ref(), tuple_index) {
                            (None, Some(idx)) => {
                                if idx >= entries.len() {
                                    return Err(RuntimeError::StackUnderflow);
                                }
                                entries[idx].1 = value.clone();
                            }
                            (Some(_), _) => {
                                if let Some(entry) = entries.iter_mut().find(|entry| {
                                    entry.0 == *name
                                        || short_name.is_some_and(|short| entry.0 == short)
                                }) {
                                    entry.1 = value.clone();
                                } else {
                                    return Err(RuntimeError::StackUnderflow);
                                }
                            }
                            _ => return Err(RuntimeError::UnexpectedType(RuntimeValue::Null)),
                        }
                        Ok(map)
                    };
                let update_generator =
                    |generator_value: RuntimeValue| -> Result<RuntimeValue, RuntimeError> {
                        let RuntimeValue::Generator { type_name, state } = generator_value else {
                            return Err(RuntimeError::UnexpectedType(generator_value));
                        };

                        let member_key = short_name.unwrap_or(name);
                        if !matches!(member_key, "done" | "index") {
                            return Err(RuntimeError::MissingMember {
                                target: RuntimeValue::Generator { type_name, state },
                                member: name.to_string(),
                            });
                        }

                        let mut guard = state
                            .lock()
                            .map_err(|_| RuntimeError::UnexpectedType(RuntimeValue::Null))?;
                        match member_key {
                            "index" => match &value {
                                RuntimeValue::Int(x) => guard.index = (*x).max(0),
                                RuntimeValue::UInt(x) => guard.index = *x as i64,
                                other => {
                                    return Err(RuntimeError::UnexpectedType((*other).clone()));
                                }
                            },
                            "done" => match &value {
                                RuntimeValue::Bool(x) => guard.completed = *x,
                                other => {
                                    return Err(RuntimeError::UnexpectedType((*other).clone()));
                                }
                            },
                            _ => {}
                        }
                        drop(guard);

                        Ok(RuntimeValue::Generator { type_name, state })
                    };

                let mut target_value = self.get_reg_value(*target).clone();
                let mut handled = false;

                for _ in 0..64 {
                    match target_value {
                        RuntimeValue::Ref(ref_name) => {
                            let current =
                                if let Some(value) = self.variables.get(&ref_name).cloned() {
                                    value
                                } else if let Some((frame, reg)) =
                                    self.find_local_reg_by_name(&ref_name)
                                {
                                    RuntimeValue::RegRef { frame, reg }
                                } else {
                                    return Err(RuntimeError::DanglingRef(ref_name.clone()));
                                };
                            match current {
                                RuntimeValue::Ref(_)
                                | RuntimeValue::VarRef(_)
                                | RuntimeValue::RegRef { .. } => {
                                    target_value = current;
                                    continue;
                                }
                                RuntimeValue::Aggregate(name, map) => {
                                    let updated = update_aggregate(&name, map)?;
                                    self.variables
                                        .insert(&ref_name, RuntimeValue::Aggregate(name, updated));
                                }
                                RuntimeValue::List(_list) => {
                                    self.variables.insert(&ref_name, value);
                                }
                                RuntimeValue::Generator { .. } => {
                                    self.variables.insert(&ref_name, update_generator(current)?);
                                }
                                other => return Err(RuntimeError::UnexpectedType(other)),
                            }
                            handled = true;
                            break;
                        }
                        RuntimeValue::VarRef(id) => {
                            let current = self
                                .variables
                                .get_by_id(id)
                                .cloned()
                                .ok_or(RuntimeError::DanglingRef(format!("#{}", id)))?;
                            match current {
                                RuntimeValue::Ref(_)
                                | RuntimeValue::VarRef(_)
                                | RuntimeValue::RegRef { .. } => {
                                    target_value = current;
                                    continue;
                                }
                                RuntimeValue::Aggregate(name, map) => {
                                    let updated = update_aggregate(&name, map)?;
                                    let _ = self
                                        .variables
                                        .set_by_id(id, RuntimeValue::Aggregate(name, updated));
                                }
                                RuntimeValue::List(_list) => {
                                    let _ = self.variables.set_by_id(id, value);
                                }
                                RuntimeValue::Generator { .. } => {
                                    let _ =
                                        self.variables.set_by_id(id, update_generator(current)?);
                                }
                                other => return Err(RuntimeError::UnexpectedType(other)),
                            }
                            handled = true;
                            break;
                        }
                        RuntimeValue::RegRef { frame, reg } => {
                            let current = self.get_reg_value_in_frame(frame, reg).clone();
                            match current {
                                RuntimeValue::Ref(_)
                                | RuntimeValue::VarRef(_)
                                | RuntimeValue::RegRef { .. } => {
                                    target_value = current;
                                    continue;
                                }
                                RuntimeValue::Aggregate(name, map) => {
                                    let updated = update_aggregate(&name, map)?;
                                    let member_source = self
                                        .frames
                                        .get(frame)
                                        .and_then(|vm_frame| vm_frame.member_sources.get(&reg))
                                        .cloned();

                                    self.set_reg_value_in_frame(
                                        frame,
                                        reg,
                                        RuntimeValue::Aggregate(name, updated),
                                    );

                                    if let Some(source) = member_source
                                        && let Some(vm_frame) = self.frames.get_mut(frame)
                                    {
                                        vm_frame.member_sources.insert(reg, source);
                                    }

                                    self.propagate_member_source_reg(reg, frame)?;
                                }
                                RuntimeValue::List(_) => {
                                    if let Some((parent_reg, field_name)) =
                                        self.current_frame().member_sources.get(&reg).cloned()
                                    {
                                        self.write_back_member_field_update(
                                            frame,
                                            reg,
                                            parent_reg,
                                            &field_name,
                                        )?;
                                    }
                                }
                                RuntimeValue::Generator { .. } => {
                                    self.set_reg_value_in_frame(
                                        frame,
                                        reg,
                                        update_generator(current)?,
                                    );
                                }
                                other => return Err(RuntimeError::UnexpectedType(other)),
                            }
                            handled = true;
                            break;
                        }
                        RuntimeValue::Aggregate(name, map) => {
                            let updated = update_aggregate(&name, map)?;
                            let member_source =
                                self.current_frame().member_sources.get(target).cloned();
                            self.set_reg_value(*target, RuntimeValue::Aggregate(name, updated));
                            if let Some(source) = member_source {
                                self.current_frame_mut()
                                    .member_sources
                                    .insert(*target, source);
                            }
                            self.propagate_member_source_reg(
                                *target,
                                self.frames.len().saturating_sub(1),
                            )?;
                            handled = true;
                            break;
                        }
                        RuntimeValue::List(_) => {
                            if let Some((parent_reg, field_name)) =
                                self.current_frame().member_sources.get(target).cloned()
                            {
                                self.write_back_member_field_update(
                                    self.frames.len().saturating_sub(1),
                                    *target,
                                    parent_reg,
                                    &field_name,
                                )?;
                            }
                            handled = true;
                            break;
                        }
                        current @ RuntimeValue::Generator { .. } => {
                            self.set_reg_value(*target, update_generator(current)?);
                            handled = true;
                            break;
                        }
                        other => return Err(RuntimeError::UnexpectedType(other)),
                    }
                }

                if !handled {
                    return Err(RuntimeError::DanglingRef(
                        "<set-member-depth-limit>".to_string(),
                    ));
                }
            }
            VMInstruction::Index { dst, value, index } => {
                let value_ref = self.get_reg_value(*value);
                let mut index_val = self.get_reg_value(*index).clone();

                if index_val.is_ref_like() {
                    index_val = self.resolve_value_for_op_ref(&index_val)?;
                }

                if let RuntimeValue::List(list) = value_ref {
                    let idx = match &index_val {
                        RuntimeValue::UInt(i) => Some(*i as usize),
                        RuntimeValue::Int(i) if *i >= 0 => Some(*i as usize),
                        _ => None,
                    };
                    if let Some(idx) = idx {
                        let out = list.as_ref().0.get(idx).cloned();
                        let out = out.unwrap_or_default();
                        self.set_reg_value(*dst, out);
                        if let Some(source) =
                            self.current_frame().member_sources.get(value).cloned()
                        {
                            self.current_frame_mut().member_sources.insert(*dst, source);
                        }
                        return Ok(TerminateValue::None);
                    }
                }

                let index_list =
                    |list: &Gc<crate::value::GcVec>| -> Result<RuntimeValue, RuntimeError> {
                        match &index_val {
                            RuntimeValue::Int(index) => {
                                Ok(Self::resolve_index(list.as_ref().0.len(), *index)
                                    .and_then(|i| list.as_ref().0.get(i).cloned())
                                    .unwrap_or_else(|| RuntimeValue::Null))
                            }
                            RuntimeValue::UInt(index) => Ok(list
                                .as_ref()
                                .0
                                .get(*index as usize)
                                .cloned()
                                .unwrap_or_else(|| RuntimeValue::Null)),
                            RuntimeValue::Range(start, end) => {
                                let (s, e) =
                                    Self::resolve_slice_range(list.as_ref().0.len(), *start, *end);
                                let slice = list.as_ref().0[s..e].to_vec();
                                Ok(RuntimeValue::List(Gc::new(crate::value::GcVec(slice))))
                            }
                            _ => Err(RuntimeError::UnexpectedType(RuntimeValue::Null)),
                        }
                    };

                let index_map = |map: &std::sync::Arc<
                    std::sync::Mutex<rustc_hash::FxHashMap<crate::value::HashKey, RuntimeValue>>,
                >|
                 -> Result<RuntimeValue, RuntimeError> {
                    let key = crate::value::HashKey::try_from(index_val.clone())?;
                    let guard = map
                        .lock()
                        .map_err(|_| RuntimeError::UnexpectedType(RuntimeValue::Null))?;
                    Ok(guard.get(&key).cloned().unwrap_or(RuntimeValue::Null))
                };

                let resolved = self.resolve_value_for_op_ref(self.get_reg_value(*value))?;
                let val = match resolved {
                    RuntimeValue::List(list) => index_list(&list)?,
                    RuntimeValue::HashMap(map) => index_map(&map)?,
                    RuntimeValue::Range(start, end) => match &index_val {
                        RuntimeValue::Int(index) => {
                            let len = (end - start).max(0) as usize;
                            Self::resolve_index(len, *index)
                                .map(|i| RuntimeValue::Int(start + i as i64))
                                .unwrap_or_else(|| RuntimeValue::Null)
                        }
                        RuntimeValue::UInt(index) => {
                            let len = (end - start).max(0) as usize;
                            if (*index as usize) < len {
                                RuntimeValue::Int(start + *index as i64)
                            } else {
                                RuntimeValue::Null
                            }
                        }
                        RuntimeValue::Range(slice_start, slice_end) => {
                            let len = (end - start).max(0) as usize;
                            let (s, e) = Self::resolve_slice_range(len, *slice_start, *slice_end);
                            RuntimeValue::Range(start + s as i64, start + e as i64)
                        }
                        _ => return Err(RuntimeError::UnexpectedType(RuntimeValue::Null)),
                    },
                    RuntimeValue::Aggregate(None, tuple) => match &index_val {
                        RuntimeValue::Int(index) => {
                            Self::resolve_index(tuple.as_ref().0.0.len(), *index)
                                .and_then(|i| tuple.as_ref().0.0.get(i).map(|(_, v)| v.clone()))
                                .unwrap_or_else(|| RuntimeValue::Null)
                        }
                        RuntimeValue::UInt(index) => tuple
                            .as_ref()
                            .0
                            .0
                            .get(*index as usize)
                            .map(|(_, v)| v.clone())
                            .unwrap_or_else(|| RuntimeValue::Null),
                        RuntimeValue::Range(start, end) => {
                            let (s, e) =
                                Self::resolve_slice_range(tuple.as_ref().0.0.len(), *start, *end);
                            let slice = tuple.as_ref().0.0[s..e].to_vec();
                            RuntimeValue::Aggregate(
                                None,
                                Gc::new(crate::value::GcMap(ObjectMap(slice))),
                            )
                        }
                        _ => return Err(RuntimeError::UnexpectedType(RuntimeValue::Null)),
                    },
                    RuntimeValue::Str(s) => match &index_val {
                        RuntimeValue::Int(index) => {
                            let resolved = if *index < 0 {
                                let len = s.chars().count();
                                Self::resolve_index(len, *index)
                            } else {
                                Some(*index as usize)
                            };
                            resolved
                                .and_then(|i| s.chars().nth(i))
                                .map(RuntimeValue::Char)
                                .unwrap_or_else(|| RuntimeValue::Null)
                        }
                        RuntimeValue::UInt(index) => s
                            .chars()
                            .nth(*index as usize)
                            .map(RuntimeValue::Char)
                            .unwrap_or_else(|| RuntimeValue::Null),
                        RuntimeValue::Range(start, end) => {
                            let v = s.chars().collect::<Vec<char>>();
                            let (s, e) = Self::resolve_slice_range(v.len(), *start, *end);
                            let slice: String = v[s..e].iter().collect();
                            RuntimeValue::Str(Arc::new(slice))
                        }
                        _ => return Err(RuntimeError::UnexpectedType(RuntimeValue::Null)),
                    },
                    RuntimeValue::Enum(_, _, Some(x)) => x.as_ref().clone(),
                    RuntimeValue::Option(Some(x)) => x.as_ref().clone(),
                    RuntimeValue::Result(Ok(x)) => x.as_ref().clone(),
                    RuntimeValue::Result(Err(x)) => x.as_ref().clone(),
                    other => return Err(RuntimeError::UnexpectedType(other)),
                };

                self.set_reg_value(*dst, val);
                if !self.current_frame().member_sources.contains_key(dst) {
                    self.propagate_member_source_alias(*value, *dst);
                }
            }
            VMInstruction::SetIndex {
                target,
                index,
                value,
            } => {
                let mut index_val = self.get_reg_value(*index).clone();

                if index_val.is_ref_like() {
                    index_val = self.resolve_value_for_op_ref(&index_val)?;
                }

                let value = self.get_reg_value(*value).clone();
                let numeric_index = || match index_val.clone() {
                    RuntimeValue::Int(index) => Ok(index),
                    RuntimeValue::UInt(index) => Ok(index as i64),
                    _ => Err(RuntimeError::UnexpectedType(RuntimeValue::Null)),
                };

                let hash_index = || crate::value::HashKey::try_from(index_val.clone());
                let mut target_value = self.get_reg_value(*target).clone();
                let mut handled = false;

                for _ in 0..64 {
                    match target_value {
                        RuntimeValue::Ref(ref_name) => {
                            let current =
                                if let Some(value) = self.variables.get(&ref_name).cloned() {
                                    value
                                } else if let Some((frame, reg)) =
                                    self.find_local_reg_by_name(&ref_name)
                                {
                                    RuntimeValue::RegRef { frame, reg }
                                } else {
                                    return Err(RuntimeError::DanglingRef(ref_name.clone()));
                                };
                            match current {
                                RuntimeValue::Ref(_)
                                | RuntimeValue::VarRef(_)
                                | RuntimeValue::RegRef { .. } => {
                                    target_value = current;
                                    continue;
                                }
                                RuntimeValue::List(mut list) => {
                                    let index = numeric_index()?;
                                    if index < 0 {
                                        return Err(RuntimeError::UnexpectedType(
                                            RuntimeValue::Null,
                                        ));
                                    }

                                    let vec = &mut Gc::make_mut(&mut list).0;
                                    let idx = Self::resolve_index_or_err(vec.len(), index)?;
                                    vec[idx] = value;

                                    self.variables.insert(&ref_name, RuntimeValue::List(list));
                                    self.propagate_member_source_reg(
                                        *target,
                                        self.frames.len().saturating_sub(1),
                                    )?;
                                }
                                RuntimeValue::HashMap(map) => {
                                    let key = hash_index()?;

                                    let mut guard = map.lock().map_err(|_| {
                                        RuntimeError::UnexpectedType(RuntimeValue::Null)
                                    })?;

                                    guard.insert(key, value);
                                }
                                _ => return Err(RuntimeError::UnexpectedType(RuntimeValue::Null)),
                            }
                            handled = true;
                            break;
                        }
                        RuntimeValue::VarRef(id) => {
                            let current = self
                                .variables
                                .get_by_id(id)
                                .cloned()
                                .ok_or(RuntimeError::DanglingRef(format!("#{}", id)))?;
                            match current {
                                RuntimeValue::Ref(_)
                                | RuntimeValue::VarRef(_)
                                | RuntimeValue::RegRef { .. } => {
                                    target_value = current;
                                    continue;
                                }
                                RuntimeValue::List(mut list) => {
                                    let index = numeric_index()?;
                                    if index < 0 {
                                        return Err(RuntimeError::UnexpectedType(
                                            RuntimeValue::Null,
                                        ));
                                    }

                                    let vec = &mut Gc::make_mut(&mut list).0;
                                    let idx = Self::resolve_index_or_err(vec.len(), index)?;
                                    vec[idx] = value;

                                    let _ = self.variables.set_by_id(id, RuntimeValue::List(list));
                                    self.propagate_member_source_reg(
                                        *target,
                                        self.frames.len().saturating_sub(1),
                                    )?;
                                }
                                RuntimeValue::HashMap(map) => {
                                    let key = hash_index()?;

                                    let mut guard = map.lock().map_err(|_| {
                                        RuntimeError::UnexpectedType(RuntimeValue::Null)
                                    })?;

                                    guard.insert(key, value);
                                }
                                _ => return Err(RuntimeError::UnexpectedType(RuntimeValue::Null)),
                            }
                            handled = true;
                            break;
                        }
                        RuntimeValue::RegRef { frame, reg } => {
                            let current = self.get_reg_value_in_frame(frame, reg).clone();
                            match current {
                                RuntimeValue::Ref(_)
                                | RuntimeValue::VarRef(_)
                                | RuntimeValue::RegRef { .. } => {
                                    target_value = current;
                                    continue;
                                }
                                RuntimeValue::List(mut list) => {
                                    let index = numeric_index()?;

                                    if index < 0 {
                                        return Err(RuntimeError::UnexpectedType(
                                            RuntimeValue::Null,
                                        ));
                                    }

                                    let vec = &mut Gc::make_mut(&mut list).0;
                                    let idx = Self::resolve_index_or_err(vec.len(), index)?;
                                    vec[idx] = value;

                                    let member_source = self
                                        .frames
                                        .get(frame)
                                        .and_then(|vm_frame| vm_frame.member_sources.get(&reg))
                                        .cloned();

                                    self.set_reg_value_in_frame(
                                        frame,
                                        reg,
                                        RuntimeValue::List(list.clone()),
                                    );

                                    if let Some(source) = member_source
                                        && let Some(vm_frame) = self.frames.get_mut(frame)
                                    {
                                        vm_frame.member_sources.insert(reg, source);
                                    }

                                    self.sync_local_reg_value(frame, reg, RuntimeValue::List(list));
                                    self.propagate_member_source_reg(reg, frame)?;
                                }
                                RuntimeValue::HashMap(map) => {
                                    let key = hash_index()?;
                                    let guard = map.lock().map_err(|_| {
                                        RuntimeError::UnexpectedType(RuntimeValue::Null)
                                    })?;
                                    let mut guard = guard;
                                    guard.insert(key, value);
                                }
                                _ => return Err(RuntimeError::UnexpectedType(RuntimeValue::Null)),
                            }
                            handled = true;
                            break;
                        }
                        RuntimeValue::List(mut list) => {
                            let index = numeric_index()?;

                            if index < 0 {
                                return Err(RuntimeError::UnexpectedType(RuntimeValue::Null));
                            }

                            let vec = &mut Gc::make_mut(&mut list).0;
                            let idx = Self::resolve_index_or_err(vec.len(), index)?;
                            vec[idx] = value;

                            let member_source =
                                self.current_frame().member_sources.get(target).cloned();
                            self.set_reg_value(*target, RuntimeValue::List(list));

                            if let Some(source) = member_source {
                                self.current_frame_mut()
                                    .member_sources
                                    .insert(*target, source);
                            }

                            self.propagate_member_source_reg(
                                *target,
                                self.frames.len().saturating_sub(1),
                            )?;

                            handled = true;
                            break;
                        }
                        RuntimeValue::HashMap(map) => {
                            let key = hash_index()?;

                            let mut guard = map
                                .lock()
                                .map_err(|_| RuntimeError::UnexpectedType(RuntimeValue::Null))?;

                            guard.insert(key, value);
                            handled = true;
                            break;
                        }
                        other => return Err(RuntimeError::UnexpectedType(other)),
                    }
                }
                if !handled {
                    return Err(RuntimeError::DanglingRef(
                        "<set-index-depth-limit>".to_string(),
                    ));
                }
            }
            VMInstruction::Ref { dst, value } => {
                let out = match self.get_reg_value(*value).clone() {
                    RuntimeValue::Ref(name) => RuntimeValue::Ref(name),
                    RuntimeValue::VarRef(id) => RuntimeValue::VarRef(id),
                    RuntimeValue::RegRef { frame, reg } => RuntimeValue::RegRef { frame, reg },
                    other => if let Some(name) = self.find_local_name_for_reg(*value) {
                        if let Some(id) = self.global_id_cached(name.as_ref())
                            && self.variables.get_by_id(id).is_some()
                        {
                            RuntimeValue::VarRef(id)
                        } else {
                            RuntimeValue::RegRef {
                                frame: self.frames.len().saturating_sub(1),
                                reg: *value,
                            }
                        }
                    } else if other.should_pass_by_reg_ref()
                        && let Some(reg) = self.find_local_reg_for_value(&other)
                    {
                        RuntimeValue::RegRef {
                            frame: self.frames.len().saturating_sub(1),
                            reg,
                        }
                    } else if let Some(id) = (0..self.variables.slot_len()).find(|id| {
                        matches!(
                            self.variables.get_by_id(*id),
                            Some(RuntimeValue::RegRef { frame, reg })
                                if *frame == self.frames.len().saturating_sub(1) && *reg == *value
                        )
                    }) {
                        RuntimeValue::VarRef(id)
                    } else if let RuntimeValue::List(list) = &other
                        && let Some(id) = (0..self.variables.slot_len()).find(|id| {
                            matches!(
                                self.variables.get_by_id(*id),
                                Some(RuntimeValue::List(other_list))
                                    if std::ptr::eq(list.as_ref(), other_list.as_ref())
                            )
                        })
                    {
                        RuntimeValue::VarRef(id)
                    } else {
                        let name = self.get_ref_id().to_string();
                        let id = self.variables.insert_with_id(&name, other);
                        RuntimeValue::VarRef(id)
                    },
                };
                self.set_reg_value(*dst, out);
                self.propagate_member_source_alias(*value, *dst);
            }
            VMInstruction::Deref { dst, value } => {
                let out = match self.get_reg_value(*value).clone() {
                    RuntimeValue::Ref(x) => self
                        .variables
                        .get(&x)
                        .cloned()
                        .ok_or(RuntimeError::DanglingRef(x))?,
                    RuntimeValue::VarRef(id) => self
                        .variables
                        .get_by_id(id)
                        .cloned()
                        .ok_or(RuntimeError::DanglingRef(format!("#{}", id)))?,
                    RuntimeValue::RegRef { frame, reg } => {
                        self.get_reg_value_in_frame(frame, reg).clone()
                    }
                    RuntimeValue::MutexGuard(guard) => guard.get_clone(),
                    other => other,
                };
                self.set_reg_value(*dst, out);
            }
            VMInstruction::SetRef { target, value } => {
                let target = self.get_reg_value(*target).clone();
                let value = self.get_reg_value(*value).clone();
                match target {
                    RuntimeValue::Ref(name) => {
                        let existed = self.variables.contains_key(&name);
                        self.variables.insert(&name, value);
                        if !existed {
                            self.invalidate_name_resolution_caches();
                        }
                    }
                    RuntimeValue::VarRef(id) => {
                        let _ = self.variables.set_by_id(id, value);
                    }
                    RuntimeValue::RegRef { frame, reg } => {
                        self.set_reg_value_in_frame(frame, reg, value);
                    }
                    RuntimeValue::MutexGuard(guard) => {
                        guard.set_value(value);
                    }
                    _ => return Err(RuntimeError::InvalidBytecode("invalid ref".to_string())),
                }
            }
            VMInstruction::ListAppend {
                target,
                value,
                right,
            } => {
                let value = self.get_reg_value(*value).clone();

                if let Some(RuntimeValue::List(data)) = self.get_mut_reg_value(*target) {
                    if *right {
                        dumpster::sync::Gc::make_mut(data).0.insert(0, value);
                    } else {
                        dumpster::sync::Gc::make_mut(data).0.push(value);
                    };
                } else {
                    let target_val = self.get_reg_value(*target).clone();

                    match target_val {
                        RuntimeValue::List(data) => {
                            let mut data = data.clone();

                            if *right {
                                dumpster::sync::Gc::make_mut(&mut data).0.insert(0, value);
                            } else {
                                dumpster::sync::Gc::make_mut(&mut data).0.push(value);
                            }

                            self.set_reg_value(*target, RuntimeValue::List(data));
                        }
                        RuntimeValue::Ref(name) => {
                            if let Some(RuntimeValue::List(data)) = self.variables.get_mut(&name) {
                                if *right {
                                    dumpster::sync::Gc::make_mut(data).0.insert(0, value);
                                } else {
                                    dumpster::sync::Gc::make_mut(data).0.push(value);
                                }
                            }
                        }
                        RuntimeValue::VarRef(id) => {
                            if let Some(RuntimeValue::List(data)) = self.variables.get_mut_by_id(id)
                            {
                                if *right {
                                    dumpster::sync::Gc::make_mut(data).0.insert(0, value);
                                } else {
                                    dumpster::sync::Gc::make_mut(data).0.push(value);
                                }
                            }
                        }
                        RuntimeValue::RegRef { frame, reg } => {
                            if let Some(RuntimeValue::List(data)) =
                                self.get_mut_reg_value_in_frame(frame, reg)
                            {
                                if *right {
                                    dumpster::sync::Gc::make_mut(data).0.insert(0, value);
                                } else {
                                    dumpster::sync::Gc::make_mut(data).0.push(value);
                                }
                            }
                        }
                        _ => {
                            return Err(RuntimeError::InvalidBytecode(
                                "ListAppend on non-list".to_string(),
                            ));
                        }
                    }
                }
            }
            VMInstruction::StrConcat {
                target,
                value,
                right,
            } => {
                let value = self.get_reg_value(*value).clone();
                let target_val = self.get_reg_value(*target).clone();

                match target_val {
                    RuntimeValue::Str(data) => {
                        let s = if *right {
                            let mut s = value.display(self);
                            s.push_str(data.as_str());
                            s
                        } else {
                            let mut s = data.as_str().to_string();
                            s.push_str(&value.display(self));
                            s
                        };

                        self.set_reg_value(*target, RuntimeValue::Str(Arc::new(s)));
                    }
                    RuntimeValue::Ref(name) => {
                        if let Some(RuntimeValue::Str(data)) = self.variables.get(&name).cloned() {
                            let s = if *right {
                                let mut s = value.display(self);
                                s.push_str(data.as_str());
                                s
                            } else {
                                let mut s = data.as_str().to_string();
                                s.push_str(&value.display(self));
                                s
                            };

                            self.variables.insert(&name, RuntimeValue::Str(Arc::new(s)));
                        }
                    }
                    RuntimeValue::VarRef(id) => {
                        if let Some(RuntimeValue::Str(data)) = self.variables.get_by_id(id).cloned()
                        {
                            let s = if *right {
                                let mut s = value.display(self);
                                s.push_str(data.as_str());
                                s
                            } else {
                                let mut s = data.as_str().to_string();
                                s.push_str(&value.display(self));
                                s
                            };

                            let _ = self.variables.set_by_id(id, RuntimeValue::Str(Arc::new(s)));
                        }
                    }
                    RuntimeValue::RegRef { frame, reg } => {
                        if let RuntimeValue::Str(data) =
                            self.get_reg_value_in_frame(frame, reg).clone()
                        {
                            let s = if *right {
                                let mut s = value.display(self);
                                s.push_str(data.as_str());
                                s
                            } else {
                                let mut s = data.as_str().to_string();
                                s.push_str(&value.display(self));
                                s
                            };

                            self.set_reg_value_in_frame(frame, reg, RuntimeValue::Str(Arc::new(s)));
                        }
                    }
                    _ => {
                        return Err(RuntimeError::InvalidBytecode(
                            "StrConcat on non-string".to_string(),
                        ));
                    }
                }
            }
            VMInstruction::Jump(target) => return Ok(TerminateValue::Jump(*target)),
            VMInstruction::Branch {
                cond,
                then_block,
                else_block,
            } => {
                return if self.eval_branch_condition(*cond, block, ip)? {
                    Ok(TerminateValue::Jump(*then_block))
                } else {
                    Ok(TerminateValue::Jump(*else_block))
                };
            }
            VMInstruction::Return { value } => {
                return if let Some(reg) = value {
                    Ok(TerminateValue::Return(self.get_reg_value(*reg).clone()))
                } else {
                    Ok(TerminateValue::Return(RuntimeValue::Null))
                };
            }
            VMInstruction::Noop => {}
        }

        Ok(TerminateValue::None)
    }
}
