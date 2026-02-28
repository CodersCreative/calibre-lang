use calibre_lir::BlockId;
use calibre_parser::ast::ObjectMap;
use calibre_parser::ast::ParserInnerType;
use calibre_parser::ast::binary::BinaryOperator;
use calibre_parser::ast::comparison::ComparisonOperator;
use dumpster::sync::Gc;
use rustc_hash::{FxHashMap, FxHashSet};
use smallvec::SmallVec;
use std::sync::Arc;

use crate::{
    PreparedDirectCall, VM, VarName,
    conversion::{VMBlock, VMFunction, VMGlobal, VMInstruction, VMLiteral},
    error::RuntimeError,
    value::{
        RuntimeValue, TerminateValue, WaitGroupInner,
        operation::{binary, boolean, comparison},
    },
};

mod instruction;

impl VM {
    #[inline]
    fn push_owner_member_candidates(
        candidates: &mut Vec<String>,
        owner: &str,
        member: &str,
        short_member: Option<&str>,
    ) {
        candidates.push(format!("{owner}::{member}"));
        if let Some(short) = short_member {
            candidates.push(format!("{owner}::{short}"));
        }
    }

    #[inline]
    fn push_short_owner_member_candidates(
        candidates: &mut Vec<String>,
        owner: &str,
        member: &str,
        short_member: Option<&str>,
    ) {
        let short_owner = calibre_parser::qualified_name_tail(owner);
        if short_owner != owner {
            Self::push_owner_member_candidates(candidates, short_owner, member, short_member);
        }
    }

    fn resolve_first_candidate<I>(&mut self, candidates: I) -> Option<RuntimeValue>
    where
        I: IntoIterator<Item = String>,
    {
        for candidate in candidates {
            if let Some((resolved, _)) = self.try_resolve_global_runtime_value(&candidate) {
                return Some(resolved);
            }
            if let Some((resolved, _)) = self.resolve_suffix_global_runtime_value(&candidate) {
                return Some(resolved);
            }
        }
        None
    }

    #[inline]
    pub(crate) fn is_runtime_callable(value: &RuntimeValue) -> bool {
        matches!(
            value,
            RuntimeValue::Function { .. }
                | RuntimeValue::NativeFunction(_)
                | RuntimeValue::ExternFunction(_)
                | RuntimeValue::BoundMethod { .. }
        )
    }

    pub(crate) fn call_runtime_callable_at(
        &mut self,
        callable: RuntimeValue,
        args: Vec<RuntimeValue>,
        callsite_block: usize,
        callsite_tag: u32,
    ) -> Result<RuntimeValue, RuntimeError> {
        match callable {
            RuntimeValue::Function { name, captures } => {
                let callsite = (self.current_frame().func_ptr, callsite_block, callsite_tag);
                let Some(func) = self.resolve_callable_cached(name.as_str(), callsite) else {
                    return Err(RuntimeError::FunctionNotFound(name.to_string()));
                };
                self.run_function(func.as_ref(), args, captures)
            }
            RuntimeValue::NativeFunction(func) => func.run(self, args),
            RuntimeValue::ExternFunction(func) => func.call(self, args),
            RuntimeValue::BoundMethod { callee, receiver } => {
                let mut full_args = vec![receiver.as_ref().clone()];
                full_args.extend(args);
                self.call_runtime_callable_at(
                    *callee,
                    full_args,
                    callsite_block,
                    callsite_tag.saturating_sub(1),
                )
            }
            _ => Err(RuntimeError::InvalidFunctionCall),
        }
    }

    #[inline]
    fn invoke_callable_value(
        &mut self,
        callable: RuntimeValue,
        args: Vec<RuntimeValue>,
        callsite_tag: u32,
    ) -> Option<RuntimeValue> {
        self.call_runtime_callable_at(callable, args, usize::MAX, callsite_tag)
            .ok()
    }

    fn resolve_display_override(
        &mut self,
        value: &RuntimeValue,
    ) -> Option<(RuntimeValue, RuntimeValue)> {
        let resolved = self.resolve_value_for_op_ref(value).ok()?;
        match resolved {
            RuntimeValue::DynObject {
                type_name,
                value,
                vtable,
                ..
            } => {
                let mapped = vtable.get("display").map(|x| x.as_str());
                let callable = self.resolve_dyn_method_callable(&type_name, "display", mapped)?;
                Some((callable, value.as_ref().clone()))
            }
            RuntimeValue::Aggregate(Some(ref type_name), _)
            | RuntimeValue::Enum(ref type_name, _, _) => {
                let callable = self.resolve_dyn_method_callable(type_name, "display", None)?;
                Some((callable, resolved))
            }
            RuntimeValue::Generator { ref type_name, .. } => {
                let callable =
                    self.resolve_dyn_method_callable(type_name.as_str(), "display", None)?;
                Some((callable, resolved))
            }
            _ => None,
        }
    }

    fn invoke_display_override(
        &mut self,
        callable: RuntimeValue,
        receiver: RuntimeValue,
    ) -> Option<String> {
        let output =
            self.invoke_callable_value(callable, vec![receiver], u32::MAX.saturating_sub(1))?;

        match output {
            RuntimeValue::Str(s) => Some(s.to_string()),
            other => Some(other.display(self)),
        }
    }

    pub(crate) fn display_value(&mut self, value: &RuntimeValue) -> String {
        if let Some((callable, receiver)) = self.resolve_display_override(value)
            && let Some(output) = self.invoke_display_override(callable, receiver)
        {
            return output;
        }
        value.display(self)
    }

    fn concrete_runtime_type_name(&self, value: &RuntimeValue) -> Option<String> {
        match value {
            RuntimeValue::Int(_) => Some("int".to_string()),
            RuntimeValue::UInt(_) => Some("uint".to_string()),
            RuntimeValue::Byte(_) => Some("byte".to_string()),
            RuntimeValue::Float(_) => Some("float".to_string()),
            RuntimeValue::Bool(_) => Some("bool".to_string()),
            RuntimeValue::Str(_) => Some("str".to_string()),
            RuntimeValue::Char(_) => Some("char".to_string()),
            RuntimeValue::Range(_, _) => Some("range".to_string()),
            RuntimeValue::Ptr(_) => Some("ptr".to_string()),
            RuntimeValue::Aggregate(Some(name), _) | RuntimeValue::Enum(name, _, _) => {
                Some(name.clone())
            }
            RuntimeValue::Generator { type_name, .. } => Some(type_name.to_string()),
            RuntimeValue::DynObject { type_name, .. } => Some(type_name.to_string()),
            _ => None,
        }
    }

    fn name_matches(actual: &str, target: &str) -> bool {
        calibre_parser::qualified_name_matches(actual, target)
    }

    fn lookup_dyn_trait_table(
        &self,
        concrete: &str,
        trait_name: &str,
    ) -> Option<&FxHashMap<String, String>> {
        for (imp_ty, traits) in self.registry.dyn_vtables.iter() {
            if !Self::name_matches(imp_ty, concrete) {
                continue;
            }
            for (imp_trait, table) in traits {
                if Self::name_matches(imp_trait, trait_name) {
                    return Some(table);
                }
            }
        }
        None
    }

    pub(crate) fn build_dyn_vtable_for_value(
        &self,
        value: &RuntimeValue,
        constraints: &[String],
    ) -> Option<(String, FxHashMap<String, String>)> {
        let concrete = self.concrete_runtime_type_name(value)?;
        if constraints.is_empty() {
            return Some((concrete, FxHashMap::default()));
        }

        let mut merged = FxHashMap::default();
        for tr in constraints {
            let table = self.lookup_dyn_trait_table(&concrete, tr)?;
            for (member, callee) in table {
                merged
                    .entry(member.clone())
                    .or_insert_with(|| callee.clone());
            }
        }
        Some((concrete, merged))
    }

    pub(crate) fn resolve_dyn_method_callable(
        &mut self,
        type_name: &str,
        member: &str,
        mapped: Option<&str>,
    ) -> Option<RuntimeValue> {
        let mut candidates = Vec::with_capacity(4);
        if let Some(m) = mapped {
            candidates.push(m.to_string());
        }
        Self::push_owner_member_candidates(&mut candidates, type_name, member, None);
        Self::push_short_owner_member_candidates(&mut candidates, type_name, member, None);
        self.resolve_first_candidate(candidates)
    }

    pub(crate) fn resolve_associated_member_value(
        &mut self,
        owner: &str,
        member: &str,
        short_member: Option<&str>,
    ) -> Option<RuntimeValue> {
        let mut candidates = Vec::with_capacity(5);

        if member.contains("::") {
            candidates.push(member.to_string());
        }

        Self::push_owner_member_candidates(&mut candidates, owner, member, short_member);
        Self::push_short_owner_member_candidates(&mut candidates, owner, member, short_member);

        self.resolve_first_candidate(candidates)
    }

    #[inline]
    fn install_captures(
        &mut self,
        captures: &[(String, RuntimeValue)],
    ) -> Vec<(String, Option<RuntimeValue>)> {
        if captures.is_empty() {
            return Vec::new();
        }
        let mut prev_vars = Vec::with_capacity(captures.len());
        for (name, value) in captures {
            let old = self.variables.insert(name, value.clone());
            prev_vars.push((name.clone(), old));
        }
        prev_vars
    }

    #[inline]
    fn restore_captures(&mut self, prev_vars: Vec<(String, Option<RuntimeValue>)>) {
        for (name, old) in prev_vars {
            if let Some(value) = old {
                self.variables.insert(&name, value);
            } else {
                self.variables.remove(&name);
            }
        }
    }

    #[inline(always)]
    fn call_arg_from_reg(&self, reg: u16) -> RuntimeValue {
        let frame = self.frames.len().saturating_sub(1);
        self.call_arg_from_frame_reg(frame, reg)
    }

    #[inline(always)]
    fn call_arg_from_frame_reg(&self, frame: usize, reg: u16) -> RuntimeValue {
        match self.get_reg_value_in_frame(frame, reg) {
            RuntimeValue::Aggregate(_, _)
            | RuntimeValue::List(_)
            | RuntimeValue::Enum(_, _, _)
            | RuntimeValue::Option(_)
            | RuntimeValue::Result(_)
            | RuntimeValue::Ptr(_) => RuntimeValue::RegRef { frame, reg },
            other => other.clone(),
        }
    }

    #[inline]
    fn collect_call_args_vec(&self, args: &[u16]) -> Vec<RuntimeValue> {
        let mut call_args = Vec::with_capacity(args.len());
        for reg in args {
            call_args.push(self.call_arg_from_reg(*reg));
        }
        call_args
    }

    #[inline]
    pub(crate) fn resolve_operand_value(
        &mut self,
        value: RuntimeValue,
    ) -> Result<RuntimeValue, RuntimeError> {
        match value {
            RuntimeValue::Ref(_)
            | RuntimeValue::VarRef(_)
            | RuntimeValue::RegRef { .. }
            | RuntimeValue::MutexGuard(_) => self.resolve_value_for_op_ref(&value),
            other => Ok(other),
        }
    }

    fn runtime_matches_type(&self, value: &RuntimeValue, target: &ParserInnerType) -> bool {
        if let RuntimeValue::DynObject {
            value: inner,
            constraints,
            ..
        } = value
        {
            return match target {
                ParserInnerType::Dynamic => true,
                ParserInnerType::DynamicTraits(traits) => traits
                    .iter()
                    .all(|tr| constraints.iter().any(|x| Self::name_matches(x, tr))),
                _ => self.runtime_matches_type(inner.as_ref(), target),
            };
        }

        match target {
            ParserInnerType::Dynamic => true,
            ParserInnerType::DynamicTraits(traits) => match value {
                RuntimeValue::DynObject { constraints, .. } => traits
                    .iter()
                    .all(|tr| constraints.iter().any(|x| Self::name_matches(x, tr))),
                other => self
                    .build_dyn_vtable_for_value(other, traits.as_slice())
                    .is_some(),
            },
            ParserInnerType::Auto(_) => true,
            ParserInnerType::Ref(inner, _) => self.runtime_matches_type(value, &inner.data_type),
            ParserInnerType::Float => matches!(value, RuntimeValue::Float(_)),
            ParserInnerType::Int => matches!(value, RuntimeValue::Int(_)),
            ParserInnerType::UInt => matches!(value, RuntimeValue::UInt(_)),
            ParserInnerType::Byte => matches!(value, RuntimeValue::Byte(_)),
            ParserInnerType::Null => matches!(value, RuntimeValue::Null),
            ParserInnerType::Bool => matches!(value, RuntimeValue::Bool(_)),
            ParserInnerType::Str => matches!(value, RuntimeValue::Str(_)),
            ParserInnerType::Char => matches!(value, RuntimeValue::Char(_)),
            ParserInnerType::Range => matches!(value, RuntimeValue::Range(_, _)),
            ParserInnerType::Ptr(_) => matches!(value, RuntimeValue::Ptr(_)),
            ParserInnerType::List(inner) => {
                if let RuntimeValue::List(items) = value {
                    items
                        .as_ref()
                        .0
                        .iter()
                        .all(|item| self.runtime_matches_type(item, &inner.data_type))
                } else {
                    false
                }
            }
            ParserInnerType::Tuple(types) => {
                if let RuntimeValue::Aggregate(None, fields) = value {
                    if fields.as_ref().0.len() != types.len() {
                        return false;
                    }
                    types.iter().enumerate().all(|(i, t)| {
                        fields
                            .as_ref()
                            .0
                            .iter()
                            .find(|(name, _)| name == &i.to_string())
                            .map(|(_, v)| self.runtime_matches_type(v, &t.data_type))
                            .unwrap_or(false)
                    })
                } else {
                    false
                }
            }
            ParserInnerType::Option(inner) => match value {
                RuntimeValue::Option(Some(v)) => {
                    self.runtime_matches_type(v.as_ref(), &inner.data_type)
                }
                RuntimeValue::Option(None) => true,
                _ => false,
            },
            ParserInnerType::Result { ok, err } => match value {
                RuntimeValue::Result(Ok(v)) => self.runtime_matches_type(v.as_ref(), &ok.data_type),
                RuntimeValue::Result(Err(v)) => {
                    self.runtime_matches_type(v.as_ref(), &err.data_type)
                }
                _ => false,
            },
            ParserInnerType::Function { .. } | ParserInnerType::NativeFunction(_) => matches!(
                value,
                RuntimeValue::Function { .. }
                    | RuntimeValue::NativeFunction(_)
                    | RuntimeValue::ExternFunction(_)
            ),
            ParserInnerType::Struct(name) => match value {
                RuntimeValue::Aggregate(Some(actual), _) | RuntimeValue::Enum(actual, _, _) => {
                    Self::name_matches(actual, name)
                }
                RuntimeValue::Generator { type_name, .. } => Self::name_matches(type_name, name),
                _ => false,
            },
            ParserInnerType::StructWithGenerics { identifier, .. } => match value {
                RuntimeValue::Aggregate(Some(actual), _) | RuntimeValue::Enum(actual, _, _) => {
                    Self::name_matches(actual, identifier)
                }
                RuntimeValue::Generator { type_name, .. } => {
                    Self::name_matches(type_name, identifier)
                }
                _ => false,
            },
            ParserInnerType::Scope(_)
            | ParserInnerType::DollarIdentifier(_)
            | ParserInnerType::FfiType(_) => false,
        }
    }

    #[inline(always)]
    fn try_fast_int_binary(
        &mut self,
        op: BinaryOperator,
        left_reg: u16,
        right_reg: u16,
    ) -> Option<RuntimeValue> {
        let left = match self.get_reg_value(left_reg) {
            RuntimeValue::Int(v) => *v,
            _ => return None,
        };
        let right = match self.get_reg_value(right_reg) {
            RuntimeValue::Int(v) => *v,
            _ => return None,
        };
        let out = match op {
            BinaryOperator::Add => RuntimeValue::Int(left.wrapping_add(right)),
            BinaryOperator::Sub => RuntimeValue::Int(left.wrapping_sub(right)),
            BinaryOperator::Mul => RuntimeValue::Int(left.wrapping_mul(right)),
            BinaryOperator::Div => {
                if right == 0 {
                    return None;
                }
                RuntimeValue::Int(left / right)
            }
            BinaryOperator::Mod => {
                if right == 0 {
                    return None;
                }
                RuntimeValue::Int(left % right)
            }
            BinaryOperator::BitAnd => RuntimeValue::Int(left & right),
            BinaryOperator::BitOr => RuntimeValue::Int(left | right),
            BinaryOperator::BitXor => RuntimeValue::Int(left ^ right),
            BinaryOperator::Shl => RuntimeValue::Int(left.wrapping_shl(right as u32)),
            BinaryOperator::Shr => RuntimeValue::Int(left.wrapping_shr(right as u32)),
            BinaryOperator::Pow => return None,
        };
        Some(out)
    }

    #[inline(always)]
    fn try_fast_int_comparison(
        &self,
        op: ComparisonOperator,
        left_reg: u16,
        right_reg: u16,
    ) -> Option<RuntimeValue> {
        let left = match self.get_reg_value(left_reg) {
            RuntimeValue::Int(v) => *v,
            _ => return None,
        };
        let right = match self.get_reg_value(right_reg) {
            RuntimeValue::Int(v) => *v,
            _ => return None,
        };
        let out = match op {
            ComparisonOperator::Greater => left > right,
            ComparisonOperator::Lesser => left < right,
            ComparisonOperator::LesserEqual => left <= right,
            ComparisonOperator::GreaterEqual => left >= right,
            ComparisonOperator::Equal => left == right,
            ComparisonOperator::NotEqual => left != right,
        };
        Some(RuntimeValue::Bool(out))
    }

    fn try_resolve_global_runtime_value(&mut self, name: &str) -> Option<(RuntimeValue, String)> {
        if let Some(found) = self.resolve_named_global_runtime_value(name) {
            return Some(found);
        }
        if name.contains("::") {
            return None;
        }

        let short_name = calibre_parser::qualified_name_tail(name);
        if short_name == name {
            return None;
        }
        if let Some(found) = self.resolve_named_global_runtime_value(short_name) {
            return Some(found);
        }
        let base = calibre_parser::qualified_name_base(short_name);
        if base != short_name
            && let Some(found) = self.resolve_suffix_global_runtime_value(base)
        {
            return Some(found);
        }
        self.resolve_suffix_global_runtime_value(short_name)
    }

    fn resolve_callable_cached(
        &mut self,
        name: &str,
        callsite: (usize, usize, u32),
    ) -> Option<Arc<VMFunction>> {
        if let Some(cached) = self.caches.callsite.get(&callsite) {
            if cached.name == name {
                return Some(Arc::clone(cached));
            }
        }
        if let Some(cached) = self.caches.call.get(name) {
            let resolved = Arc::clone(cached);
            self.caches.callsite.insert(callsite, Arc::clone(&resolved));
            return Some(resolved);
        }

        let found = self.resolve_function_by_name(name);

        if let Some(ref func) = found {
            let cached = Arc::clone(func);
            self.caches
                .call
                .insert(name.to_string(), Arc::clone(&cached));
            self.caches.callsite.insert(callsite, cached);
        }
        found
    }

    fn try_move_global_runtime_value(&mut self, name: &str) -> Option<RuntimeValue> {
        if let Some(found) = self.move_named_global_runtime_value(name) {
            return Some(found);
        }
        if name.contains("::") {
            return None;
        }
        let short_name = calibre_parser::qualified_name_tail(name);
        if short_name == name {
            return None;
        }
        if let Some(found) = self.move_named_global_runtime_value(short_name) {
            return Some(found);
        }
        self.move_suffix_global_runtime_value(short_name)
    }

    #[inline]
    fn resolve_named_global_runtime_value(&self, name: &str) -> Option<(RuntimeValue, String)> {
        if let Some(func) = self.get_function_ref(name) {
            return Some((self.make_runtime_function(func), name.to_string()));
        }
        self.variables.get(name).map(|var| {
            (
                self.resolve_saveable_runtime_value_ref(var),
                name.to_string(),
            )
        })
    }

    #[inline]
    fn resolve_suffix_global_runtime_value(
        &mut self,
        short_name: &str,
    ) -> Option<(RuntimeValue, String)> {
        if let Some(func) = self.find_unique_function_by_suffix_ref(short_name) {
            return Some((self.make_runtime_function(func), func.name.clone()));
        }
        let var_name = self.find_unique_var_by_suffix_cached(short_name)?;
        let var = self.variables.get(&var_name)?;
        Some((self.resolve_saveable_runtime_value_ref(var), var_name))
    }

    #[inline]
    fn move_named_global_runtime_value(&mut self, name: &str) -> Option<RuntimeValue> {
        if let Some(func) = self.take_function(name) {
            return Some(self.make_runtime_function(&func));
        }
        self.variables
            .remove(name)
            .map(|var| self.resolve_saveable_runtime_value_ref(&var))
    }

    #[inline]
    fn move_suffix_global_runtime_value(&mut self, short_name: &str) -> Option<RuntimeValue> {
        if let Some(func) = self.find_unique_function_by_suffix_ref(short_name) {
            let func_name = func.name.clone();
            let value = self.make_runtime_function(func);
            self.moved_functions.insert(func_name);
            return Some(value);
        }
        let var_name = self.find_unique_var_by_suffix_cached(short_name)?;
        self.variables
            .remove(&var_name)
            .map(|var| self.resolve_saveable_runtime_value_ref(&var))
    }

    pub fn run(
        &mut self,
        function: &VMFunction,
        args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, RuntimeError> {
        self.run_globals()?;
        self.run_function(function, args, Self::empty_captures())
    }

    pub fn run_globals(&mut self) -> Result<(), RuntimeError> {
        if self.registry.globals.is_empty() {
            return Ok(());
        }
        let registry = Arc::clone(&self.registry);
        for global in registry.globals.values() {
            self.run_global(global)?;
        }
        Ok(())
    }

    pub fn run_global(&mut self, global: &VMGlobal) -> Result<RuntimeValue, RuntimeError> {
        let mut block = global
            .blocks
            .get(*global.block_map.get(&global.entry).unwrap_or(&0))
            .ok_or_else(|| RuntimeError::InvalidBytecode("global has no blocks".to_string()))?;

        let mut prev_block: Option<BlockId> = None;
        loop {
            match self.run_block(block, prev_block)? {
                TerminateValue::Jump(target) => {
                    prev_block = Some(block.id);
                    block = global
                        .blocks
                        .get(*global.block_map.get(&target).unwrap_or(&0))
                        .ok_or_else(|| {
                            RuntimeError::InvalidBytecode(format!(
                                "invalid global block {}",
                                target.0
                            ))
                        })?;
                }
                TerminateValue::Return(x) => match x {
                    RuntimeValue::Null => break,
                    x => return Ok(x),
                },
                TerminateValue::Yield { .. } => break,
                TerminateValue::None => break,
            }
        }

        Ok(RuntimeValue::Null)
    }

    pub fn run_function<I>(
        &mut self,
        function: &VMFunction,
        args: I,
        captures: std::sync::Arc<Vec<(String, RuntimeValue)>>,
    ) -> Result<RuntimeValue, RuntimeError>
    where
        I: IntoIterator<Item = RuntimeValue>,
    {
        let mut state = crate::TaskState::default();
        match self.run_function_with_budget(function, args, captures, usize::MAX, &mut state)? {
            Some(value) => Ok(value),
            None => Ok(RuntimeValue::Null),
        }
    }

    #[inline]
    fn run_function_from_regs(
        &mut self,
        function: &VMFunction,
        args: &[u16],
        captures: std::sync::Arc<Vec<(String, RuntimeValue)>>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let caller_frame = self.frames.len().saturating_sub(1);
        let prev_vars = self.install_captures(captures.as_ref());

        let func_ptr = function as *const VMFunction as usize;
        self.push_frame(function.reg_count as usize, func_ptr);
        for (reg, arg_reg) in function.param_regs.iter().zip(args.iter().copied()) {
            let arg = self.call_arg_from_frame_reg(caller_frame, arg_reg);
            self.set_reg_value(*reg, arg);
        }
        let base = self.local_map_base_for(function);
        let frame = self.current_frame_mut();
        frame.local_map_base = Some(base);

        let mut block_id = function.entry;
        let mut block = function
            .blocks
            .get(*function.block_map.get(&block_id).unwrap_or(&0))
            .ok_or_else(|| RuntimeError::InvalidBytecode("function has no blocks".to_string()))?;
        let mut prev_block: Option<BlockId> = None;
        let mut result = RuntimeValue::Null;
        let mut returned = false;
        loop {
            match self.run_block(block, prev_block)? {
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
                }
                TerminateValue::Return(x) => {
                    result = x;
                    returned = true;
                    break;
                }
                TerminateValue::Yield { .. } => break,
                TerminateValue::None => break,
            }
        }

        if function.returns_value && !returned {
            result = self.get_reg_value(function.ret_reg).clone();
        }

        if let RuntimeValue::RegRef { frame, reg } = result {
            result = self.get_reg_value_in_frame(frame, reg).clone();
        }

        self.pop_frame();
        self.restore_captures(prev_vars);

        Ok(result)
    }

    fn try_trampoline_self_tail_call(
        &mut self,
        block: &VMBlock,
        ip: u32,
        dst: u16,
        args: &[u16],
        func: &VMFunction,
    ) -> Option<TerminateValue> {
        let Some(VMInstruction::Return {
            value: Some(ret_reg),
        }) = block.instructions.get((ip as usize).saturating_add(1))
        else {
            return None;
        };
        if *ret_reg != dst || args.len() != func.param_regs.len() {
            return None;
        }

        let caller_frame = self.frames.len().saturating_sub(1);
        let mut call_args: SmallVec<[RuntimeValue; 8]> = SmallVec::with_capacity(args.len());
        for reg in args {
            call_args.push(self.call_arg_from_frame_reg(caller_frame, *reg));
        }

        let base = self.local_map_base_for(func);
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
            frame.local_map_base = Some(base);
            frame.acc = RuntimeValue::Null;
            frame.func_ptr = func as *const VMFunction as usize;
        }
        for (reg, arg) in func.param_regs.iter().zip(call_args) {
            let idx = *reg as usize;
            if idx < reg_count {
                self.reg_arena[start + idx] = arg;
            }
        }
        Some(TerminateValue::Jump(func.entry))
    }

    fn resolve_direct_callsite_cached(
        &mut self,
        block: &VMBlock,
        ip: u32,
        name_idx: u16,
    ) -> Result<Option<PreparedDirectCall>, RuntimeError> {
        let callsite = (self.current_frame().func_ptr, block.id.0 as usize, ip);
        if let Some(cached) = self.caches.prepared_direct_calls.get(&callsite) {
            return Ok(Some(cached.clone()));
        }

        let func_name = self.local_string(block, name_idx)?;
        if let Some(func) = self.resolve_callable_cached(func_name, callsite) {
            let prepared = PreparedDirectCall::Vm(func);
            self.caches
                .prepared_direct_calls
                .insert(callsite, prepared.clone());
            return Ok(Some(prepared));
        }

        let Some((value, _)) = self.try_resolve_global_runtime_value(func_name) else {
            return Ok(None);
        };

        let prepared = match value {
            RuntimeValue::NativeFunction(func) => PreparedDirectCall::Native(func),
            RuntimeValue::ExternFunction(func) => PreparedDirectCall::Extern(func),
            RuntimeValue::Function { name, captures } if captures.as_ref().is_empty() => {
                let Some(func) = self.resolve_callable_cached(name.as_ref(), callsite) else {
                    return Ok(None);
                };
                PreparedDirectCall::Vm(func)
            }
            _ => return Ok(None),
        };
        self.caches
            .prepared_direct_calls
            .insert(callsite, prepared.clone());
        Ok(Some(prepared))
    }

    pub fn run_function_with_budget<I>(
        &mut self,
        function: &VMFunction,
        args: I,
        captures: std::sync::Arc<Vec<(String, RuntimeValue)>>,
        budget: usize,
        state: &mut crate::TaskState,
    ) -> Result<Option<RuntimeValue>, RuntimeError>
    where
        I: IntoIterator<Item = RuntimeValue>,
    {
        state.yielded = None;
        let prev_vars = self.install_captures(captures.as_ref());

        if state.block.is_none() {
            let func_ptr = function as *const VMFunction as usize;
            self.push_frame(function.reg_count as usize, func_ptr);
            for (reg, arg) in function.param_regs.iter().zip(args) {
                self.set_reg_value(*reg, arg);
            }
            let base = self.local_map_base_for(function);
            let frame = self.current_frame_mut();
            frame.local_map_base = Some(base);
            state.block = Some(function.entry);
            state.ip = 0;
            state.prev_block = None;
        }

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

        if function.returns_value && !returned {
            result = self.get_reg_value(function.ret_reg).clone();
        }

        if let RuntimeValue::RegRef { frame, reg } = result {
            result = self.get_reg_value_in_frame(frame, reg).clone();
        }

        self.pop_frame();
        self.restore_captures(prev_vars);

        Ok(Some(result))
    }

    fn apply_phis(&mut self, block: &VMBlock, prev: Option<BlockId>) -> Result<(), RuntimeError> {
        if block.phis.is_empty() {
            return Ok(());
        }
        let Some(prev) = prev else {
            return Ok(());
        };
        for phi in &block.phis {
            let mut selected = None;
            for (pred, reg) in &phi.sources {
                if *pred == prev {
                    selected = Some(*reg);
                    break;
                }
            }
            let reg = selected.unwrap_or_else(|| phi.sources.first().map(|x| x.1).unwrap_or(0));
            let value = self.get_reg_value(reg).clone();
            self.set_reg_value(phi.dest, value);
        }
        Ok(())
    }

    pub fn run_block(
        &mut self,
        block: &VMBlock,
        prev: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        self.run_block_with_budget(block, prev, 0, None)
    }

    pub fn run_block_with_budget(
        &mut self,
        block: &VMBlock,
        prev: Option<BlockId>,
        start_ip: usize,
        budget: Option<usize>,
    ) -> Result<TerminateValue, RuntimeError> {
        if start_ip == 0 {
            self.apply_phis(block, prev)?;
        }
        let mut fuel = budget.unwrap_or(usize::MAX);
        for (ip, instruction) in block.instructions.iter().enumerate().skip(start_ip) {
            if (ip & 0x3f) == 0 {
                self.maybe_collect_garbage();
            }
            let step = match self.run_instruction(instruction, block, ip as u32, prev) {
                Ok(step) => step,
                Err(e) => {
                    let span = block.instruction_spans.get(ip).cloned().unwrap_or_default();
                    return Err(RuntimeError::at(span, e));
                }
            };

            match step {
                TerminateValue::None => {}
                x => return Ok(x),
            }

            if fuel != usize::MAX {
                fuel = fuel.saturating_sub(1);
                if fuel == 0 {
                    return Ok(TerminateValue::Yield {
                        block: block.id,
                        ip: ip + 1,
                        prev_block: prev,
                        yielded: None,
                    });
                }
            }
        }

        Ok(TerminateValue::None)
    }
}
