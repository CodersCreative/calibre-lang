use crate::{
    VM,
    conversion::{VMBlock, VMFunction, VMGlobal},
    error::RuntimeError,
    evaluate::instruction::VMEvaluation,
    value::{RuntimeValue, TerminateValue},
};
use calibre_lir::ast::BlockId;
use calibre_parser::ast::idents::ParserText;
use calibre_parser::ast::types::ParserInnerType;
use std::sync::Arc;
use tracing::{debug, instrument};
use ustr::{Ustr, UstrMap};

pub mod access;
pub mod binary;
pub mod calling;
pub mod functions;
pub mod instruction;
pub mod literals;
pub mod memory;
pub mod registers;
pub mod termination;
pub mod variables;
pub mod write_back;

#[derive(Debug)]
enum CaptureRestore {
    Value(Option<RuntimeValue>),
    AliasOnly,
    Keep,
}

impl VM {
    #[inline]
    fn push_owner_member_candidates(
        candidates: &mut Vec<String>,
        owner: &str,
        member: &str,
        short_member: Option<&str>,
    ) {
        candidates.push(format!("{owner}::{member}"));
        candidates.push(format!("{owner}.{member}"));
        if let Some(short) = short_member {
            candidates.push(format!("{owner}::{short}"));
            candidates.push(format!("{owner}.{short}"));
        }
    }

    #[inline]
    fn push_short_owner_member_candidates(
        candidates: &mut Vec<String>,
        owner: &str,
        member: &str,
        short_member: Option<&str>,
    ) {
        let short_owner =
            ParserText::get_temp_name_suffix(&owner).unwrap_or_else(|| owner.to_string());
        if short_owner != owner {
            Self::push_owner_member_candidates(candidates, &short_owner, member, short_member);
        }
    }

    fn resolve_first_candidate<I>(&mut self, candidates: I) -> Option<RuntimeValue>
    where
        I: IntoIterator<Item = String>,
    {
        for candidate in candidates {
            if let Some(resolved) = self.get_value(&Ustr::from(&candidate)) {
                if matches!(resolved, RuntimeValue::Null) {
                    continue;
                }
                return Some(resolved);
            }
        }
        None
    }

    #[inline]
    fn build_member_candidates(
        owner: &str,
        member: &str,
        short_member: Option<&str>,
        include_member_as_is: bool,
        mapped: Option<&str>,
    ) -> Vec<String> {
        let mut candidates = Vec::with_capacity(10);
        if let Some(mapped) = mapped {
            candidates.push(mapped.to_string());
        }
        if include_member_as_is && member.contains("::") {
            candidates.push(member.to_string());
        }
        Self::push_owner_member_candidates(&mut candidates, owner, member, short_member);
        Self::push_short_owner_member_candidates(&mut candidates, owner, member, short_member);
        if owner.contains("Self::Item") {
            let normalized = owner.replace("Self::Item", "T");
            Self::push_owner_member_candidates(&mut candidates, &normalized, member, short_member);
            Self::push_short_owner_member_candidates(
                &mut candidates,
                &normalized,
                member,
                short_member,
            );
        }

        candidates
    }

    fn lookup_dyn_trait_table(&self, concrete: &str, trait_name: &str) -> Option<&UstrMap<Ustr>> {
        for (imp_ty, traits) in self.registry.dyn_vtables.iter() {
            if !ParserText::temp_name_suffix_matches(imp_ty, &concrete) {
                continue;
            }
            for (imp_trait, table) in traits {
                if ParserText::temp_name_suffix_matches(imp_trait, &trait_name) {
                    return Some(table);
                }
            }
        }
        None
    }

    pub(crate) fn build_dyn_vtable_for_value(
        &self,
        value: &RuntimeValue,
        constraints: &[Ustr],
    ) -> Option<(Ustr, UstrMap<Ustr>)> {
        let concrete = value.impl_name()?;
        if constraints.is_empty() {
            return Some((concrete, UstrMap::default()));
        }

        let mut merged = UstrMap::default();
        for tr in constraints {
            let table = self.lookup_dyn_trait_table(&concrete, tr)?;
            for (member, callee) in table {
                merged.entry(*member).or_insert_with(|| *callee);
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
        let candidates = Self::build_member_candidates(type_name, member, None, false, mapped);
        self.resolve_first_candidate(candidates)
    }

    pub(crate) fn resolve_associated_member_value(
        &mut self,
        owner: &str,
        member: &str,
        short_member: Option<&str>,
    ) -> Option<RuntimeValue> {
        let candidates = Self::build_member_candidates(owner, member, short_member, true, None);
        for candidate in candidates {
            if let Some(resolved) = self.get_value(&Ustr::from(&candidate)) {
                if matches!(resolved, RuntimeValue::Null) {
                    continue;
                }
                return Some(resolved);
            }
        }

        if !owner.contains(":<")
            && let Some(found) = self.resolve_struct_like_member(owner, member, short_member)
        {
            return Some(found);
        }

        if !ParserText::is_temp_name(&owner) {
            let std_owner = format!("std::{owner}");
            let candidates =
                Self::build_member_candidates(&std_owner, member, short_member, true, None);
            for candidate in candidates {
                if let Some(resolved) = self.get_value(&Ustr::from(&candidate)) {
                    return Some(resolved);
                }
            }
        }
        None
    }

    fn resolve_struct_like_member(
        &mut self,
        owner: &str,
        member: &str,
        short_member: Option<&str>,
    ) -> Option<RuntimeValue> {
        let mut resolved: Option<Arc<VMFunction>> = None;

        for func in self.registry.functions.values() {
            if !func.name.contains(owner)
                || !(func.name.ends_with(&format!(".{member}"))
                    || short_member.is_some_and(|short| func.name.ends_with(&format!(".{short}"))))
            {
                continue;
            }

            if resolved.is_some() {
                return None;
            }

            resolved = Some(Arc::clone(func));
        }

        resolved.map(|func| self.make_runtime_function(&func))
    }

    #[inline]
    fn install_captures(
        &mut self,
        captures: &[(Ustr, RuntimeValue)],
    ) -> Vec<(Ustr, CaptureRestore)> {
        if captures.is_empty() {
            return Vec::new();
        }
        let mut prev_vars = Vec::with_capacity(captures.len() * 2);
        let mut install_one =
            |key: Ustr, value: &RuntimeValue, prev_vars: &mut Vec<(Ustr, CaptureRestore)>| {
                if let RuntimeValue::Ref(target) = value
                    && target == &key
                {
                    prev_vars.push((key, CaptureRestore::Keep));
                    return;
                }

                let old = self.variables.get(&key).cloned();

                if let RuntimeValue::VarRef(id) = value {
                    self.variables.bind_alias_by_id(key, *id);
                    prev_vars.push((
                        key,
                        if old.is_some() {
                            CaptureRestore::Value(old)
                        } else {
                            CaptureRestore::AliasOnly
                        },
                    ));
                } else {
                    prev_vars.push((
                        key,
                        CaptureRestore::Value(self.variables.insert(key, value.clone())),
                    ));
                }
            };

        for (name, value) in captures {
            install_one(*name, value, &mut prev_vars);
        }

        prev_vars
    }

    #[inline]
    fn should_install_capture(name: &str) -> bool {
        !(name == "true" || name == "false" || name == "null")
    }

    #[inline]
    fn restore_captures(&mut self, prev_vars: Vec<(Ustr, CaptureRestore)>) {
        for (name, old) in prev_vars {
            match old {
                CaptureRestore::Value(Some(value)) => {
                    self.variables.insert(name, value);
                }
                CaptureRestore::Value(None) => {
                    self.variables.remove(&name);
                }
                CaptureRestore::AliasOnly => {
                    let _ = self.variables.remove_name_only(&name);
                }
                CaptureRestore::Keep => {}
            }
        }
    }

    #[inline(always)]
    fn call_arg_from_frame_reg(&self, frame: usize, reg: u16) -> RuntimeValue {
        match self.get_reg_value_in_frame(frame, reg) {
            RuntimeValue::RegRef { frame, reg } => {
                if let Ok(resolved) = self.resolve_value(RuntimeValue::RegRef {
                    frame: *frame,
                    reg: *reg,
                }) {
                    match resolved {
                        value if value.should_pass_by_reg_ref() => RuntimeValue::RegRef {
                            frame: *frame,
                            reg: *reg,
                        },
                        other => other,
                    }
                } else {
                    RuntimeValue::RegRef {
                        frame: *frame,
                        reg: *reg,
                    }
                }
            }
            RuntimeValue::Ref(name) => {
                if let Ok(resolved) = self.resolve_value(RuntimeValue::Ref(*name)) {
                    if resolved.should_pass_by_reg_ref() {
                        RuntimeValue::Ref(*name)
                    } else {
                        resolved
                    }
                } else {
                    RuntimeValue::Ref(*name)
                }
            }
            RuntimeValue::VarRef(id) => {
                if let Ok(resolved) = self.resolve_value(RuntimeValue::VarRef(*id)) {
                    if resolved.should_pass_by_reg_ref() {
                        RuntimeValue::VarRef(*id)
                    } else {
                        resolved
                    }
                } else {
                    RuntimeValue::VarRef(*id)
                }
            }
            other => other.clone(),
        }
    }

    #[inline]
    fn collect_call_args_vec(&self, args: &[u16]) -> Vec<RuntimeValue> {
        let frame = self.frames.len().saturating_sub(1);

        args.iter()
            .map(|reg| self.call_arg_from_frame_reg(frame, *reg))
            .collect()
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
                ParserInnerType::DynamicTraits(traits) => {
                    traits.iter().all(|tr| constraints.iter().any(|x| x == tr))
                }
                _ => self.runtime_matches_type(inner.as_ref(), target),
            };
        }

        match target {
            ParserInnerType::Dynamic => true,
            ParserInnerType::DynamicTraits(traits) => match value {
                RuntimeValue::DynObject { constraints, .. } => {
                    traits.iter().all(|tr| constraints.iter().any(|x| x == tr))
                }
                other => self
                    .build_dyn_vtable_for_value(
                        other,
                        &traits.iter().map(|x| Ustr::from(x)).collect::<Vec<_>>(),
                    )
                    .is_some(),
            },
            ParserInnerType::Auto(_) => true,
            ParserInnerType::Ref(inner, _) => self.runtime_matches_type(value, &inner.data_type),
            ParserInnerType::Big => matches!(value, RuntimeValue::Big(_)),
            ParserInnerType::Float => matches!(value, RuntimeValue::Float(_)),
            ParserInnerType::Int => matches!(value, RuntimeValue::Int(_)),
            ParserInnerType::UInt => matches!(value, RuntimeValue::UInt(_)),
            ParserInnerType::Host => matches!(value, RuntimeValue::Host(_)),
            ParserInnerType::Gen(_) => matches!(value, RuntimeValue::Generator { .. }),
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
            ParserInnerType::Function { .. } | ParserInnerType::NativeFunction { .. } => {
                let val = matches!(
                    value,
                    RuntimeValue::Function { .. } | RuntimeValue::NativeFunction(_)
                );

                #[cfg(feature = "native")]
                {
                    val || matches!(value, RuntimeValue::ExternFunction(_))
                }
                #[cfg(not(feature = "native"))]
                val
            }
            ParserInnerType::Struct(identifier)
            | ParserInnerType::StructWithGenerics { identifier, .. } => match value {
                RuntimeValue::Aggregate(Some(actual), _) | RuntimeValue::Enum(actual, _, _) => {
                    actual == identifier
                }
                RuntimeValue::Generator { type_name, .. } => identifier == type_name.as_str(),
                _ => false,
            },
            ParserInnerType::Scope(_)
            | ParserInnerType::DollarIdentifier(_)
            | ParserInnerType::FfiType(_) => false,
        }
    }

    fn resolve_callable_cached(
        &mut self,
        name: Ustr,
        callsite: (usize, usize, u32),
    ) -> Option<Arc<VMFunction>> {
        if let Some(cached) = self.caches.callsite.get(&callsite)
            && cached.name == name
        {
            return Some(Arc::clone(cached));
        }

        if let Some(cached) = self.caches.call.get(&name) {
            let resolved = Arc::clone(cached);
            self.caches.callsite.insert(callsite, Arc::clone(&resolved));
            return Some(resolved);
        }

        let found = self.resolve_function_by_name(&name);

        if let Some(ref func) = found {
            let cached = Arc::clone(func);
            self.caches.call.insert(name, Arc::clone(&cached));
            self.caches.callsite.insert(callsite, cached);
        }

        found
    }

    #[inline]
    fn get_value(&self, name: &Ustr) -> Option<RuntimeValue> {
        if let Some(native) = RuntimeValue::natives().get(name.as_str()) {
            return Some(native.clone());
        }

        if let Some(func) = self.get_function_ref(name) {
            return Some(self.make_runtime_function(func));
        }

        self.variables
            .get(name)
            .map(|var| self.resolve_saveable_runtime_value_ref(var))
    }

    #[inline]
    fn remove_value(&mut self, name: &Ustr) -> Option<RuntimeValue> {
        if let Some(func) = self.get_function_ref(name) {
            return Some(self.make_runtime_function(func));
        }

        self.variables
            .remove(name)
            .map(|var| self.resolve_saveable_runtime_value(var))
    }

    pub fn run(
        &mut self,
        function: &VMFunction,
        args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, RuntimeError> {
        self.run_globals()?;
        self.run_function(function, args, Self::empty_captures(), true)
    }

    #[instrument(skip_all, fields(count = self.registry.globals.len()))]
    pub fn run_globals(&mut self) -> Result<(), RuntimeError> {
        if self.registry.globals.is_empty() {
            return Ok(());
        }

        let registry = Arc::clone(&self.registry);
        self.in_global = true;

        for (name, global) in registry.globals.iter() {
            if !self.registry.functions.contains_key(name) && !self.variables.contains_key(name) {
                self.run_global(global)?;
            }
        }

        self.in_global = false;

        Ok(())
    }

    #[instrument(skip_all, fields(entry = ?global.entry))]
    pub fn run_global(&mut self, global: &VMGlobal) -> Result<RuntimeValue, RuntimeError> {
        debug!("running global");

        let entry = global
            .block_map
            .get(&global.entry)
            .copied()
            .ok_or_else(|| RuntimeError::InvalidBytecode("global has no blocks".to_string()))?;

        let mut block = global
            .blocks
            .get(entry)
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

    #[instrument(skip_all)]
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

    #[inline]
    pub fn run_block(
        &mut self,
        block: &VMBlock,
        prev: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        self.run_block_with_budget(block, prev, 0, None)
    }

    #[instrument(skip_all)]
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
            tracing::trace!(ip, instruction = ?instruction, "executing instruction");
            if (ip & 0x3f) == 0 {
                self.maybe_collect_garbage();
            }

            let step = match instruction.run(self, block, ip as u32, prev) {
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
