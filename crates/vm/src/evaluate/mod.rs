use crate::{
    VM, VarName,
    conversion::{VMBlock, VMFunction, VMGlobal, VMInstruction, VMLiteral},
    error::RuntimeError,
    value::{
        RuntimeValue, TerminateValue, WaitGroupInner,
        operation::{binary, boolean, comparison},
    },
};
use calibre_lir::ast::BlockId;
use calibre_parser::ast::types::ParserInnerType;
use calibre_parser::ast::{ObjectMap, idents::ParserText};
use dumpster::sync::Gc;
use std::sync::Arc;
use tracing::{debug, instrument, trace};
use ustr::{Ustr, UstrMap, UstrSet};

mod instruction;
mod tailcall;

#[derive(Debug)]
enum CaptureRestore {
    Value(Option<RuntimeValue>),
    AliasOnly,
    Keep,
}

impl VM {
    fn recover_member_source_for_list(&self, reg: u16, frame_idx: usize) -> Option<(u16, Ustr)> {
        if let Some(source) = self
            .frames
            .get(frame_idx)?
            .member_sources
            .get(&reg)
            .cloned()
        {
            return Some(source);
        }

        let RuntimeValue::List(target_list) = self.get_reg_value_in_frame(frame_idx, reg) else {
            return None;
        };

        self.frames.get(frame_idx)?.member_sources.iter().find_map(
            |(candidate_reg, candidate_source)| {
                if *candidate_reg == reg {
                    return None;
                }
                if let RuntimeValue::List(other_list) =
                    self.get_reg_value_in_frame(frame_idx, *candidate_reg)
                    && std::ptr::eq(other_list.as_ref(), target_list.as_ref())
                {
                    return Some(candidate_source.clone());
                }
                None
            },
        )
    }

    #[instrument(skip_all)]
    fn write_back_runtime_value(&mut self, target: RuntimeValue, value: RuntimeValue) {
        self.write_back_runtime_value_with_depth(target, value, 32)
    }

    fn write_back_runtime_value_with_depth(
        &mut self,
        target: RuntimeValue,
        value: RuntimeValue,
        depth: usize,
    ) {
        if depth <= 0 {
            trace!("write_back_runtime_value exceeded max depth, forcing write");
            match target {
                RuntimeValue::Ref(name) => {
                    self.variables.insert(name, value);
                }
                RuntimeValue::VarRef(id) => {
                    let _ = self.variables.set_by_id(id, value);
                }
                _ => {}
            }
            return;
        }

        trace!("writing back runtime value at depth {}", depth);
        match target {
            RuntimeValue::Ref(name) => {
                if let Some(current) = self.variables.get(&name).cloned() {
                    match current {
                        RuntimeValue::Ref(_)
                        | RuntimeValue::VarRef(_)
                        | RuntimeValue::RegRef { .. } => {
                            self.write_back_runtime_value_with_depth(current, value, depth - 1);
                        }
                        _ => {
                            self.variables.insert(name, value);
                        }
                    }
                } else {
                    self.variables.insert(name, value);
                }
            }
            RuntimeValue::VarRef(id) => {
                if let Some(current) = self.variables.get_by_id(id).cloned() {
                    match current {
                        RuntimeValue::Ref(_)
                        | RuntimeValue::VarRef(_)
                        | RuntimeValue::RegRef { .. } => {
                            self.write_back_runtime_value_with_depth(current, value, depth - 1);
                        }
                        _ => {
                            let _ = self.variables.set_by_id(id, value);
                        }
                    }
                } else {
                    let _ = self.variables.set_by_id(id, value);
                }
            }
            RuntimeValue::RegRef { frame, reg } => {
                let current = self.get_reg_value_in_frame(frame, reg).clone();
                match current {
                    RuntimeValue::Ref(_)
                    | RuntimeValue::VarRef(_)
                    | RuntimeValue::RegRef { .. } => {
                        self.write_back_runtime_value(current, value);
                    }
                    _ => self.set_reg_value_in_frame(frame, reg, value),
                }
            }
            _ => {}
        }
    }

    #[instrument(skip_all, fields(args_count = args.len()))]
    fn propagate_member_source_args(
        &mut self,
        args: &[u16],
        caller_frame: usize,
    ) -> Result<(), RuntimeError> {
        let propagated_args: Vec<(usize, u16, u16, Ustr)> = args
            .iter()
            .filter_map(|arg_reg| {
                let arg_val = self.get_reg_value_in_frame(caller_frame, *arg_reg);
                let RuntimeValue::RegRef { frame, reg } = arg_val else {
                    return None;
                };
                if *frame != caller_frame {
                    return None;
                }
                let (parent_reg, field) = self
                    .frames
                    .get(caller_frame)?
                    .member_sources
                    .get(&reg)
                    .cloned()?;
                Some((caller_frame, *reg, parent_reg, field))
            })
            .collect();

        for (frame_idx, field_reg, parent_reg, field_name) in propagated_args {
            self.write_back_member_field_update(frame_idx, field_reg, parent_reg, &field_name)?;
        }

        Ok(())
    }

    #[instrument(skip_all, fields(frame_idx = frame_idx, field_reg = field_reg, parent_reg = parent_reg, field_name = %field_name))]
    fn write_back_member_field_update(
        &mut self,
        frame_idx: usize,
        field_reg: u16,
        parent_reg: u16,
        field_name: &Ustr,
    ) -> Result<(), RuntimeError> {
        let updated_field = self.get_reg_value_in_frame(frame_idx, field_reg);
        let parent_raw = self.get_reg_value_in_frame(frame_idx, parent_reg);
        let parent_resolved = self.resolve_value_for_op_ref(&parent_raw)?;

        fn update(
            value: RuntimeValue,
            field: &str,
            replacement: &RuntimeValue,
        ) -> Option<RuntimeValue> {
            match value {
                RuntimeValue::Aggregate(type_name, mut map) => {
                    let entries = &mut Gc::make_mut(&mut map).0.0;

                    if let Some(entry) = entries.iter_mut().find(|(name, _)| name == field) {
                        entry.1 = replacement.clone();
                        return Some(RuntimeValue::Aggregate(type_name, map));
                    }

                    let (_, wrapped) = entries.iter_mut().find(|(name, _)| name == "0")?;

                    let nested = update(wrapped.clone(), field, replacement)?;
                    *wrapped = nested;

                    Some(RuntimeValue::Aggregate(type_name, map))
                }
                RuntimeValue::List(mut list) => {
                    let values = &mut Gc::make_mut(&mut list).0;
                    let mut changed = false;

                    for item in values.iter_mut() {
                        if let Some(nested) = update(item.clone(), field, replacement) {
                            *item = nested;
                            changed = true;
                        }
                    }

                    changed.then_some(RuntimeValue::List(list))
                }
                _ => None,
            }
        }

        let leaf = field_name.rsplit('.').next().unwrap_or(field_name);
        let leaf = leaf.rsplit_once(']').map(|(_, name)| name).unwrap_or(leaf);
        if let Some(updated_parent) = update(parent_resolved, leaf, &updated_field) {
            let parent_source = self
                .frames
                .get(frame_idx)
                .and_then(|frame| frame.member_sources.get(&parent_reg))
                .cloned();
            match parent_raw {
                RuntimeValue::Ref(_) | RuntimeValue::VarRef(_) | RuntimeValue::RegRef { .. } => {
                    self.write_back_runtime_value(parent_raw.clone(), updated_parent);
                }
                _ => {
                    self.set_reg_value_in_frame(frame_idx, parent_reg, updated_parent.clone());

                    if let Some(source) = parent_source {
                        if let Some(frame) = self.frames.get_mut(frame_idx) {
                            frame.member_sources.insert(parent_reg, source);
                        }
                        self.propagate_member_source_reg(parent_reg, frame_idx)?;
                    }
                }
            }
        }
        Ok(())
    }

    fn propagate_member_source_reg(
        &mut self,
        reg: u16,
        frame_idx: usize,
    ) -> Result<(), RuntimeError> {
        let Some((parent_reg, field_name)) = self.recover_member_source_for_list(reg, frame_idx)
        else {
            return Ok(());
        };

        self.write_back_member_field_update(frame_idx, reg, parent_reg, &field_name)?;
        Ok(())
    }

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
            if let Some((resolved, _)) = self.resolve_runtime_value(&Ustr::from(&candidate)) {
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

    #[instrument(skip_all, fields(args_count = args.len(), callsite_block = callsite_block))]
    pub(crate) fn call_runtime_callable_at(
        &mut self,
        callable: RuntimeValue,
        args: Vec<RuntimeValue>,
        callsite_block: usize,
        callsite_tag: u32,
    ) -> Result<RuntimeValue, RuntimeError> {
        trace!("calling runtime callable");
        match self.resolve_value_for_op_ref(&callable)? {
            RuntimeValue::Function { name, captures } => {
                let callsite = (self.current_frame().func_ptr, callsite_block, callsite_tag);
                let Some(func) = self.resolve_callable_cached(name, callsite) else {
                    return Err(RuntimeError::FunctionNotFound(name.to_string()));
                };
                let mut seen = UstrSet::default();
                let mut refreshed_caps = Vec::with_capacity(captures.len());
                let mut seen_names = UstrSet::default();
                for (cap_name, old_value) in captures.iter() {
                    if !seen_names.insert(cap_name.clone()) {
                        continue;
                    }
                    let value = self.capture_value(cap_name, &mut seen);
                    let value = if matches!(value, RuntimeValue::Null)
                        && !matches!(old_value, RuntimeValue::Null)
                    {
                        old_value.clone()
                    } else {
                        value
                    };
                    refreshed_caps.push((cap_name.clone(), value));
                }
                let refreshed = Arc::new(refreshed_caps);
                self.run_function(func.as_ref(), args, refreshed)
            }
            RuntimeValue::NativeFunction(func) => func.run(self, args),
            #[cfg(feature = "native")]
            RuntimeValue::ExternFunction(func) => func.call(self, args),
            RuntimeValue::BoundMethod { callee, receiver } => {
                let mut full_args = vec![receiver.as_ref().clone()];
                full_args.extend(args);
                if full_args.len() >= 2 {
                    let same_identity = match (&full_args[0], &full_args[1]) {
                        (RuntimeValue::List(a), RuntimeValue::List(b)) => {
                            std::ptr::eq(a.as_ref(), b.as_ref())
                        }
                        (RuntimeValue::HashMap(a), RuntimeValue::HashMap(b)) => {
                            std::ptr::eq(a.as_ref(), b.as_ref())
                        }
                        (RuntimeValue::HashSet(a), RuntimeValue::HashSet(b)) => {
                            std::ptr::eq(a.as_ref(), b.as_ref())
                        }
                        _ => false,
                    };

                    if same_identity {
                        full_args.truncate(1);
                    }
                }
                self.call_runtime_callable_at(
                    *callee,
                    full_args,
                    callsite_block,
                    callsite_tag.saturating_sub(1),
                )
            }
            other => Err(RuntimeError::InvalidFunctionCallValue(Box::new(other))),
        }
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
            if let Some((resolved, _)) = self.resolve_runtime_value(&Ustr::from(&candidate)) {
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
                if let Some((resolved, _)) = self.resolve_runtime_value(&Ustr::from(&candidate)) {
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
                if let Ok(resolved) = self.resolve_value_for_op_ref(&RuntimeValue::RegRef {
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
                if let Ok(resolved) =
                    self.resolve_value_for_op_ref(&RuntimeValue::Ref(name.clone()))
                {
                    if resolved.should_pass_by_reg_ref() {
                        RuntimeValue::Ref(name.clone())
                    } else {
                        resolved
                    }
                } else {
                    RuntimeValue::Ref(name.clone())
                }
            }
            RuntimeValue::VarRef(id) => {
                if let Ok(resolved) = self.resolve_value_for_op_ref(&RuntimeValue::VarRef(*id)) {
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
    fn resolve_runtime_value(&self, name: &Ustr) -> Option<(RuntimeValue, String)> {
        if let Some(native) = RuntimeValue::natives().get(name.as_str()) {
            return Some((native.clone(), name.to_string()));
        }
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
    fn move_runtime_value(&mut self, name: Ustr) -> Option<RuntimeValue> {
        if let Some(func) = self.take_function(name) {
            return Some(self.make_runtime_function(&func));
        }
        self.variables
            .remove(&name)
            .map(|var| self.resolve_saveable_runtime_value_ref(&var))
    }

    #[instrument(skip_all, fields(function = %function.name, args = args.len()))]
    pub fn run(
        &mut self,
        function: &VMFunction,
        args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, RuntimeError> {
        self.run_globals()?;
        self.run_function(function, args, Self::empty_captures())
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

    #[instrument(skip_all, fields(function = %function.name))]
    pub fn run_function<I>(
        &mut self,
        function: &VMFunction,
        args: I,
        captures: Arc<Vec<(Ustr, RuntimeValue)>>,
    ) -> Result<RuntimeValue, RuntimeError>
    where
        I: IntoIterator<Item = RuntimeValue>,
    {
        trace!("entering function");
        self.in_global = false;
        let mut state = crate::TaskState::default();
        match self.run_function_with_budget(function, args, captures, usize::MAX, &mut state)? {
            Some(value) => Ok(value),
            None => Ok(RuntimeValue::Null),
        }
    }

    #[instrument(skip_all, fields(function = %function.name, args_count = args.len()))]
    #[inline]
    fn run_function_from_regs(
        &mut self,
        function: &VMFunction,
        args: &[u16],
        captures: Arc<Vec<(Ustr, RuntimeValue)>>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let caller_frame = self.frames.len().saturating_sub(1);

        let func_ptr = function as *const VMFunction as usize;
        self.push_frame(
            function.reg_count as usize,
            func_ptr,
            Some(function.name.clone()),
        );

        for (reg, arg_reg) in function.param_regs.iter().zip(args.iter().copied()) {
            let arg = self.get_reg_value_in_frame(caller_frame, arg_reg).clone();
            self.set_reg_value(*reg, arg);
        }

        if function.needs_param_vars {
            for (name, reg) in function
                .params
                .iter()
                .zip(function.param_regs.iter().copied())
            {
                let value = self.get_reg_value(reg).clone();
                let _ = self.variables.insert(*name, value);
            }
        }

        let prev_vars = if captures.is_empty() {
            Vec::new()
        } else {
            let filtered_captures: Vec<(Ustr, RuntimeValue)> = captures
                .iter()
                .filter(|(name, _)| {
                    Self::should_install_capture(name) && !function.param_names.contains(name)
                })
                .cloned()
                .collect();
            self.install_captures(filtered_captures.as_slice())
        };

        let mut block_id = function.entry;
        let entry =
            function.block_map.get(&block_id).copied().ok_or_else(|| {
                RuntimeError::InvalidBytecode("function has no blocks".to_string())
            })?;
        let mut block = function
            .blocks
            .get(entry)
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

        self.propagate_member_source_args(args, caller_frame)?;

        self.pop_frame();
        self.restore_captures(prev_vars);

        if function.needs_param_vars {
            for name in function.params.iter() {
                self.variables.remove(name);
            }
        }

        Ok(result)
    }

    #[instrument(skip_all, fields(function = %function.name, budget = budget))]
    pub fn run_function_with_budget<I>(
        &mut self,
        function: &VMFunction,
        args: I,
        captures: Arc<Vec<(Ustr, RuntimeValue)>>,
        budget: usize,
        state: &mut crate::TaskState,
    ) -> Result<Option<RuntimeValue>, RuntimeError>
    where
        I: IntoIterator<Item = RuntimeValue>,
    {
        trace!("running function with budget");
        state.yielded = None;
        let prev_vars = if state.block.is_none() {
            let func_ptr = function as *const VMFunction as usize;
            self.push_frame(
                function.reg_count as usize,
                func_ptr,
                Some(function.name.clone()),
            );
            for (reg, arg) in function.param_regs.iter().zip(args) {
                self.set_reg_value(*reg, arg.clone());
            }

            for (name, reg) in function
                .params
                .iter()
                .zip(function.param_regs.iter().copied())
            {
                let value = self.get_reg_value(reg).clone();
                let _ = self.variables.insert(*name, value);
            }

            let param_names: UstrSet = function.params.clone().into_iter().collect();
            let filtered_captures: Vec<(Ustr, RuntimeValue)> = captures
                .iter()
                .filter(|(name, _)| {
                    Self::should_install_capture(name) && !param_names.contains(name)
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

        if function.returns_value && !returned {
            result = self.get_reg_value(function.ret_reg).clone();
        }

        if let RuntimeValue::RegRef { frame, reg } = result {
            result = self.get_reg_value_in_frame(frame, reg).clone();
        }

        self.pop_frame();
        self.restore_captures(prev_vars);

        for name in function.params.iter() {
            self.variables.remove(name);
        }

        Ok(Some(result))
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
    #[instrument(skip_all)]
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
