use calibre_lir::ast::BlockId;
use calibre_parser::ast::binary::BinaryOperator;
use calibre_parser::ast::comparison::ComparisonOperator;
use calibre_parser::ast::types::ParserInnerType;
use calibre_parser::ast::{ObjectMap, idents::ParserText};
use dumpster::sync::Gc;
use rustc_hash::{FxHashMap, FxHashSet};
use smallvec::SmallVec;
use std::sync::Arc;

use crate::{
    VM, VarName,
    conversion::{VMBlock, VMFunction, VMGlobal, VMInstruction, VMLiteral},
    error::RuntimeError,
    value::{
        RuntimeValue, TerminateValue, WaitGroupInner,
        operation::{binary, boolean, comparison},
    },
};

mod instruction;

#[derive(Debug)]
enum CaptureRestore {
    Value(Option<RuntimeValue>),
    AliasOnly,
    Keep,
}

impl VM {
    fn recover_member_source_for_list(&self, reg: u16, frame_idx: usize) -> Option<(u16, String)> {
        if let Some(source) = self
            .frames
            .get(frame_idx)?
            .member_sources
            .get(&reg)
            .cloned()
        {
            return Some(source);
        }

        let RuntimeValue::List(target_list) = self.get_reg_value_in_frame(frame_idx, reg).clone()
        else {
            return None;
        };

        self.frames.get(frame_idx)?.member_sources.iter().find_map(
            |(candidate_reg, candidate_source)| {
                if *candidate_reg == reg {
                    return None;
                }
                if let RuntimeValue::List(other_list) = self
                    .get_reg_value_in_frame(frame_idx, *candidate_reg)
                    .clone()
                    && std::ptr::eq(other_list.as_ref(), target_list.as_ref())
                {
                    return Some(candidate_source.clone());
                }
                None
            },
        )
    }

    fn write_back_runtime_value(&mut self, target: RuntimeValue, value: RuntimeValue) {
        match target {
            RuntimeValue::Ref(name) => {
                if let Some(current) = self.variables.get(&name).cloned() {
                    match current {
                        RuntimeValue::Ref(_)
                        | RuntimeValue::VarRef(_)
                        | RuntimeValue::RegRef { .. } => {
                            self.write_back_runtime_value(current, value);
                        }
                        _ => {
                            self.variables.insert(&name, value);
                        }
                    }
                } else {
                    self.variables.insert(&name, value);
                }
            }
            RuntimeValue::VarRef(id) => {
                if let Some(current) = self.variables.get_by_id(id).cloned() {
                    match current {
                        RuntimeValue::Ref(_)
                        | RuntimeValue::VarRef(_)
                        | RuntimeValue::RegRef { .. } => {
                            self.write_back_runtime_value(current, value);
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

    fn propagate_member_source_args(
        &mut self,
        args: &[u16],
        caller_frame: usize,
    ) -> Result<(), RuntimeError> {
        let propagated_args: Vec<(usize, u16, u16, String)> = args
            .iter()
            .filter_map(|arg_reg| {
                let arg_val = self.get_reg_value_in_frame(caller_frame, *arg_reg).clone();
                let RuntimeValue::RegRef { frame, reg } = arg_val else {
                    return None;
                };
                if frame != caller_frame {
                    return None;
                }
                let (parent_reg, field) = self
                    .frames
                    .get(caller_frame)?
                    .member_sources
                    .get(&reg)
                    .cloned()?;
                Some((caller_frame, reg, parent_reg, field))
            })
            .collect();

        for (frame_idx, field_reg, parent_reg, field_name) in propagated_args {
            self.write_back_member_field_update(frame_idx, field_reg, parent_reg, &field_name)?;
        }

        Ok(())
    }

    fn write_back_member_field_update(
        &mut self,
        frame_idx: usize,
        field_reg: u16,
        parent_reg: u16,
        field_name: &str,
    ) -> Result<(), RuntimeError> {
        let updated_field = self.get_reg_value_in_frame(frame_idx, field_reg).clone();
        let parent_raw = self.get_reg_value_in_frame(frame_idx, parent_reg).clone();
        let parent_resolved = self.resolve_value_for_op_ref(&parent_raw)?;
        if let RuntimeValue::Aggregate(type_name, mut map) = parent_resolved
            && let Some(entry) = Gc::make_mut(&mut map)
                .0
                .0
                .iter_mut()
                .find(|(field, _)| field == field_name)
        {
            entry.1 = updated_field;
            let updated_parent = RuntimeValue::Aggregate(type_name, map);
            let parent_source = self
                .frames
                .get(frame_idx)
                .and_then(|frame| frame.member_sources.get(&parent_reg))
                .cloned();
            match parent_raw {
                RuntimeValue::Ref(_) | RuntimeValue::VarRef(_) | RuntimeValue::RegRef { .. } => {
                    self.write_back_runtime_value(parent_raw, updated_parent);
                }
                _ => {
                    self.set_reg_value_in_frame(frame_idx, parent_reg, updated_parent.clone());
                    self.sync_local_reg_value(frame_idx, parent_reg, updated_parent);
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
            if let Some((resolved, _)) = self.resolve_runtime_value(&candidate) {
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
                let mut seen = FxHashSet::default();
                let mut refreshed_caps = Vec::with_capacity(captures.len());
                let mut seen_names = FxHashSet::default();
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
            RuntimeValue::ExternFunction(func) => func.call(self, args),
            RuntimeValue::Channel(_) => self.call_runtime_callable_at(
                RuntimeValue::NativeFunction(Arc::new(
                    crate::native::stdlib::r#async::ChannelSend(),
                )),
                {
                    let mut full_args = Vec::with_capacity(args.len() + 1);
                    full_args.push(callable);
                    full_args.extend(args);
                    full_args
                },
                callsite_block,
                callsite_tag,
            ),
            RuntimeValue::BoundMethod { callee, receiver } => {
                let mut full_args = vec![receiver.as_ref().clone()];
                full_args.extend(args);
                if full_args.len() >= 2 {
                    let same_identity = match (&full_args[0], &full_args[1]) {
                        (RuntimeValue::List(a), RuntimeValue::List(b)) => {
                            std::ptr::eq(a.as_ref(), b.as_ref())
                        }
                        (RuntimeValue::Aggregate(_, a), RuntimeValue::Aggregate(_, b)) => {
                            std::ptr::eq(a.as_ref(), b.as_ref())
                        }
                        _ => false,
                    };
                    let same_value = matches!(
                        comparison(
                            &ComparisonOperator::Equal,
                            full_args[0].clone(),
                            full_args[1].clone()
                        ),
                        Ok(RuntimeValue::Bool(true))
                    );
                    if same_identity || same_value {
                        full_args.remove(1);
                    }
                }
                self.call_runtime_callable_at(
                    *callee,
                    full_args,
                    callsite_block,
                    callsite_tag.saturating_sub(1),
                )
            }
            other => {
                if matches!(other, RuntimeValue::Channel(_)) {
                    self.call_runtime_callable_at(
                        RuntimeValue::NativeFunction(Arc::new(
                            crate::native::stdlib::r#async::ChannelSend(),
                        )),
                        args,
                        callsite_block,
                        callsite_tag,
                    )
                } else {
                    Err(RuntimeError::InvalidFunctionCallValue(other))
                }
            }
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

    pub fn resolve_display_override(
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

    pub fn invoke_display_override(
        &mut self,
        callable: RuntimeValue,
        receiver: RuntimeValue,
    ) -> Option<String> {
        let output =
            self.invoke_callable_value(callable, vec![receiver], u32::MAX.saturating_sub(1))?;

        match output {
            RuntimeValue::Str(s) => Some(s.lock().unwrap().to_string()),
            other => Some(other.display(self)),
        }
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
            RuntimeValue::Channel(_) => Some("Channel".to_string()),
            RuntimeValue::WaitGroup(_) => Some("WaitGroup".to_string()),
            RuntimeValue::Mutex(_) | RuntimeValue::MutexGuard(_) => Some("Mutex".to_string()),
            RuntimeValue::HashMap(_) => Some("HashMap".to_string()),
            RuntimeValue::HashSet(_) => Some("HashSet".to_string()),
            RuntimeValue::TcpStream(_) => Some("TcpStream".to_string()),
            RuntimeValue::TcpListener(_) => Some("TcpListener".to_string()),
            RuntimeValue::Generator { type_name, .. } => Some(type_name.to_string()),
            RuntimeValue::DynObject { type_name, .. } => Some(type_name.to_string()),
            _ => None,
        }
    }

    fn lookup_dyn_trait_table(
        &self,
        concrete: &str,
        trait_name: &str,
    ) -> Option<&FxHashMap<String, String>> {
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
            if let Some((resolved, _)) = self.resolve_runtime_value(&candidate) {
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
                if let Some((resolved, _)) = self.resolve_runtime_value(&candidate) {
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
        captures: &[(String, RuntimeValue)],
    ) -> Vec<(String, CaptureRestore)> {
        if captures.is_empty() {
            return Vec::new();
        }
        let mut prev_vars = Vec::with_capacity(captures.len() * 2);
        let mut install_one =
            |key: &str, value: &RuntimeValue, prev_vars: &mut Vec<(String, CaptureRestore)>| {
                if let RuntimeValue::Ref(target) = value
                    && target == key
                {
                    prev_vars.push((key.to_string(), CaptureRestore::Keep));
                    return;
                }

                let old = self.variables.get(key).cloned();

                if let RuntimeValue::VarRef(id) = value {
                    self.variables.bind_alias_by_id(key, *id);
                    prev_vars.push((
                        key.to_string(),
                        if old.is_some() {
                            CaptureRestore::Value(old)
                        } else {
                            CaptureRestore::AliasOnly
                        },
                    ));
                } else {
                    prev_vars.push((
                        key.to_string(),
                        CaptureRestore::Value(self.variables.insert(key, value.clone())),
                    ));
                }
            };

        for (name, value) in captures {
            install_one(name, value, &mut prev_vars);
        }

        prev_vars
    }

    #[inline]
    fn should_install_capture(name: &str) -> bool {
        !(name == "true" || name == "false" || name == "null")
    }

    #[inline]
    fn restore_captures(&mut self, prev_vars: Vec<(String, CaptureRestore)>) {
        for (name, old) in prev_vars {
            match old {
                CaptureRestore::Value(Some(value)) => {
                    self.variables.insert(&name, value);
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

        args.into_iter()
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
            ParserInnerType::Struct(identifier)
            | ParserInnerType::StructWithGenerics { identifier, .. } => match value {
                RuntimeValue::Aggregate(Some(actual), _) | RuntimeValue::Enum(actual, _, _) => {
                    actual == identifier
                }
                RuntimeValue::Generator { type_name, .. } => type_name == type_name,
                _ => false,
            },
            ParserInnerType::Scope(_)
            | ParserInnerType::DollarIdentifier(_)
            | ParserInnerType::FfiType(_) => false,
        }
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

    #[inline]
    fn resolve_runtime_value(&self, name: &str) -> Option<(RuntimeValue, String)> {
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
    fn move_runtime_value(&mut self, name: &str) -> Option<RuntimeValue> {
        if let Some(func) = self.take_function(name) {
            return Some(self.make_runtime_function(&func));
        }
        self.variables
            .remove(name)
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
        for (name, global) in registry.globals.iter() {
            if registry.functions.contains_key(name) {
                continue;
            }
            self.run_global(global)?;
        }
        Ok(())
    }

    pub fn run_global(&mut self, global: &VMGlobal) -> Result<RuntimeValue, RuntimeError> {
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

    pub fn run_function<I>(
        &mut self,
        function: &VMFunction,
        args: I,
        captures: Arc<Vec<(String, RuntimeValue)>>,
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
        captures: Arc<Vec<(String, RuntimeValue)>>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let caller_frame = self.frames.len().saturating_sub(1);

        let func_ptr = function as *const VMFunction as usize;
        self.push_frame(
            function.reg_count as usize,
            func_ptr,
            Some(function.name.clone()),
        );

        for (reg, arg_reg) in function.param_regs.iter().zip(args.iter().copied()) {
            let arg = self.call_arg_from_frame_reg(caller_frame, arg_reg);
            self.set_reg_value(*reg, arg);
        }

        let base = self.local_map_base_for(function);
        let param_names: std::collections::HashSet<&str> =
            function.params.iter().map(|x| x.as_str()).collect();
        let filtered_captures: Vec<(String, RuntimeValue)> = captures
            .iter()
            .filter(|(name, _)| {
                Self::should_install_capture(name) && !param_names.contains(name.as_str())
            })
            .cloned()
            .collect();

        let prev_vars = self.install_captures(filtered_captures.as_slice());

        let frame = self.current_frame_mut();
        frame.local_map_base = Some(base);

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

    pub fn run_function_with_budget<I>(
        &mut self,
        function: &VMFunction,
        args: I,
        captures: Arc<Vec<(String, RuntimeValue)>>,
        budget: usize,
        state: &mut crate::TaskState,
    ) -> Result<Option<RuntimeValue>, RuntimeError>
    where
        I: IntoIterator<Item = RuntimeValue>,
    {
        state.yielded = None;
        let prev_vars = if state.block.is_none() {
            let func_ptr = function as *const VMFunction as usize;
            self.push_frame(
                function.reg_count as usize,
                func_ptr,
                Some(function.name.clone()),
            );
            for (reg, arg) in function.param_regs.iter().zip(args) {
                self.set_reg_value(*reg, arg);
            }
            let base = self.local_map_base_for(function);
            let param_names: std::collections::HashSet<&str> =
                function.params.iter().map(|x| x.as_str()).collect();
            let filtered_captures: Vec<(String, RuntimeValue)> = captures
                .iter()
                .filter(|(name, _)| {
                    Self::should_install_capture(name) && !param_names.contains(name.as_str())
                })
                .cloned()
                .collect();
            let frame = self.current_frame_mut();
            frame.local_map_base = Some(base);
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
            if let Some(name) = phi.name.as_ref() {
                let interned = self.intern_name(name);
                let frame = self.current_frame_mut();
                if let Some(mapped) = frame.local_map.get_mut(&interned) {
                    *mapped = phi.dest;
                }
            } else {
                let frame = self.current_frame_mut();
                for (name, mapped) in frame.local_map.iter_mut() {
                    let key = name.as_ref();
                    if !ParserText::is_temp_name(&key) || *mapped != reg {
                        continue;
                    }
                    *mapped = phi.dest;
                }
            }
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
