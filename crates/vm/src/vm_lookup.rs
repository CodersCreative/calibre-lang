use super::*;

#[derive(Debug, Clone)]
pub(crate) enum VarName {
    Var(String),
    Func(String),
    Global,
}

impl VM {
    pub(crate) fn find_unique_function_by_suffix_ref(
        &self,
        short_name: &str,
    ) -> Option<&VMFunction> {
        let func = self.func_suffix.get(short_name)?.as_ref()?;
        (!self.moved_functions.contains(&func.name)).then(|| func.as_ref())
    }

    pub(crate) fn resolve_function_by_name(&self, name: &str) -> Option<Arc<VMFunction>> {
        if let Some(found) = self.registry.functions.get(name)
            && !self.moved_functions.contains(name)
        {
            return Some(found.clone());
        } else if let Some((prefix, _)) = name.split_once("->") {
            return self
                .registry
                .functions
                .iter()
                .filter(|(k, _)| !self.moved_functions.contains(*k))
                .find(|(k, _)| k.starts_with(prefix))
                .map(|(_, v)| Arc::clone(v));
        }
        if name.contains("::") {
            return None;
        }
        let short_name = calibre_parser::qualified_name_tail(name);
        if short_name == name {
            return None;
        }
        let func = self.func_suffix.get(short_name)?.as_ref()?;
        (!self.moved_functions.contains(&func.name)).then(|| func.clone())
    }

    pub(crate) fn find_unique_var_by_suffix(&self, short_name: &str) -> Option<String> {
        self.variables.keys().find_map(|name| {
            let suffix = calibre_parser::qualified_name_tail(name);
            (suffix == short_name).then(|| name.to_string())
        })
    }

    #[inline]
    pub(crate) fn find_unique_var_by_suffix_cached(&mut self, short_name: &str) -> Option<String> {
        if let Some(cached) = self.caches.unique_var_suffix.get(short_name) {
            return cached.clone();
        }
        let found = self.find_unique_var_by_suffix(short_name);
        self.caches
            .unique_var_suffix
            .insert(short_name.to_string(), found.clone());
        found
    }

    #[inline]
    pub(crate) fn invalidate_name_resolution_caches(&mut self) {
        self.caches.unique_var_suffix.clear();
    }

    pub(crate) fn resolve_library_candidates(name: &str) -> Vec<String> {
        let has_path = name.contains('/') || name.contains('\\');
        let lower = name.to_ascii_lowercase();
        let has_ext =
            lower.ends_with(".so") || lower.ends_with(".dylib") || lower.ends_with(".dll");

        if has_path || has_ext {
            return vec![name.to_string()];
        }

        let base = match name {
            "c" | "libc" => "c",
            other => other,
        };

        let mut out = Vec::new();

        #[cfg(target_os = "android")]
        {
            if base == "c" {
                out.push("libc.so".to_string());
            }
            out.push(format!("lib{}.so", base));
            out.push(format!("{}.so", base));
            out.push(base.to_string());
        }
        #[cfg(any(target_os = "linux", target_os = "android"))]
        {
            if base == "c" {
                out.push("libc.so.6".to_string());
            }
            out.push(format!("lib{}.so", base));
            out.push(format!("{}.so", base));
            out.push(base.to_string());
        }
        #[cfg(target_os = "macos")]
        {
            if base == "c" {
                out.push("libc.dylib".to_string());
                out.push("/usr/lib/libc.dylib".to_string());
            }
            out.push(format!("lib{}.dylib", base));
            out.push(format!("{}.dylib", base));
            out.push(base.to_string());
        }
        #[cfg(target_os = "windows")]
        {
            if base == "c" {
                out.push("msvcrt.dll".to_string());
            }
            out.push(format!("{}.dll", base));
            out.push(format!("lib{}.dll", base));
            out.push(base.to_string());
        }

        out.into_iter().filter(|c| !c.is_empty()).collect()
    }

    pub(crate) fn capture_value(&self, name: &str, seen: &mut FxHashSet<String>) -> RuntimeValue {
        let is_local_style = name.contains(':') || name.contains("->");

        if is_local_style {
            let current_frame = self.frames.len().saturating_sub(1);
            if let Some(id) = self.variables.id_of(name)
                && let Some(RuntimeValue::RegRef { frame, .. }) = self.variables.get_by_id(id)
                && *frame != current_frame
            {
                return RuntimeValue::VarRef(id);
            }
        }

        if is_local_style {
            for (frame_idx, frame) in self.frames.iter().enumerate().rev() {
                if let Some(reg) = frame.local_map.get(name).copied() {
                    return RuntimeValue::RegRef {
                        frame: frame_idx,
                        reg,
                    };
                }
                if let Some(base) = frame.local_map_base.as_ref() {
                    if let Some(reg) = base.get(name).copied() {
                        return RuntimeValue::RegRef {
                            frame: frame_idx,
                            reg,
                        };
                    }
                }
            }
        }

        if !is_local_style {
            let frame = self.current_frame();
            let target_tail = calibre_parser::qualified_name_tail(name);
            let mut found: Option<Reg> = None;
            let mut ambiguous = false;
            let tail_matches = |key: &Arc<str>, target: &str| {
                let local = key.as_ref();
                let colon_tail = local
                    .rsplit_once(':')
                    .map(|(_, tail)| tail)
                    .unwrap_or(local);
                calibre_parser::qualified_name_tail(colon_tail) == target
            };
            for (key, reg) in frame.local_map.iter() {
                if tail_matches(key, target_tail) {
                    if found.is_some() {
                        ambiguous = true;
                        break;
                    }
                    found = Some(*reg);
                }
            }
            if !ambiguous {
                for (key, reg) in frame
                    .local_map_base
                    .as_ref()
                    .into_iter()
                    .flat_map(|base| base.iter())
                {
                    if tail_matches(key, target_tail) {
                        if found.is_some() {
                            ambiguous = true;
                            break;
                        }
                        found = Some(*reg);
                    }
                }
            }
            if !ambiguous {
                if let Some(reg) = found {
                    let frame_idx = self.frames.len().saturating_sub(1);
                    return RuntimeValue::RegRef {
                        frame: frame_idx,
                        reg,
                    };
                }
            }
        }

        if let Some(id) = self.variables.id_of(name) {
            return RuntimeValue::VarRef(id);
        }

        if let Some(value) = self.variables.get(name) {
            return self.resolve_saveable_runtime_value_ref(value);
        }

        if is_local_style
            && let Ok(value) = self.resolve_value_for_op_ref(&RuntimeValue::Ref(name.to_string()))
            && !matches!(value, RuntimeValue::Null)
        {
            return self.resolve_saveable_runtime_value_ref(&value);
        }

        match self.resolve_var_name(name) {
            VarName::Var(var) => {
                if let Some(value) = self.variables.get(&var) {
                    self.resolve_saveable_runtime_value_ref(value)
                } else {
                    RuntimeValue::Null
                }
            }
            VarName::Func(func) => self
                .registry
                .functions
                .get(&func)
                .map(|f| self.make_runtime_function_inner(f, seen))
                .unwrap_or_else(|| RuntimeValue::Null),
            VarName::Global => {
                let short_name = calibre_parser::qualified_name_tail(name);
                if short_name != name
                    && let Some(func) = self.find_unique_function_by_suffix_ref(short_name)
                {
                    return self.make_runtime_function_inner(func, seen);
                }
                RuntimeValue::Null
            }
        }
    }

    pub(crate) fn capture_values(
        &self,
        captures: &[String],
        seen: &mut FxHashSet<String>,
    ) -> Vec<(String, RuntimeValue)> {
        let mut out = Vec::with_capacity(captures.len());
        let mut seen_names = FxHashSet::default();
        for name in captures {
            if !seen_names.insert(name.clone()) {
                continue;
            }
            out.push((name.clone(), self.capture_value(name, seen)));
        }
        out
    }

    pub(crate) fn make_runtime_function(&self, func: &VMFunction) -> RuntimeValue {
        let mut seen = FxHashSet::default();
        self.make_runtime_function_inner(func, &mut seen)
    }

    fn make_runtime_function_inner(
        &self,
        func: &VMFunction,
        seen: &mut FxHashSet<String>,
    ) -> RuntimeValue {
        let name = func.name.clone();
        if !seen.insert(name.clone()) {
            return RuntimeValue::Function {
                name: name.clone().into(),
                captures: std::sync::Arc::new(Vec::new()),
            };
        }
        RuntimeValue::Function {
            name: name.clone().into(),
            captures: std::sync::Arc::new(self.capture_values(&func.captures, seen)),
        }
    }

    #[inline]
    pub(crate) fn is_gen_type_name(type_name: &str) -> bool {
        let short = calibre_parser::qualified_name_tail(type_name);
        short == "gen" || short.starts_with("gen->")
    }

    pub(crate) fn resolve_aggregate_member_slot(
        &mut self,
        type_name: &str,
        map: &GcMap,
        name: &str,
        short_name: Option<&str>,
    ) -> Option<usize> {
        let _ = type_name;
        map.0.0.iter().enumerate().find_map(|(idx, (field, _))| {
            if field == name || short_name.is_some_and(|short| field == short) {
                Some(idx)
            } else {
                None
            }
        })
    }

    pub(crate) fn resolve_var_name(&self, name: &str) -> VarName {
        let resolve_by_suffix = |short_name: &str| {
            self.find_unique_var_by_suffix(short_name)
                .map(VarName::Var)
                .or_else(|| {
                    self.find_unique_function_by_suffix_ref(short_name)
                        .map(|f| VarName::Func(f.name.clone()))
                })
        };

        if self.get_function_ref(name).is_some() {
            return VarName::Func(name.to_string());
        } else if self.variables.contains_key(name) {
            return VarName::Var(name.to_string());
        } else if name.contains('.') && !name.contains("::") {
            let normalized = name.replace('.', "::");
            if self.get_function_ref(&normalized).is_some() {
                return VarName::Func(normalized);
            }
            if self.variables.contains_key(&normalized) {
                return VarName::Var(normalized);
            }
        } else if name.contains("::") {
            return VarName::Global;
        }

        let short_name = calibre_parser::qualified_name_tail(name);
        if short_name != name {
            if let Some(resolved) = resolve_by_suffix(short_name) {
                return resolved;
            };
        } else if let Some(resolved) = resolve_by_suffix(name) {
            return resolved;
        }

        VarName::Global
    }

    #[inline]
    pub(crate) fn global_id_cached(&mut self, name: &str) -> Option<usize> {
        if let Some(id) = self.caches.globals_id.get(name).copied() {
            return Some(id);
        }
        let id = self.variables.id_of(name)?;
        self.caches.globals_id.insert(name.to_string(), id);
        Some(id)
    }

    #[inline]
    fn checked_local_string_idx(&self, block: &VMBlock, idx: u16) -> Result<usize, RuntimeError> {
        let idx = idx as usize;
        if idx < block.local_strings.len() {
            return Ok(idx);
        }
        Err(RuntimeError::InvalidBytecode(format!(
            "missing string {}",
            idx
        )))
    }

    pub(crate) fn local_string<'a>(
        &self,
        block: &'a VMBlock,
        idx: u16,
    ) -> Result<&'a str, RuntimeError> {
        let idx = self.checked_local_string_idx(block, idx)?;
        Ok(block.local_strings[idx].as_str())
    }
}
