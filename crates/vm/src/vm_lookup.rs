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
        }
        if let Some((prefix, _)) = name.split_once("->") {
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
        let (_, short_name) = name.rsplit_once(':')?;
        let func = self.func_suffix.get(short_name)?.as_ref()?;
        (!self.moved_functions.contains(&func.name)).then(|| func.clone())
    }

    pub(crate) fn find_unique_var_by_suffix(&self, short_name: &str) -> Option<String> {
        self.variables.keys().find_map(|name| {
            let (_, suffix) = name.rsplit_once(':')?;
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

    fn find_unique_local_by_suffix(&self, short_name: &str) -> Option<Reg> {
        let frame = self.current_frame();
        frame
            .local_map_base
            .as_ref()
            .into_iter()
            .flat_map(|base| base.iter())
            .chain(frame.local_map.iter())
            .find_map(|(name, reg)| {
                let (_, suffix) = name.as_ref().rsplit_once(':')?;
                (suffix == short_name).then_some(*reg)
            })
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

    fn capture_value(&self, name: &str, seen: &mut FxHashSet<String>) -> RuntimeValue {
        if let Some(reg) = self.current_frame().local_map.get(name).copied() {
            return self.get_reg_value(reg);
        }
        if let Some(base) = self.current_frame().local_map_base.as_ref()
            && let Some(reg) = base.get(name).copied()
        {
            return self.get_reg_value(reg);
        }
        if let Some((_, short)) = name.rsplit_once(':')
            && let Some(reg) = self.find_unique_local_by_suffix(short)
        {
            return self.get_reg_value(reg);
        }

        match self.resolve_var_name(name) {
            VarName::Var(var) => self
                .variables
                .get(&var)
                .map(|v| self.resolve_saveable_runtime_value_ref(v))
                .unwrap_or(RuntimeValue::Null),
            VarName::Func(func) => self
                .registry
                .functions
                .get(&func)
                .map(|f| self.make_runtime_function_inner(f, seen))
                .unwrap_or(RuntimeValue::Null),
            VarName::Global => {
                if let Some((_, short_name)) = name.rsplit_once(':')
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
        captures
            .iter()
            .map(|name| (name.clone(), self.capture_value(name, seen)))
            .collect()
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
        let short = type_name.rsplit(':').next().unwrap_or(type_name);
        short == "gen" || short.starts_with("gen->")
    }

    pub(crate) fn resolve_aggregate_member_slot(
        &mut self,
        type_name: &str,
        map: &GcMap,
        name: &str,
        short_name: Option<&str>,
    ) -> Option<usize> {
        if map.0.0.len() <= 3 {
            return map.0.0.iter().enumerate().find_map(|(idx, (field, _))| {
                if field == name || short_name.is_some_and(|short| field == short) {
                    Some(idx)
                } else {
                    None
                }
            });
        }

        if let Some(slots) = self.caches.aggregate_member_slots.get(type_name) {
            if let Some(idx) = slots.get(name).copied() {
                return Some(idx);
            }
            if let Some(short) = short_name
                && let Some(idx) = slots.get(short).copied()
            {
                return Some(idx);
            }
        }

        let idx = map.0.0.iter().enumerate().find_map(|(idx, (field, _))| {
            if field == name || short_name.is_some_and(|short| field == short) {
                Some(idx)
            } else {
                None
            }
        })?;

        let slots = self
            .caches
            .aggregate_member_slots
            .entry(type_name.to_string())
            .or_default();
        slots.insert(name.to_string(), idx);
        if let Some(short) = short_name {
            let _ = slots.entry(short.to_string()).or_insert(idx);
        }
        Some(idx)
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
        }
        if self.variables.contains_key(name) {
            return VarName::Var(name.to_string());
        }
        if name.contains("::") {
            return VarName::Global;
        }
        if let Some((_, short_name)) = name.rsplit_once(':') {
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
