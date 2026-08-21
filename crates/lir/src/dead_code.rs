use crate::{
    ast::{LirLValue, LirNodeType, LirTerminator},
    environment::{LirFunction, LirGlobal, LirRegistry},
};
use rustc_hash::FxHashSet;

struct WorkList {
    stack: Vec<String>,
    seen: FxHashSet<String>,
}

impl WorkList {
    pub fn pop(&mut self) -> Option<String> {
        self.stack.pop()
    }

    pub fn push(&mut self, value: String) {
        if self.seen.contains(&value) {
            return;
        }

        self.seen.insert(value.clone());
        self.stack.push(value);
    }
}

impl LirRegistry {
    pub fn eliminate_dead_code(mut self, entry_points: Vec<String>) -> LirRegistry {
        let (reachable_functions, reachable_globals, referenced_types) =
            self.collect_references(entry_points);

        self.dyn_vtables
            .retain(|concrete_type, _| referenced_types.contains(concrete_type));

        self.functions
            .retain(|name, _| reachable_functions.contains(name));

        self.globals
            .retain(|name, _| reachable_globals.contains(name));

        self
    }

    fn collect_references(
        &self,
        entry_points: Vec<String>,
    ) -> (FxHashSet<String>, FxHashSet<String>, FxHashSet<String>) {
        let mut reachable_functions: FxHashSet<String> = FxHashSet::default();
        let mut reachable_globals: FxHashSet<String> = FxHashSet::default();
        let mut referenced_types: FxHashSet<String> = FxHashSet::default();

        for entry in entry_points.iter() {
            if self.functions.contains_key(entry) {
                reachable_functions.insert(entry.clone());
            }
        }

        let mut worklist = WorkList {
            stack: entry_points.clone(),
            seen: entry_points.into_iter().collect(),
        };

        for (concrete_type, trait_map) in &self.dyn_vtables {
            if referenced_types.contains(concrete_type) {
                for (_trait_name, methods) in trait_map {
                    for (_method_name, function_name) in methods {
                        if reachable_functions.insert(function_name.clone()) {
                            worklist.push(function_name.clone());
                        }
                    }
                }
            }
        }

        for global_name in reachable_globals.clone() {
            if let Some(global) = self.globals.get(&global_name) {
                global.collect_references(
                    self,
                    &mut reachable_functions,
                    &mut reachable_globals,
                    &mut referenced_types,
                    &mut worklist,
                );
            }
        }

        while let Some(func_name) = worklist.pop() {
            if let Some(func) = self.functions.get(&func_name) {
                func.collect_references(
                    self,
                    &mut reachable_functions,
                    &mut reachable_globals,
                    &mut referenced_types,
                    &mut worklist,
                );
            }
        }

        (reachable_functions, reachable_globals, referenced_types)
    }
}

impl LirFunction {
    fn collect_references(
        &self,
        registry: &LirRegistry,
        reachable_functions: &mut FxHashSet<String>,
        reachable_globals: &mut FxHashSet<String>,
        referenced_types: &mut FxHashSet<String>,
        worklist: &mut WorkList,
    ) {
        for block in &self.blocks {
            for instruction in &block.instructions {
                instruction.node_type.collect_references(
                    registry,
                    reachable_functions,
                    reachable_globals,
                    referenced_types,
                    worklist,
                );
            }

            if let Some(terminator) = &block.terminator {
                terminator.collect_references(
                    registry,
                    reachable_functions,
                    reachable_globals,
                    referenced_types,
                    worklist,
                );
            }
        }
    }
}

impl LirGlobal {
    fn collect_references(
        &self,
        registry: &LirRegistry,
        reachable_functions: &mut FxHashSet<String>,
        reachable_globals: &mut FxHashSet<String>,
        referenced_types: &mut FxHashSet<String>,
        worklist: &mut WorkList,
    ) {
        for block in &self.blocks {
            for instruction in &block.instructions {
                instruction.node_type.collect_references(
                    registry,
                    reachable_functions,
                    reachable_globals,
                    referenced_types,
                    worklist,
                );
            }

            if let Some(terminator) = &block.terminator {
                terminator.collect_references(
                    registry,
                    reachable_functions,
                    reachable_globals,
                    referenced_types,
                    worklist,
                );
            }
        }
    }
}

impl LirNodeType {
    fn collect_references(
        &self,
        registry: &LirRegistry,
        reachable_functions: &mut FxHashSet<String>,
        reachable_globals: &mut FxHashSet<String>,
        referenced_types: &mut FxHashSet<String>,
        worklist: &mut WorkList,
    ) {
        match self {
            LirNodeType::Load(name) | LirNodeType::Move(name) | LirNodeType::RefLoad(name) => {
                if registry.functions.contains_key(name.as_ref()) {
                    if reachable_functions.insert(name.as_ref().to_string()) {
                        worklist.push(name.as_ref().to_string());
                    }
                } else if registry.globals.contains_key(name.as_ref()) {
                    reachable_globals.insert(name.as_ref().to_string());
                }
            }
            LirNodeType::Call { caller, args } => {
                caller.collect_references(
                    registry,
                    reachable_functions,
                    reachable_globals,
                    referenced_types,
                    worklist,
                );

                for arg in args {
                    arg.collect_references(
                        registry,
                        reachable_functions,
                        reachable_globals,
                        referenced_types,
                        worklist,
                    );
                }
            }
            LirNodeType::Closure { label, .. } => {
                if registry.functions.contains_key(label.as_ref())
                    && reachable_functions.insert(label.as_ref().to_string())
                {
                    worklist.push(label.as_ref().to_string());
                }
            }
            LirNodeType::List { elements, .. } => {
                for element in elements {
                    element.collect_references(
                        registry,
                        reachable_functions,
                        reachable_globals,
                        referenced_types,
                        worklist,
                    );
                }
            }
            LirNodeType::Aggregate { name, fields } => {
                if let Some(x) = name {
                    referenced_types.insert(x.to_string());
                }

                for (_field_name, field) in &fields.0 {
                    field.collect_references(
                        registry,
                        reachable_functions,
                        reachable_globals,
                        referenced_types,
                        worklist,
                    );
                }
            }
            LirNodeType::Range {
                from: left,
                to: right,
                ..
            }
            | LirNodeType::Boolean { left, right, .. }
            | LirNodeType::Comparison { left, right, .. }
            | LirNodeType::Binary { left, right, .. } => {
                left.collect_references(
                    registry,
                    reachable_functions,
                    reachable_globals,
                    referenced_types,
                    worklist,
                );
                right.collect_references(
                    registry,
                    reachable_functions,
                    reachable_globals,
                    referenced_types,
                    worklist,
                );
            }

            LirNodeType::Index(base, index) => {
                base.collect_references(
                    registry,
                    reachable_functions,
                    reachable_globals,
                    referenced_types,
                    worklist,
                );
                index.collect_references(
                    registry,
                    reachable_functions,
                    reachable_globals,
                    referenced_types,
                    worklist,
                );
            }
            LirNodeType::Enum { name, payload, .. } => {
                referenced_types.insert(name.as_ref().to_string());
                if let Some(payload) = payload {
                    payload.collect_references(
                        registry,
                        reachable_functions,
                        reachable_globals,
                        referenced_types,
                        worklist,
                    );
                }
            }
            LirNodeType::Spawn { callee: value }
            | LirNodeType::Declare { value, .. }
            | LirNodeType::Deref(value)
            | LirNodeType::Ref(value)
            | LirNodeType::Member(value, _)
            | LirNodeType::As(value, _, _)
            | LirNodeType::Is(value, _) => {
                value.collect_references(
                    registry,
                    reachable_functions,
                    reachable_globals,
                    referenced_types,
                    worklist,
                );
            }
            LirNodeType::Assign { dest, value } => {
                value.collect_references(
                    registry,
                    reachable_functions,
                    reachable_globals,
                    referenced_types,
                    worklist,
                );
                if let LirLValue::Ptr(ptr) = dest {
                    ptr.collect_references(
                        registry,
                        reachable_functions,
                        reachable_globals,
                        referenced_types,
                        worklist,
                    );
                }
            }
            LirNodeType::Noop
            | LirNodeType::Literal(_)
            | LirNodeType::Drop(_)
            | LirNodeType::ExternFunction { .. } => {}
        }
    }
}

impl LirTerminator {
    fn collect_references(
        &self,
        registry: &LirRegistry,
        reachable_functions: &mut FxHashSet<String>,
        reachable_globals: &mut FxHashSet<String>,
        referenced_types: &mut FxHashSet<String>,
        worklist: &mut WorkList,
    ) {
        match self {
            LirTerminator::Jump { .. } => {}
            LirTerminator::Branch { condition, .. } => {
                condition.collect_references(
                    registry,
                    reachable_functions,
                    reachable_globals,
                    referenced_types,
                    worklist,
                );
            }
            LirTerminator::Return { value, .. } => {
                if let Some(value) = value {
                    value.collect_references(
                        registry,
                        reachable_functions,
                        reachable_globals,
                        referenced_types,
                        worklist,
                    );
                }
            }
        }
    }
}
