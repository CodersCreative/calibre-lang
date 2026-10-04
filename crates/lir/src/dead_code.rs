use crate::{
    ast::{
        LirAggregate, LirAs, LirAssign, LirBinary, LirBoolean, LirCall, LirClosure, LirComparison,
        LirDeclare, LirDeref, LirDiscriminant, LirEmit, LirEnum, LirIndex, LirIs, LirLValue,
        LirList, LirLoad, LirMember, LirMove, LirNodeType, LirRange, LirRef, LirRefLoad, LirSpawn,
        LirTerminator,
    },
    environment::{LirFunction, LirGlobal, LirRegistry},
};
use calibre_mir::symbols::VariableKey;
use rustc_hash::FxHashSet;

enum WorkItem {
    Function(VariableKey),
    Global(VariableKey),
}

#[derive(Default)]
struct WorkList {
    stack: Vec<WorkItem>,
    seen_functions: FxHashSet<VariableKey>,
    seen_globals: FxHashSet<VariableKey>,
}

impl WorkList {
    pub fn pop(&mut self) -> Option<WorkItem> {
        self.stack.pop()
    }

    pub fn push_function(&mut self, value: VariableKey) {
        if self.seen_functions.contains(&value) {
            return;
        }

        self.seen_functions.insert(value.clone());
        self.stack.push(WorkItem::Function(value));
    }

    pub fn push_global(&mut self, value: VariableKey) {
        if self.seen_globals.contains(&value) {
            return;
        }

        self.seen_globals.insert(value.clone());
        self.stack.push(WorkItem::Global(value));
    }
}

impl LirRegistry {
    pub fn eliminate_dead_code(
        mut self,
        entry_points: Vec<VariableKey>,
        include_tests: bool,
    ) -> LirRegistry {
        let (reachable_functions, reachable_globals) =
            self.collect_references(entry_points, include_tests);

        self.functions
            .retain(|name, _| reachable_functions.contains(name));

        self.globals
            .retain(|name, _| reachable_globals.contains(name));

        self
    }

    fn collect_references(
        &self,
        entry_points: Vec<VariableKey>,
        include_tests: bool,
    ) -> (FxHashSet<VariableKey>, FxHashSet<VariableKey>) {
        let mut reachable_functions = FxHashSet::default();
        let mut reachable_globals = FxHashSet::default();

        for entry in entry_points.iter() {
            if self.functions.contains_key(entry) {
                reachable_functions.insert(entry.clone());
            }
        }

        if include_tests {
            for entry in self
                .functions
                .keys()
                .filter(|x| x.name().contains("test::"))
            {
                reachable_functions.insert(entry.clone());
            }
        }

        let mut worklist = WorkList::default();

        for entry in entry_points.iter() {
            if self.functions.contains_key(entry) {
                worklist.push_function(entry.clone());
            }
        }

        if include_tests {
            for entry in self
                .functions
                .keys()
                .filter(|x| x.name().contains("test::"))
            {
                worklist.push_function(entry.clone());
            }
        }

        while let Some(item) = worklist.pop() {
            match item {
                WorkItem::Function(func_name) => {
                    if let Some(func) = self.functions.get(&func_name) {
                        func.collect_references(
                            self,
                            &mut reachable_functions,
                            &mut reachable_globals,
                            &mut worklist,
                        );
                    }
                }
                WorkItem::Global(global_name) => {
                    if let Some(global) = self.globals.get(&global_name) {
                        global.collect_references(
                            self,
                            &mut reachable_functions,
                            &mut reachable_globals,
                            &mut worklist,
                        );
                    }
                }
            }
        }

        (reachable_functions, reachable_globals)
    }
}

impl LirFunction {
    fn collect_references(
        &self,
        registry: &LirRegistry,
        reachable_functions: &mut FxHashSet<VariableKey>,
        reachable_globals: &mut FxHashSet<VariableKey>,
        worklist: &mut WorkList,
    ) {
        for block in self.blocks.iter().flatten() {
            for instruction in &block.instructions {
                instruction.node_type.collect_references(
                    registry,
                    reachable_functions,
                    reachable_globals,
                    worklist,
                );
            }

            if let Some(terminator) = &block.terminator {
                terminator.collect_references(
                    registry,
                    reachable_functions,
                    reachable_globals,
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
        reachable_functions: &mut FxHashSet<VariableKey>,
        reachable_globals: &mut FxHashSet<VariableKey>,
        worklist: &mut WorkList,
    ) {
        for block in self.blocks.iter().flatten() {
            for instruction in &block.instructions {
                instruction.node_type.collect_references(
                    registry,
                    reachable_functions,
                    reachable_globals,
                    worklist,
                );
            }

            if let Some(terminator) = &block.terminator {
                terminator.collect_references(
                    registry,
                    reachable_functions,
                    reachable_globals,
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
        reachable_functions: &mut FxHashSet<VariableKey>,
        reachable_globals: &mut FxHashSet<VariableKey>,
        worklist: &mut WorkList,
    ) {
        match self {
            LirNodeType::Load(LirLoad { value })
            | LirNodeType::Move(LirMove { value })
            | LirNodeType::RefLoad(LirRefLoad { value }) => {
                if registry.functions.contains_key(value)
                    && reachable_functions.insert(value.clone())
                {
                    worklist.push_function(value.clone());
                }

                if registry.globals.contains_key(value) && reachable_globals.insert(value.clone()) {
                    worklist.push_global(value.clone());
                }
            }
            LirNodeType::Call(LirCall { caller, args, .. }) => {
                caller.collect_references(
                    registry,
                    reachable_functions,
                    reachable_globals,
                    worklist,
                );

                for arg in args {
                    arg.collect_references(
                        registry,
                        reachable_functions,
                        reachable_globals,
                        worklist,
                    );
                }
            }
            LirNodeType::Closure(LirClosure { label, .. }) => {
                if registry.functions.contains_key(label)
                    && reachable_functions.insert(label.clone())
                {
                    worklist.push_function(label.clone());
                }
            }
            LirNodeType::List(LirList { values, .. }) => {
                for element in values {
                    element.collect_references(
                        registry,
                        reachable_functions,
                        reachable_globals,
                        worklist,
                    );
                }
            }
            LirNodeType::Aggregate(LirAggregate { fields, .. }) => {
                for (_field_name, field) in &fields.0 {
                    field.collect_references(
                        registry,
                        reachable_functions,
                        reachable_globals,
                        worklist,
                    );
                }
            }
            LirNodeType::Range(LirRange {
                from: left,
                to: right,
                ..
            })
            | LirNodeType::Boolean(LirBoolean { left, right, .. })
            | LirNodeType::Comparison(LirComparison { left, right, .. })
            | LirNodeType::Binary(LirBinary { left, right, .. }) => {
                left.collect_references(registry, reachable_functions, reachable_globals, worklist);

                right.collect_references(
                    registry,
                    reachable_functions,
                    reachable_globals,
                    worklist,
                );
            }

            LirNodeType::Index(LirIndex { base, index }) => {
                base.collect_references(registry, reachable_functions, reachable_globals, worklist);
                index.collect_references(
                    registry,
                    reachable_functions,
                    reachable_globals,
                    worklist,
                );
            }
            LirNodeType::Enum(LirEnum { payload, .. }) => {
                if let Some(payload) = payload {
                    payload.collect_references(
                        registry,
                        reachable_functions,
                        reachable_globals,
                        worklist,
                    );
                }
            }
            LirNodeType::As(LirAs { value, .. })
            | LirNodeType::Declare(LirDeclare { value, .. })
            | LirNodeType::Is(LirIs { value, .. }) => {
                value.collect_references(
                    registry,
                    reachable_functions,
                    reachable_globals,
                    worklist,
                );
            }
            LirNodeType::Emit(LirEmit { value })
            | LirNodeType::Spawn(LirSpawn { value })
            | LirNodeType::Deref(LirDeref { value })
            | LirNodeType::Ref(LirRef { value })
            | LirNodeType::Discriminant(LirDiscriminant { value })
            | LirNodeType::Member(LirMember {
                base: value,
                field: _,
            }) => {
                value.collect_references(
                    registry,
                    reachable_functions,
                    reachable_globals,
                    worklist,
                );
            }
            LirNodeType::Assign(LirAssign { dest, value }) => {
                value.collect_references(
                    registry,
                    reachable_functions,
                    reachable_globals,
                    worklist,
                );

                if let LirLValue::Ptr(ptr) = dest {
                    ptr.collect_references(
                        registry,
                        reachable_functions,
                        reachable_globals,
                        worklist,
                    );
                }
            }
            LirNodeType::Noop
            | LirNodeType::Literal(_)
            | LirNodeType::Drop(_)
            | LirNodeType::ExternFunction(_) => {}
        }
    }
}

impl LirTerminator {
    fn collect_references(
        &self,
        registry: &LirRegistry,
        reachable_functions: &mut FxHashSet<VariableKey>,
        reachable_globals: &mut FxHashSet<VariableKey>,
        worklist: &mut WorkList,
    ) {
        match self {
            LirTerminator::Jump { .. } => {}
            LirTerminator::Branch { condition, .. } => {
                condition.collect_references(
                    registry,
                    reachable_functions,
                    reachable_globals,
                    worklist,
                );
            }
            LirTerminator::Return { value, .. } => {
                if let Some(value) = value {
                    value.collect_references(
                        registry,
                        reachable_functions,
                        reachable_globals,
                        worklist,
                    );
                }
            }
        }
    }
}
