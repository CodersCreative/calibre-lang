use crate::{
    ast::{
        LirAggregate, LirAs, LirAssign, LirBinary, LirBoolean, LirCall, LirClosure, LirComparison,
        LirDeclare, LirDeref, LirEnum, LirIndex, LirIs, LirLValue, LirList, LirLoad, LirMember,
        LirMove, LirNodeType, LirRange, LirRef, LirRefLoad, LirSpawn, LirTerminator,
    },
    environment::{LirFunction, LirGlobal, LirRegistry},
};
use ustr::{Ustr, UstrSet};

enum WorkItem {
    Function(Ustr),
    Global(Ustr),
    Type(Ustr),
}

#[derive(Default)]
struct WorkList {
    stack: Vec<WorkItem>,
    seen_functions: UstrSet,
    seen_globals: UstrSet,
    seen_types: UstrSet,
}

impl WorkList {
    pub fn pop(&mut self) -> Option<WorkItem> {
        self.stack.pop()
    }

    pub fn push_function(&mut self, value: Ustr) {
        if self.seen_functions.contains(&value) {
            return;
        }

        self.seen_functions.insert(value.clone());
        self.stack.push(WorkItem::Function(value));
    }

    pub fn push_global(&mut self, value: Ustr) {
        if self.seen_globals.contains(&value) {
            return;
        }

        self.seen_globals.insert(value.clone());
        self.stack.push(WorkItem::Global(value));
    }

    pub fn push_type(&mut self, value: Ustr) {
        if self.seen_types.contains(&value) {
            return;
        }

        self.seen_types.insert(value.clone());
        self.stack.push(WorkItem::Type(value));
    }

    pub fn has_work(&self) -> bool {
        !self.stack.is_empty()
    }
}

impl LirRegistry {
    pub fn eliminate_dead_code(
        mut self,
        entry_points: Vec<Ustr>,
        include_tests: bool,
    ) -> LirRegistry {
        let (reachable_functions, reachable_globals, referenced_types) =
            self.collect_references(entry_points, include_tests);

        self.dyn_vtables
            .retain(|concrete_type, _| referenced_types.contains(concrete_type));

        self.functions.retain(|name, _| {
            reachable_functions.contains(name)
                || name
                    .rsplit_once(".")
                    .is_some_and(|x| reachable_functions.contains(&Ustr::from(x.1)))
        });

        self.globals.retain(|name, _| {
            reachable_globals.contains(name)
                || name
                    .rsplit_once(".")
                    .is_some_and(|x| reachable_globals.contains(&Ustr::from(x.1)))
        });

        self
    }

    fn collect_references(
        &self,
        entry_points: Vec<Ustr>,
        include_tests: bool,
    ) -> (UstrSet, UstrSet, UstrSet) {
        let mut reachable_functions = UstrSet::default();
        let mut reachable_globals = UstrSet::default();
        let mut referenced_types = UstrSet::default();

        for entry in entry_points.iter() {
            if self.functions.contains_key(entry) {
                reachable_functions.insert(entry.clone());
            }
        }

        if include_tests {
            for entry in self.functions.keys().filter(|x| x.contains("test::")) {
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
            for entry in self.functions.keys().filter(|x| x.contains("test::")) {
                worklist.push_function(entry.clone());
            }
        }

        while worklist.has_work() {
            for (concrete_type, trait_map) in &self.dyn_vtables {
                if referenced_types.contains(concrete_type) {
                    for methods in trait_map.values() {
                        for function_name in methods.values() {
                            if reachable_functions.insert(function_name.clone()) {
                                worklist.push_function(function_name.clone());
                            }
                        }
                    }
                }
            }

            for typ in referenced_types.clone().iter() {
                for global in self.globals.iter().filter(|x| x.0.contains(typ.as_str())) {
                    if reachable_globals.insert(global.0.clone()) {
                        worklist.push_global(global.0.clone());
                    }
                }

                for func in self.functions.iter().filter(|x| x.0.contains(typ.as_str())) {
                    if reachable_functions.insert(func.0.clone()) {
                        worklist.push_function(func.0.clone());
                    }
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
                                &mut referenced_types,
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
                                &mut referenced_types,
                                &mut worklist,
                            );
                        }
                    }
                    WorkItem::Type(typ) => {
                        for global in self.globals.iter().filter(|x| x.0.contains(typ.as_str())) {
                            if reachable_globals.insert(global.0.clone()) {
                                worklist.push_global(global.0.clone());
                            }
                        }

                        for func in self.functions.iter().filter(|x| x.0.contains(typ.as_str())) {
                            if reachable_functions.insert(func.0.clone()) {
                                worklist.push_function(func.0.clone());
                            }
                        }
                    }
                }
            }
        }

        (reachable_functions, reachable_globals, referenced_types)
    }
}

impl LirFunction {
    fn collect_references(
        &self,
        registry: &LirRegistry,
        reachable_functions: &mut UstrSet,
        reachable_globals: &mut UstrSet,
        referenced_types: &mut UstrSet,
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
        reachable_functions: &mut UstrSet,
        reachable_globals: &mut UstrSet,
        referenced_types: &mut UstrSet,
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
        reachable_functions: &mut UstrSet,
        reachable_globals: &mut UstrSet,
        referenced_types: &mut UstrSet,
        worklist: &mut WorkList,
    ) {
        match self {
            LirNodeType::Load(LirLoad { value })
            | LirNodeType::Move(LirMove { value })
            | LirNodeType::RefLoad(LirRefLoad { value }) => {
                if registry.functions.contains_key(value) && reachable_functions.insert(*value) {
                    worklist.push_function(*value);
                }

                if registry.globals.contains_key(value) {
                    if reachable_globals.insert(*value) {
                        worklist.push_global(*value);
                    }
                }
            }
            LirNodeType::Call(LirCall { caller, args }) => {
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
            LirNodeType::Closure(LirClosure { label, .. }) => {
                if registry.functions.contains_key(label) && reachable_functions.insert(*label) {
                    worklist.push_function(*label);
                }
            }
            LirNodeType::List(LirList { values, data_type }) => {
                let type_name = Ustr::from(&data_type.impl_name());
                if referenced_types.insert(type_name.clone()) {
                    worklist.push_type(type_name);
                }

                for element in values {
                    element.collect_references(
                        registry,
                        reachable_functions,
                        reachable_globals,
                        referenced_types,
                        worklist,
                    );
                }
            }
            LirNodeType::Aggregate(LirAggregate { name, fields }) => {
                if let Some(x) = name {
                    if referenced_types.insert(*x) {
                        worklist.push_type(*x);
                    }
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
            LirNodeType::Range(LirRange {
                from: left,
                to: right,
                ..
            })
            | LirNodeType::Boolean(LirBoolean { left, right, .. })
            | LirNodeType::Comparison(LirComparison { left, right, .. })
            | LirNodeType::Binary(LirBinary { left, right, .. }) => {
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

            LirNodeType::Index(LirIndex { base, index }) => {
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
            LirNodeType::Enum(LirEnum { name, payload, .. }) => {
                if referenced_types.insert(*name) {
                    worklist.push_type(*name);
                }
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
            LirNodeType::As(LirAs {
                value,
                data_type,
                failure_mode: _,
            })
            | LirNodeType::Declare(LirDeclare {
                value, data_type, ..
            })
            | LirNodeType::Is(LirIs { value, data_type }) => {
                let type_name = Ustr::from(&data_type.impl_name());
                if referenced_types.insert(type_name.clone()) {
                    worklist.push_type(type_name);
                }
                value.collect_references(
                    registry,
                    reachable_functions,
                    reachable_globals,
                    referenced_types,
                    worklist,
                );
            }
            LirNodeType::Spawn(LirSpawn { value })
            | LirNodeType::Deref(LirDeref { value })
            | LirNodeType::Ref(LirRef { value })
            | LirNodeType::Member(LirMember {
                base: value,
                field: _,
            }) => {
                value.collect_references(
                    registry,
                    reachable_functions,
                    reachable_globals,
                    referenced_types,
                    worklist,
                );
            }
            LirNodeType::Assign(LirAssign { dest, value }) => {
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
            | LirNodeType::ExternFunction(_) => {}
        }
    }
}

impl LirTerminator {
    fn collect_references(
        &self,
        registry: &LirRegistry,
        reachable_functions: &mut UstrSet,
        reachable_globals: &mut UstrSet,
        referenced_types: &mut UstrSet,
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
