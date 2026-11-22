use crate::{
    ast::{
        BlockId, LirAggregate, LirBlock, LirCall, LirComparison, LirLValue, LirLiteral, LirLoad,
        LirMove, LirNode, LirNodeType, LirRefLoad, LirTerminator,
    },
    environment::{LirRegistry, LirType},
};
use calibre_mir::{
    ast::types::MirDataType,
    symbols::{TypeKey, VariableKey, resolve::Key},
};
use calibre_parser::ast::{ObjectMap, RefMutability, comparison::ComparisonOperator};
use rustc_hash::{FxHashMap, FxHashSet};
use std::fmt::Display;
use ustr::Ustr;

struct Context {
    suspend: VariableKey,
    some: VariableKey,
    none: VariableKey,
    counter: usize,
}

impl Context {
    fn temp_key(&mut self, prefix: impl Display) -> VariableKey {
        self.counter += 1;
        VariableKey::from(format!("{}_{}", prefix, self.counter).as_str())
    }

    pub fn extract_suspends(
        &mut self,
        node: &mut LirNodeType,
        out: &mut Vec<(VariableKey, LirNodeType)>,
    ) {
        if node
            .callee_key()
            .is_some_and(|key| key.name() == self.suspend.name())
            && let LirNodeType::Call(call) = node
        {
            self.extract_suspends(&mut call.args[0], out);

            let key = self.temp_key("gen_yield_tmp");
            let x = call.args[0].clone();
            *node = LirNodeType::load(key.clone());
            out.push((key, x));

            return;
        }

        match node {
            LirNodeType::Call(call) => {
                self.extract_suspends(&mut call.caller, out);
                for arg in call.args.iter_mut() {
                    self.extract_suspends(arg, out);
                }
            }
            LirNodeType::Deref(d) => self.extract_suspends(&mut d.value, out),
            LirNodeType::Ref(r) => self.extract_suspends(&mut r.value, out),
            LirNodeType::Clone(c) => self.extract_suspends(&mut c.value, out),
            LirNodeType::Binary(b) => {
                self.extract_suspends(&mut b.left, out);
                self.extract_suspends(&mut b.right, out);
            }
            LirNodeType::Comparison(c) => {
                self.extract_suspends(&mut c.left, out);
                self.extract_suspends(&mut c.right, out);
            }
            LirNodeType::As(a) => self.extract_suspends(&mut a.value, out),
            LirNodeType::Is(i) => self.extract_suspends(&mut i.value, out),
            LirNodeType::Range(r) => {
                self.extract_suspends(&mut r.from, out);
                self.extract_suspends(&mut r.to, out);
            }
            LirNodeType::List(l) => {
                for v in l.values.iter_mut() {
                    self.extract_suspends(v, out);
                }
            }
            LirNodeType::Aggregate(a) => {
                for (_, v) in a.fields.0.iter_mut() {
                    self.extract_suspends(v, out);
                }
            }
            LirNodeType::Enum(e) => {
                if let Some(p) = e.payload.as_mut() {
                    self.extract_suspends(p, out);
                }
            }
            LirNodeType::Index(i) => {
                self.extract_suspends(&mut i.base, out);
                self.extract_suspends(&mut i.index, out);
            }
            LirNodeType::Member(m) => self.extract_suspends(&mut m.base, out),
            LirNodeType::Discriminant(d) => self.extract_suspends(&mut d.value, out),
            LirNodeType::Spawn(s) => self.extract_suspends(&mut s.value, out),
            LirNodeType::Assign(a) => {
                if let LirLValue::Ptr(p) = &mut a.dest {
                    self.extract_suspends(p, out);
                }
                self.extract_suspends(&mut a.value, out);
            }
            LirNodeType::Declare(d) => self.extract_suspends(&mut d.value, out),
            _ => {}
        }
    }

    fn split_blocks(
        &mut self,
        blocks: Vec<LirBlock>,
    ) -> (Vec<Segment>, Vec<(usize, VariableKey, LirNodeType)>) {
        let mut segments: Vec<Segment> = Vec::new();
        let mut yields: Vec<(usize, VariableKey, LirNodeType)> = Vec::new();
        let mut next_id = blocks.iter().map(|b| b.id.0).max().unwrap_or(0) + 1;

        for block in blocks {
            let mut current = Segment {
                block_id: block.id,
                instructions: Vec::new(),
                yield_value: None,
                terminator: None,
            };

            for instr in block.instructions {
                let mut node = instr.node_type;
                let mut found = Vec::new();
                self.extract_suspends(&mut node, &mut found);

                for (key, node) in found {
                    current.close_yield(&mut segments, &mut next_id, key, node, &mut yields);
                }

                current.instructions.push(LirNode::new(instr.span, node));
            }

            if let Some(mut t) = block.terminator {
                match &mut t {
                    LirTerminator::Return { value: Some(v), .. } => {
                        let mut found = Vec::new();
                        self.extract_suspends(v, &mut found);

                        for (tmp, x) in found {
                            current.close_yield(&mut segments, &mut next_id, tmp, x, &mut yields);
                        }
                    }
                    LirTerminator::Branch { condition, .. } => {
                        let mut found = Vec::new();
                        self.extract_suspends(condition, &mut found);

                        for (tmp, x) in found {
                            current.close_yield(&mut segments, &mut next_id, tmp, x, &mut yields);
                        }
                    }
                    _ => {}
                }

                current.terminator = Some(t);
            }

            if !current.instructions.is_empty()
                || current.yield_value.is_some()
                || current.terminator.is_some()
            {
                segments.push(current);
            }
        }

        (segments, yields)
    }

    fn transform_function(
        &mut self,
        registry: &mut LirRegistry,
        func_key: &VariableKey,
    ) -> Option<(TypeKey, usize)> {
        let func = registry.functions.get_mut(func_key)?;

        if func.params.is_empty() || !func.params[0].data_type.is_ref() {
            return None;
        }

        let param_key = func.params[0].identifier.clone();
        let mut var_types = FxHashMap::default();

        for param in &func.params {
            var_types.insert(param.identifier.clone(), param.data_type.clone());
        }

        for capture in &func.captures {
            var_types.insert(capture.name.clone(), capture.data_type.clone());
        }

        for block in func.blocks.iter().flatten() {
            for instr in &block.instructions {
                if let LirNodeType::Declare(decl) = &instr.node_type {
                    var_types.insert(decl.dest.clone(), decl.data_type.clone());
                }
            }
        }

        let old_blocks: Vec<LirBlock> = func.blocks.iter().flatten().cloned().collect();
        if old_blocks.is_empty() {
            return None;
        }

        let (mut segments, yields) = self.split_blocks(old_blocks);

        if yields.is_empty() {
            func.blocks = segments
                .into_iter()
                .map(|s| {
                    Some(LirBlock {
                        id: s.block_id,
                        instructions: s.instructions,
                        terminator: s.terminator,
                    })
                })
                .collect::<Vec<_>>()
                .into_boxed_slice();

            return None;
        }

        let mut excluded: FxHashSet<VariableKey> = func
            .params
            .iter()
            .map(|x| x.identifier.clone())
            .chain(func.captures.iter().map(|x| x.name.clone()))
            .collect();

        excluded.insert(self.suspend.clone());
        excluded.extend(yields.iter().map(|(_, key, _)| key.clone()));

        let resume_of: Vec<usize> = yields
            .iter()
            .map(|(index, _, _)| {
                let index = index + 1;
                assert!(
                    index < segments.len(),
                    "yield point has no resume segmentment"
                );
                index
            })
            .collect();

        let segments_len = segments.len();
        let mut use_def: Vec<(FxHashSet<VariableKey>, FxHashSet<VariableKey>)> =
            Vec::with_capacity(segments_len);
        let mut successors: Vec<Vec<usize>> = vec![Vec::new(); segments_len];

        for (i, segment) in segments.iter().enumerate() {
            use_def.push(segment.block_use_def());

            if segment.yield_value.is_some() {
                if let Some(index) = yields.iter().position(|(index, _, _)| *index == i) {
                    successors[i] = vec![resume_of[index]];
                }
            } else if let Some(t) = &segment.terminator {
                for id in t.successors() {
                    if let Some(index) = segments.iter().position(|x| x.block_id == id) {
                        successors[i].push(index);
                    }
                }
            }
        }

        let mut live_in = vec![FxHashSet::default(); segments_len];

        loop {
            let mut changed = false;

            for i in (0..segments_len).rev() {
                let (use_set, def_set) = &use_def[i];
                let mut new_in: FxHashSet<VariableKey> = use_set.clone();

                for s in &successors[i] {
                    for v in &live_in[*s] {
                        if !def_set.contains(v) {
                            new_in.insert(v.clone());
                        }
                    }
                }

                if new_in != live_in[i] {
                    live_in[i] = new_in;
                    changed = true;
                }
            }

            if !changed {
                break;
            }
        }

        let mut persisted = FxHashSet::default();

        for r in resume_of.clone().into_iter().take(yields.len()) {
            for v in &live_in[r] {
                if !excluded.contains(v) {
                    persisted.insert(v.clone());
                }
            }
        }

        let mut persisted: Vec<VariableKey> = persisted.into_iter().collect();
        persisted.sort_by_key(|v| v.to_string());

        let field_of: FxHashMap<VariableKey, usize> = persisted
            .iter()
            .enumerate()
            .map(|(i, v)| (v.clone(), i))
            .collect();

        let state_key = TypeKey::from(format!("gen_state_{}", func_key).as_str());

        let state_mir_ty = MirDataType::Struct {
            identifier: state_key.clone(),
            generic_types: Vec::new(),
        };

        let mut type_fields: Vec<(Ustr, MirDataType)> = vec![
            (Ustr::from("tag"), MirDataType::UInt),
            (Ustr::from("yield"), MirDataType::Dynamic),
        ];

        for (i, _) in persisted.iter().enumerate() {
            type_fields.push((Ustr::from(&format!("v{}", i)), MirDataType::Dynamic));
        }

        registry
            .types
            .insert(state_key.clone(), LirType::Struct(ObjectMap(type_fields)));

        for (tag, (index, (head_index, key, node))) in (1..).zip(yields.iter().enumerate()) {
            let head = &mut segments[*head_index];
            let resume_index = resume_of[index];

            head.instructions.insert(
                0,
                LirNode::new(
                    Default::default(),
                    LirNodeType::declare(key.clone(), LirNodeType::null(), MirDataType::Dynamic),
                ),
            );

            for key in live_in[resume_index]
                .iter()
                .filter(|v| field_of.contains_key(*v))
            {
                let field = format!("v{}", field_of[key]);

                head.instructions.push(LirNode::new(
                    Default::default(),
                    LirNodeType::assign(
                        LirLValue::Ptr(Box::new(LirNodeType::member(
                            &param_key,
                            Ustr::from(&field),
                        ))),
                        LirNodeType::load(key.clone()),
                    ),
                ));
            }

            let node_key = self.temp_key("gen_yield_value");
            let yield_type = node.get_yield_type(&var_types);

            head.instructions.push(LirNode::new(
                Default::default(),
                LirNodeType::declare(node_key.clone(), node.clone(), yield_type),
            ));

            head.instructions.push(LirNode::new(
                Default::default(),
                LirNodeType::assign(
                    LirLValue::Ptr(Box::new(LirNodeType::member(
                        &param_key,
                        Ustr::from("yield"),
                    ))),
                    LirNodeType::load(node_key.clone()),
                ),
            ));

            head.instructions.push(LirNode::new(
                Default::default(),
                LirNodeType::assign(
                    LirLValue::Ptr(Box::new(LirNodeType::member(&param_key, Ustr::from("tag")))),
                    LirNodeType::Literal(LirLiteral::UInt(tag)),
                ),
            ));

            head.terminator = Some(LirTerminator::Return {
                span: Default::default(),
                value: Some(LirNodeType::Call(LirCall {
                    caller: Box::new(LirNodeType::load(self.some.clone())),
                    args: Box::new([LirNodeType::load(node_key)]),
                    returns_value: true,
                    return_type: None,
                })),
            });

            head.yield_value = None;

            let resume = &mut segments[resume_index];

            let restore_set: Vec<VariableKey> = live_in[resume_index]
                .iter()
                .filter(|v| field_of.contains_key(*v) && *v != key)
                .cloned()
                .collect();

            let mut prefix: Vec<LirNode> = Vec::new();

            for v in &restore_set {
                let field = format!("v{}", field_of[v]);
                prefix.push(LirNode::new(
                    Default::default(),
                    LirNodeType::assign(
                        LirLValue::Var(v.clone()),
                        LirNodeType::deref_member(&param_key, Ustr::from(&field)),
                    ),
                ));
            }

            prefix.push(LirNode::new(
                Default::default(),
                LirNodeType::declare(
                    key.clone(),
                    LirNodeType::deref_member(&param_key, Ustr::from("yield")),
                    MirDataType::Dynamic,
                ),
            ));

            prefix.append(&mut resume.instructions);
            resume.instructions = prefix;
        }

        let entry_block_id = segments.first().map(|s| s.block_id).unwrap_or(BlockId(0));

        let entry_segment_index = segments
            .iter()
            .position(|s| s.block_id == entry_block_id)
            .unwrap_or(0);

        let resume_ids: Vec<BlockId> = yields
            .iter()
            .enumerate()
            .map(|(i, _)| segments[resume_of[i]].block_id)
            .collect();

        let resume_len = resume_ids.len();

        let base = segments.iter().map(|s| s.block_id.0).max().unwrap_or(0) + 1;
        let none_block_id = BlockId(base + resume_len as u32 + 1);
        let block_ids: Vec<BlockId> = (0..=resume_len).map(|i| BlockId(base + i as u32)).collect();

        let mut blocks: Vec<LirBlock> = (0..resume_len)
            .into_iter()
            .map(|i| LirBlock {
                id: block_ids[i],
                instructions: Vec::new(),
                terminator: Some(LirTerminator::Branch {
                    span: Default::default(),
                    condition: LirNodeType::Comparison(LirComparison {
                        left: Box::new(LirNodeType::deref_member(&param_key, Ustr::from("tag"))),
                        right: Box::new(LirNodeType::Literal(LirLiteral::UInt(i + 1))),
                        operator: ComparisonOperator::Equal,
                    }),
                    then_block: resume_ids[i],
                    else_block: Some(block_ids[i + 1]),
                }),
            })
            .collect();

        blocks.push(LirBlock {
            id: block_ids[resume_len],
            instructions: Vec::new(),
            terminator: Some(LirTerminator::Branch {
                span: Default::default(),
                condition: LirNodeType::Comparison(LirComparison {
                    left: Box::new(LirNodeType::deref_member(&param_key, Ustr::from("tag"))),
                    right: Box::new(LirNodeType::Literal(LirLiteral::UInt(0))),
                    operator: ComparisonOperator::Equal,
                }),
                then_block: segments[entry_segment_index].block_id,
                else_block: Some(none_block_id),
            }),
        });

        let none_block = LirBlock {
            id: none_block_id,
            instructions: Vec::new(),
            terminator: Some(LirTerminator::Return {
                span: Default::default(),
                value: Some(LirNodeType::load(self.none.clone())),
            }),
        };

        blocks.push(none_block);

        for s in segments {
            blocks.push(LirBlock {
                id: s.block_id,
                instructions: s.instructions,
                terminator: s.terminator.or(Some(LirTerminator::Return {
                    span: Default::default(),
                    value: None,
                })),
            });
        }

        func.blocks = blocks.into_iter().map(Some).collect();

        if !func.return_type.is_null() && func.return_type != MirDataType::Never {
            for block in func.blocks.iter_mut().flatten() {
                let Some(LirTerminator::Return { value, .. }) = &mut block.terminator else {
                    continue;
                };

                if value.is_some() {
                    continue;
                }

                let should_fold = block
                    .instructions
                    .last()
                    .is_some_and(|last| last.node_type.is_return_candidate());

                if should_fold {
                    let node = block.instructions.pop()?.node_type;
                    *value = Some(node);
                }
            }
        }

        for block in func.blocks.iter_mut().flatten() {
            for instr in block.instructions.iter_mut() {
                if let LirNodeType::Declare(decl) = &mut instr.node_type {
                    decl.is_referenced = false;
                }
            }
        }

        if let Some(p) = func.params.first_mut() {
            p.data_type = MirDataType::Ref(Box::new(state_mir_ty), RefMutability::MutRef);
        }

        func.referenced_params |= 1;

        Some((state_key, persisted.len()))
    }
}

struct Segment {
    block_id: BlockId,
    instructions: Vec<LirNode>,
    yield_value: Option<(VariableKey, LirNodeType)>,
    terminator: Option<LirTerminator>,
}

impl Segment {
    fn close_yield(
        &mut self,
        segments: &mut Vec<Segment>,
        next_id: &mut u32,
        key: VariableKey,
        node: LirNodeType,
        yields: &mut Vec<(usize, VariableKey, LirNodeType)>,
    ) {
        self.yield_value = Some((key.clone(), node.clone()));

        segments.push(std::mem::replace(
            self,
            Segment {
                block_id: BlockId(*next_id),
                instructions: Vec::new(),
                yield_value: None,
                terminator: None,
            },
        ));

        *next_id += 1;
        yields.push((segments.len() - 1, key, node));
    }

    fn block_use_def(&self) -> (FxHashSet<VariableKey>, FxHashSet<VariableKey>) {
        let mut use_set = FxHashSet::default();
        let mut def_set = FxHashSet::default();
        let mut tmp_use = FxHashSet::default();
        let mut tmp_def = FxHashSet::default();

        for instr in &self.instructions {
            tmp_use.clear();
            tmp_def.clear();
            instr.node_type.uses(&mut tmp_use);
            instr.node_type.defs(&mut tmp_def);

            for u in tmp_use.drain() {
                if !def_set.contains(&u) {
                    use_set.insert(u);
                }
            }

            def_set.extend(tmp_def.iter().cloned());
        }

        if let Some(t) = &self.terminator {
            tmp_use.clear();
            t.uses(&mut tmp_use);

            for u in tmp_use.drain() {
                if !def_set.contains(&u) {
                    use_set.insert(u);
                }
            }
        }

        (use_set, def_set)
    }
}

impl LirRegistry {
    pub fn lower_generators(&mut self) {
        let resolve_native = |name: &str| {
            self.natives
                .get(&Ustr::from(name))
                .and_then(|key| match key {
                    Key::VariableKey(var_key) => Some(var_key.clone()),
                    _ => None,
                })
                .unwrap_or_else(|| VariableKey::from(name))
        };

        let mut ctx = Context {
            suspend: resolve_native("gen_suspend"),
            some: resolve_native("some"),
            none: resolve_native("none"),
            counter: 0,
        };

        let pairs = self.find_gen_pairs();

        if pairs.is_empty() {
            return;
        }

        let mut done: FxHashSet<VariableKey> = FxHashSet::default();

        for (label, key) in pairs {
            if done.contains(&label) {
                continue;
            }

            done.insert(label.clone());

            if let Some((type_key, field_count)) = ctx.transform_function(self, &label) {
                self.patch_state_literal(&key, &type_key, field_count);
            }
        }
    }

    fn find_gen_pairs(&self) -> Vec<(VariableKey, VariableKey)> {
        let mut pairs = Vec::new();

        for func in self.functions.values() {
            for block in func.blocks.iter().flatten() {
                let mut aggregates = Vec::new();
                for node in &block.instructions {
                    node.node_type.collect_aggregates(&mut aggregates);
                }

                match &block.terminator {
                    Some(LirTerminator::Branch { condition, .. }) => {
                        condition.collect_aggregates(&mut aggregates)
                    }
                    Some(LirTerminator::Return {
                        value: Some(value), ..
                    }) => value.collect_aggregates(&mut aggregates),
                    _ => {}
                }

                for LirAggregate { name, fields } in aggregates {
                    if !name
                        .as_ref()
                        .is_some_and(|n| n.name() == "gen" || n.name().starts_with("gen_"))
                    {
                        continue;
                    }

                    let data_key = fields.get("data").and_then(|v| match v {
                        LirNodeType::Load(LirLoad { value })
                        | LirNodeType::Move(LirMove { value })
                        | LirNodeType::RefLoad(LirRefLoad { value }) => Some(value.clone()),
                        _ => None,
                    });

                    let state_key = fields.get("state").and_then(|v| match v {
                        LirNodeType::Load(LirLoad { value })
                        | LirNodeType::Move(LirMove { value })
                        | LirNodeType::RefLoad(LirRefLoad { value }) => Some(value.clone()),
                        _ => None,
                    });

                    if let (Some(data_key), Some(state_key)) = (data_key, state_key) {
                        'outer: for func in self.functions.values() {
                            for block in func.blocks.iter().flatten() {
                                for node in &block.instructions {
                                    if let LirNodeType::Declare(declare) = &node.node_type
                                        && declare.dest == data_key
                                        && let LirNodeType::Closure(func) = declare.value.as_ref()
                                    {
                                        pairs.push((func.label.clone(), state_key.clone()));
                                        break 'outer;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        pairs
    }

    fn patch_state_literal(&mut self, key: &VariableKey, type_key: &TypeKey, field_count: usize) {
        let data_type = MirDataType::Struct {
            identifier: type_key.clone(),
            generic_types: Vec::new(),
        };

        let mut fields: Vec<(Ustr, LirNodeType)> = vec![
            (Ustr::from("tag"), LirNodeType::Literal(LirLiteral::UInt(0))),
            (Ustr::from("yield"), LirNodeType::null()),
        ];

        for i in 0..field_count {
            fields.push((Ustr::from(&format!("v{}", i)), LirNodeType::null()));
        }

        let node = LirNodeType::Aggregate(LirAggregate {
            name: Some(type_key.clone()),
            fields: ObjectMap(fields),
        });

        for func in self.functions.values_mut() {
            for block in func.blocks.iter_mut().flatten() {
                for instr in block.instructions.iter_mut() {
                    if let LirNodeType::Declare(d) = &mut instr.node_type
                        && d.dest == *key
                    {
                        *d.value = node.clone();
                        d.data_type = data_type.clone();
                        return;
                    }
                }
            }
        }
    }
}
