use calibre_mir::{environment::MiddleEnvironment, scoping::ScopeId};
use calibre_parser::{Span, ast::types::ParserDataType};
use indextree::{Arena, NodeId};
use rustc_hash::FxHashMap;
use std::fmt::Display;
use std::sync::atomic::Ordering;
use tracing::{debug, instrument};

use crate::{
    COUNTER, ast::{BlockId, LirBlock, LirLiteral, LirNode, LirNodeType, LirTerminator},
};

#[derive(Debug, Clone, Default)]
pub struct LirRegistry {
    pub nodes : LirNodes,
    pub functions: FxHashMap<String, LirFunction>,
    pub globals: FxHashMap<String, LirGlobal>,
    pub natives: FxHashMap<String, String>,
    pub dyn_vtables: FxHashMap<String, FxHashMap<String, FxHashMap<String, String>>>,
    pub scope_to_file: FxHashMap<ScopeId, String>,
}


impl LirRegistry {
    pub fn append(&mut self, other: LirRegistry) {
        self.functions.extend(other.functions);
        for (concrete, trait_map) in other.dyn_vtables {
            let entry = self.dyn_vtables.entry(concrete).or_default();
            for (trait_name, methods) in trait_map {
                entry.entry(trait_name).or_default().extend(methods);
            }
        }
    }
}

impl Display for LirRegistry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for val in &self.globals {
            writeln!(f, "{}", val.1)?;
        }

        for func in &self.functions {
            writeln!(f, "{}\n", func.1)?;
        }

        Ok(())
    }
}

pub type LirId = NodeId;

#[derive(Debug, Clone)]
pub struct LirNodes {
    nodes: Arena<LirNodeType>,
    spans: FxHashMap<LirId, Span>,
    pub null : LirId,
    pub noop : LirId,
}

impl Default for LirNodes {
    fn default() -> Self {
        let mut nodes = Arena::default();
        let null = nodes.new_node(LirNodeType::Literal(LirLiteral::Null));
        let noop = nodes.new_node(LirNodeType::Noop);

        let mut spans = FxHashMap::default();
        spans.insert(null, Span::default());
        spans.insert(noop, Span::default());

        Self { nodes, spans, null, noop }
    }
}

impl LirNodes {
    pub fn add(&mut self, node : LirNodeType, span : Span) -> LirId {
        let id = self.nodes.new_node(node);
        self.spans.insert(id, span);
        id
    }

    pub fn get(&self, id : LirId) -> Option<&LirNodeType> {
        self.nodes.get(id).map(|x| x.get())
    }

    pub fn get_mut(&mut self, id : LirId) -> Option<&mut LirNodeType> {
        self.nodes.get_mut(id).map(|x| x.get_mut())
    }

    pub fn add_with_parent(&mut self, node : LirNodeType, parent : LirId, span : Span) -> LirId {
        let id = self.add(node, span);
        parent.append(id, &mut self.nodes);
        id
    }

    pub fn add_with_children(&mut self, node : LirNodeType, children : impl Iterator<Item = LirId>, span : Span) -> LirId {
        let id = self.add(node, span);
        for child in children {
            id.append(child, &mut self.nodes);
        }
        
        id
    }
}

#[derive(Debug, Clone)]
pub struct LirGlobal {
    pub name: Box<str>,
    pub data_type: ParserDataType,
    pub blocks: Box<[LirBlock]>,
}

impl Display for LirGlobal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut txt = format!("const {} : {} =", self.name, self.data_type);

        for block in &self.blocks {
            txt.push_str(&format!("\n{}", block).replace("\n", "\n\t"));
        }

        write!(f, "{}", txt)
    }
}

#[derive(Debug, Clone)]
pub struct LirFunction {
    pub name: Box<str>,
    pub params: Box<[(Box<str>, ParserDataType)]>,
    pub captures: Box<[(Box<str>, ParserDataType)]>,
    pub return_type: ParserDataType,
    pub blocks: Box<[LirBlock]>,
}

impl Display for LirFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut txt = format!("const {} = fn(", self.name);
        for param in &self.params {
            txt.push_str(&format!("{} : {}, ", param.0, param.1));
        }

        txt = txt.trim_end().trim_end_matches(",").to_string();
        txt.push_str(&format!(") -> {}:", self.return_type));

        for block in &self.blocks {
            txt.push_str(&format!("\n{}", block).replace("\n", "\n\t"));
        }

        write!(f, "{}", txt)
    }
}

#[derive(Debug, Clone)]
pub struct LirEnvironment<'a> {
    pub env: &'a MiddleEnvironment,
    pub last_ident: Option<String>,
    pub registry: LirRegistry,
    pub blocks: Vec<LirBlock>,
    pub current_block: BlockId,
    pub loop_stack: Vec<(BlockId, BlockId, Option<String>)>,
    pub allow_global_hoist: bool,
}

impl<'a> LirEnvironment<'a> {
    pub fn new(env: &'a MiddleEnvironment) -> Self {
        Self::new_with_hoist(env, true)
    }

    #[instrument(skip_all, fields(allow_global_hoist = allow_global_hoist))]
    pub fn new_with_hoist(env: &'a MiddleEnvironment, allow_global_hoist: bool) -> Self {
        debug!("creating LIR environment");
        let entry_id = BlockId(0);

        let scope_to_file: FxHashMap<ScopeId, String> = env
            .scoping
            .scopes
            .iter()
            .filter_map(|x| {
                env.scoping
                    .get_id(x)
                    .map(|id| (id, x.get().path.to_string_lossy().to_string()))
            })
            .collect();

        debug!(
            scope_count = scope_to_file.len(),
            "built scope to file mapping"
        );

        Self {
            env,
            last_ident: None,
            registry: LirRegistry {
                natives: env.symbols.native_mappings.clone(),
                dyn_vtables: Self::build_dyn_vtables(env),
                scope_to_file,
                ..Default::default()
            },
            blocks: vec![LirBlock {
                id: entry_id,
                instructions: vec![],
                terminator: None,
            }],
            current_block: entry_id,
            loop_stack: vec![],
            allow_global_hoist,
        }
    }

    pub fn get_temp(&mut self) -> String {
        let id = COUNTER.fetch_add(1, Ordering::Relaxed);
        format!("tmp_{}", id)
    }

    #[inline]
    pub fn current_block_open(&self) -> bool {
        self.blocks[self.current_block.0 as usize]
            .terminator
            .is_none()
    }

    #[inline]
    pub fn find_loop_target(&self, label: Option<&str>, use_exit: bool) -> Option<BlockId> {
        let labeled = label.and_then(|lbl| {
            self.loop_stack
                .iter()
                .rev()
                .find(|(_, _, l)| l.as_deref() == Some(lbl))
        });

        labeled
            .or_else(|| self.loop_stack.last())
            .map(|(header, exit, _)| if use_exit { *exit } else { *header })
    }

    pub fn add_instr(&mut self, instr: LirId) {
        let idx = self.current_block.0 as usize;
        self.blocks[idx].instructions.push(instr);
    }

    pub fn set_terminator(&mut self, term: LirTerminator) {
        let idx = self.current_block.0 as usize;
        if self.blocks[idx].terminator.is_none() {
            self.blocks[idx].terminator = Some(term);
        }
    }

    pub fn create_block(&mut self) -> BlockId {
        let id = BlockId(self.blocks.len() as u32);
        self.blocks.push(LirBlock {
            id,
            instructions: vec![],
            terminator: None,
        });
        id
    }

    pub fn add(&mut self, node : LirNodeType, span : Span) -> LirId {
        self.registry.nodes.add(node, span)
    }

    pub fn add_with_parent(&mut self, node : LirNodeType, parent : LirId, span : Span) -> LirId {
        self.registry.nodes.add_with_parent(node, parent, span)
    }

    pub fn add_with_children(&mut self, node : LirNodeType, children : impl Iterator<Item = LirId>, span : Span) -> LirId {
        self.registry.nodes.add_with_children(node, children, span)
    }

    pub fn switch_to(&mut self, id: BlockId) {
        self.current_block = id;
    }
}
