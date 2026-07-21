use crate::errors::MiddleErr;
use calibre_parser::{
    Location, Span,
    ast::{Node, ParserText, PotentialDollarIdentifier},
};
use rustc_hash::{FxHashMap, FxHashSet};
use std::path::PathBuf;

#[derive(Debug, Clone, Default)]
pub struct Scoping {
    pub scope_counter: u64,
    pub scopes: FxHashMap<u64, MiddleScope>,
    pub loaded_scopes: FxHashSet<u64>,
    pub loop_stack: Vec<LoopContext>,
}

impl Scoping {
    pub fn scope_or_err(&self, scope: &u64) -> Result<&MiddleScope, MiddleErr> {
        self.scopes
            .get(scope)
            .ok_or_else(|| MiddleErr::Internal(format!("missing scope {scope}")))
    }

    pub fn scope_mut_or_err(&mut self, scope: &u64) -> Result<&mut MiddleScope, MiddleErr> {
        self.scopes
            .get_mut(scope)
            .ok_or_else(|| MiddleErr::Internal(format!("missing scope {scope}")))
    }

    pub fn get_location(&self, scope: &u64, span: Span) -> Option<Location> {
        self.scopes.get(scope).map(|s| Location {
            path: s.path.clone(),
            span,
        })
    }

    pub fn resolve_macro_arg(&self, scope: &u64, iden: &str) -> Option<&Node> {
        let scope = self.scopes.get(scope)?;

        if let Some(x) = scope.macro_args.get(iden) {
            Some(x)
        } else if let Some(parent) = scope.parent.as_ref() {
            self.resolve_macro_arg(parent, iden)
        } else {
            None
        }
    }

    pub fn resolve_macro(&self, scope: &u64, iden: &str) -> Option<&ScopeMacro> {
        let scope = self.scopes.get(scope)?;

        if let Some(x) = scope.macros.get(iden) {
            Some(x)
        } else if let Some(parent) = scope.parent.as_ref() {
            self.resolve_macro(parent, iden)
        } else {
            None
        }
    }

    pub fn get_global_scope<'a>(&'a self) -> &'a MiddleScope {
        self.scopes.get(&0).unwrap_or_else(|| empty_scope())
    }

    pub fn get_root_scope<'a>(&'a self) -> &'a MiddleScope {
        for i in 1..self.scope_counter {
            if let Some(scope) = self.scopes.get(&i)
                && scope.namespace == "root"
                && scope.parent == Some(0)
            {
                return scope;
            }
        }

        self.scopes.get(&0).unwrap_or_else(|| empty_scope())
    }
}

#[derive(Debug, Clone, Default)]
pub struct LoopContext {
    pub label: Option<String>,
    pub result_target: Option<ParserText>,
    pub broke_target: Option<ParserText>,
    pub continue_inject: Option<Node>,
    pub scope_id: u64,
}

fn empty_scope() -> &'static MiddleScope {
    static EMPTY: std::sync::OnceLock<MiddleScope> = std::sync::OnceLock::new();
    EMPTY.get_or_init(|| MiddleScope {
        id: 0,
        parent: None,
        mappings: FxHashMap::default(),
        macros: FxHashMap::default(),
        macro_args: FxHashMap::default(),
        children: FxHashMap::default(),
        namespace: "empty".to_string(),
        path: PathBuf::new(),
        defined: Vec::new(),
        defers: Vec::new(),
    })
}

#[derive(Debug, Clone, PartialEq)]
pub struct ScopeMacro {
    pub name: String,
    pub args: Vec<(PotentialDollarIdentifier, Node)>,
    pub body: Vec<Node>,
    pub create_new_scope: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleScope {
    pub id: u64,
    pub parent: Option<u64>,
    pub mappings: FxHashMap<String, String>,
    pub macros: FxHashMap<String, ScopeMacro>,
    pub macro_args: FxHashMap<String, Node>,
    pub children: FxHashMap<String, u64>,
    pub namespace: String,
    pub path: PathBuf,
    pub defined: Vec<String>,
    pub defers: Vec<Node>,
}
