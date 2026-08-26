use crate::errors::MiddleErr;
use calibre_parser::{
    Location, Span,
    ast::{
        idents::{ParserText, PotentialDollarIdentifier},
        nodes::Node,
        types::ParserInnerType,
    },
};
use rustc_hash::{FxHashMap, FxHashSet};
use std::path::PathBuf;

pub mod resolve;

#[derive(Debug, Clone, Default)]
pub struct Scoping {
    pub scope_counter: u64,
    pub scopes: FxHashMap<u64, MiddleScope>,
    pub loaded_scopes: FxHashSet<u64>,
    pub loop_stack: Vec<LoopContext>,
    pub return_type_stack: Vec<ParserInnerType>,
    pub generic_param_stack: Vec<Vec<String>>,
    pub all_time_generics: FxHashSet<String>,
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

    pub fn push_generic_params(&mut self, params: Vec<String>) {
        self.all_time_generics.extend(params.clone());
        self.generic_param_stack.push(params);
    }

    pub fn pop_generic_params(&mut self) {
        self.generic_param_stack.pop();
    }

    pub fn is_generic_param(&self, ident: &str) -> bool {
        for params in self.generic_param_stack.iter().rev() {
            if params.contains(&ident.to_string()) {
                return true;
            }
        }
        false
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

    pub fn get_global_scope(&self) -> &MiddleScope {
        self.scopes.get(&0).unwrap_or_else(|| empty_scope())
    }

    pub fn get_root_scope(&self) -> &MiddleScope {
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

    pub fn add_scope(&mut self, mut scope: MiddleScope) {
        scope.id = self.scope_counter;
        self.scopes.insert(scope.id, scope);
        self.scope_counter += 1;
    }

    pub fn new_scope(
        &mut self,
        parent: Option<u64>,
        path: PathBuf,
        namespace: Option<&str>,
    ) -> u64 {
        if let (Some(parent_id), Some(ns)) = (parent, namespace) {
            let existing = self.scopes.values().find_map(|scope| {
                if scope.namespace != ns {
                    return None;
                }
                if scope.path == path {
                    return Some(scope.id);
                }
                let left = std::fs::canonicalize(&scope.path).ok();
                let right = std::fs::canonicalize(&path).ok();
                if left.is_some() && left == right {
                    Some(scope.id)
                } else {
                    None
                }
            });
            if let Some(existing_id) = existing {
                if let Some(parent_scope) = self.scopes.get_mut(&parent_id) {
                    parent_scope.children.insert(ns.to_string(), existing_id);
                }
                return existing_id;
            }
        }

        if let Some(parent) = parent {
            let scope = MiddleScope {
                macros: FxHashMap::default(),
                macro_args: FxHashMap::default(),
                id: self.scope_counter,
                namespace: namespace
                    .unwrap_or(&self.scope_counter.to_string())
                    .to_string(),
                parent: Some(parent),
                children: FxHashMap::default(),
                mappings: FxHashMap::default(),
                type_mappings: FxHashMap::default(),
                defers: Vec::new(),
                path,
            };

            self.add_scope(scope);

            if let Some(scope_ref) = self.scopes.get_mut(&parent) {
                scope_ref.children.insert(
                    namespace
                        .map(String::from)
                        .unwrap_or((self.scope_counter - 1).to_string()),
                    self.scope_counter - 1,
                );
            }

            self.scope_counter - 1
        } else {
            let scope = MiddleScope {
                macros: FxHashMap::default(),
                macro_args: FxHashMap::default(),
                id: self.scope_counter,
                namespace: namespace
                    .unwrap_or(&self.scope_counter.to_string())
                    .to_string(),
                parent: None,
                children: FxHashMap::default(),
                mappings: FxHashMap::default(),
                type_mappings: FxHashMap::default(),
                defers: Vec::new(),
                path,
            };
            self.add_scope(scope);
            self.scope_counter - 1
        }
    }

    pub fn new_scope_from_parent_shallow(&mut self, parent: u64) -> u64 {
        let Some(path) = self.scopes.get(&parent).map(|s| s.path.clone()) else {
            return parent;
        };
        self.new_scope(Some(parent), path, None)
    }

    pub fn new_build_scope_from_parent(&mut self, parent: u64, namespace: &str) -> Option<u64> {
        let path = self.scopes.get(&parent)?.path.clone();
        let parent_name = path.file_name()?;
        let folder = path.parent()?.to_path_buf();

        let extra = if parent_name == "main.cal" || parent_name == "mod.cal" {
            String::new()
        } else {
            let parent_str = parent_name.to_str()?;
            let base = parent_str.split('.').next()?;
            format!("{base}/")
        };

        let mut path1 = folder.clone();
        path1 = path1.join(format!("{extra}{namespace}/build.cal"));

        if path1.exists() {
            Some(self.new_scope(Some(parent), path1, Some(namespace)))
        } else {
            None
        }
    }

    pub fn get_scope_from_path(
        &self,
        path: &[String],
        mut parent: Option<u64>,
    ) -> Result<u64, MiddleErr> {
        let mut skip = 0;

        if parent.is_none() {
            parent = self
                .scopes
                .iter()
                .find(|(_, v)| v.namespace == path[0])
                .map(|x| x.0)
                .cloned();

            if parent.is_none() {
                return Err(MiddleErr::Scope(path[0].clone()));
            }

            skip = 1;
        }

        for name in path.iter().skip(skip) {
            if let Some(p) = parent {
                parent = Some(self.get_scope_from_parent(p, name)?);
            }
        }

        parent.ok_or_else(|| MiddleErr::Scope(path.join("::")))
    }

    pub fn get_scope_from_parent(&self, parent: u64, namespace: &str) -> Result<u64, MiddleErr> {
        let parent_scope = self
            .scopes
            .get(&parent)
            .ok_or_else(|| MiddleErr::Internal(format!("missing scope {parent}")))?;

        for (_, child) in parent_scope.children.iter() {
            if let Some(x) = self.scopes.get(child)
                && x.namespace == namespace
            {
                return Ok(x.id);
            }
        }

        Err(MiddleErr::Scope(namespace.to_string()))
    }

    pub fn new_scope_from_parent(
        &mut self,
        parent: u64,
        namespace: &str,
    ) -> Result<u64, MiddleErr> {
        if let Ok(scope) = self.get_scope_from_parent(parent, namespace) {
            return Ok(scope);
        }

        let path = self
            .scopes
            .get(&parent)
            .ok_or_else(|| MiddleErr::Internal(format!("missing parent scope {parent}")))?
            .path
            .clone();
        let parent_name = path.file_name().ok_or_else(|| {
            MiddleErr::Internal(format!("missing parent filename for scope {parent}"))
        })?;
        let folder = path.parent().ok_or_else(|| {
            MiddleErr::Internal(format!("missing parent directory for scope {parent}"))
        })?;

        let extra = if parent_name == "main.cal" || parent_name == "mod.cal" {
            String::new()
        } else {
            let parent_str = parent_name.to_str().ok_or_else(|| {
                MiddleErr::Internal(format!("invalid parent filename for scope {parent}"))
            })?;
            let base = parent_str.split('.').next().ok_or_else(|| {
                MiddleErr::Internal(format!("invalid parent filename for scope {parent}"))
            })?;
            format!("{base}/")
        };

        let path_ends = [".cal", "/main.cal", "/mod.cal"];
        let path_starts = [format!("{extra}{namespace}"), namespace.to_string()];
        let paths: Vec<PathBuf> = path_starts
            .into_iter()
            .flat_map(|x| {
                let folder = folder.to_path_buf();
                path_ends
                    .iter()
                    .map(|y| folder.join(format!("{}{}", x, y)))
                    .collect::<Vec<_>>()
            })
            .collect();

        for path in paths.clone() {
            if path.exists() {
                return Ok(self.new_scope(Some(parent), path, Some(namespace)));
            }
        }

        Err(MiddleErr::Scope(format!(
            "could not resolve module {namespace}; tried {paths:?}"
        )))
    }

    pub fn collect_defers_until(&self, scope: &u64, stop_scope: Option<u64>) -> Vec<Node> {
        let mut out = Vec::new();
        let mut current = Some(*scope);
        while let Some(id) = current {
            let Some(s) = self.scopes.get(&id) else {
                break;
            };
            if stop_scope.is_some_and(|stop| stop == id) {
                break;
            }
            out.extend(s.defers.clone());
            current = s.parent;
        }
        out
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
        type_mappings: FxHashMap::default(),
        mappings: FxHashMap::default(),
        macros: FxHashMap::default(),
        macro_args: FxHashMap::default(),
        children: FxHashMap::default(),
        namespace: "empty".to_string(),
        path: PathBuf::new(),
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
    pub type_mappings: FxHashMap<String, ParserInnerType>,
    pub macros: FxHashMap<String, ScopeMacro>,
    pub macro_args: FxHashMap<String, Node>,
    pub children: FxHashMap<String, u64>,
    pub namespace: String,
    pub path: PathBuf,
    pub defers: Vec<Node>,
}

impl MiddleScope {
    #[inline]
    pub fn path_or_fallback(&self) -> String {
        let file = self.path.to_string_lossy().to_string();
        if file.is_empty() {
            String::from("unknown")
        } else {
            file
        }
    }
}
