use crate::errors::MiddleErr;
use calibre_parser::{
    Location, Span,
    ast::{idents::PotentialDollarIdentifier, nodes::AstNode, types::ParserInnerType},
};
use indextree::{Arena, Node, NodeId};
use std::path::PathBuf;
use ustr::{Ustr, UstrMap, UstrSet};

pub mod resolve;
pub type ScopeId = NodeId;

#[derive(Debug, Clone, Default)]
pub struct Scoping {
    pub scopes: Arena<MiddleScope>,
    pub loop_stack: Vec<LoopContext>,
    pub return_type_stack: Vec<ParserInnerType>,
    pub generic_param_stack: Vec<Vec<Ustr>>,
    pub all_time_generics: UstrSet,
}

impl Scoping {
    #[inline(always)]
    pub fn scope_or_err(&self, scope: ScopeId) -> Result<&MiddleScope, MiddleErr> {
        self.scopes
            .get(scope)
            .map(|x| x.get())
            .ok_or_else(|| MiddleErr::Internal(format!("missing scope {scope}")))
    }

    #[inline(always)]
    pub fn scope_mut_or_err(&mut self, scope: ScopeId) -> Result<&mut MiddleScope, MiddleErr> {
        self.scopes
            .get_mut(scope)
            .map(|x| x.get_mut())
            .ok_or_else(|| MiddleErr::Internal(format!("missing scope {scope}")))
    }

    #[inline(always)]
    pub fn push_generic_params(&mut self, params: Vec<Ustr>) {
        self.all_time_generics.extend(params.clone());
        self.generic_param_stack.push(params);
    }

    #[inline(always)]
    pub fn pop_generic_params(&mut self) {
        self.generic_param_stack.pop();
    }

    #[inline(always)]
    pub fn is_generic_param(&self, ident: &Ustr) -> bool {
        for params in self.generic_param_stack.iter().rev() {
            if params.contains(ident) {
                return true;
            }
        }
        false
    }

    #[inline(always)]
    pub fn get_location(&self, scope: ScopeId, span: Span) -> Option<Location> {
        self.scope_or_err(scope).ok().map(|s| Location {
            path: s.path.clone(),
            span,
        })
    }

    #[inline(always)]
    pub fn resolve_macro_arg(&self, scope: ScopeId, iden: &Ustr) -> Option<&AstNode> {
        scope.ancestors(&self.scopes).find_map(|x| {
            self.scope_or_err(x.clone())
                .ok()
                .and_then(|x| x.macro_args.get(iden))
        })
    }

    #[inline(always)]
    pub fn resolve_macro(&self, scope: ScopeId, iden: &Ustr) -> Option<&ScopeMacro> {
        scope.ancestors(&self.scopes).find_map(|x| {
            self.scope_or_err(x.clone())
                .ok()
                .and_then(|x| x.macros.get(iden))
        })
    }

    #[inline(always)]
    pub fn get_global_scope(&self) -> Option<ScopeId> {
        self.scopes.roots().next()
    }

    #[inline(always)]
    pub fn get_parent(&self, scope: ScopeId) -> Option<ScopeId> {
        scope.ancestors(&self.scopes).nth(1)
    }

    pub fn get_id(&self, scope: &Node<MiddleScope>) -> Option<ScopeId> {
        self.scopes.get_node_id(scope)
    }

    #[inline(always)]
    pub fn get_root_scope(&self) -> Option<ScopeId> {
        self.scopes.iter().find_map(|x| {
            if x.get().namespace == "root" {
                self.get_id(x)
            } else {
                None
            }
        })
    }

    #[inline(always)]
    pub fn add_scope(&mut self, scope: MiddleScope, parent: Option<ScopeId>) -> ScopeId {
        let scope = self.scopes.new_node(scope);

        if let Some(parent) = parent {
            parent.append(scope, &mut self.scopes);
        }

        scope
    }

    pub fn new_scope(
        &mut self,
        parent: Option<ScopeId>,
        path: PathBuf,
        namespace: Option<&Ustr>,
    ) -> ScopeId {
        if let (Some(parent_id), Some(ns)) = (parent, namespace) {
            let existing = self.scopes.iter().find_map(|scope| {
                let scope_ref = scope.get();
                if &scope_ref.namespace != ns {
                    return None;
                }

                if scope_ref.path == path {
                    return Some(self.get_id(scope)?);
                }

                let left = std::fs::canonicalize(&scope_ref.path).ok();
                let right = std::fs::canonicalize(&path).ok();
                if left.is_some() && left == right {
                    Some(self.get_id(scope)?)
                } else {
                    None
                }
            });

            if let Some(existing_id) = existing {
                parent_id.append(existing_id, &mut self.scopes);
                return existing_id;
            }
        }

        let id = self.add_scope(
            MiddleScope {
                macros: UstrMap::default(),
                macro_args: UstrMap::default(),
                namespace: namespace.cloned().unwrap_or_default(),
                mappings: UstrMap::default(),
                type_mappings: UstrMap::default(),
                children: UstrMap::default(),
                defers: Vec::new(),
                path,
                built: false,
            },
            parent,
        );

        id
    }

    pub fn new_scope_from_parent_shallow(&mut self, parent: ScopeId) -> ScopeId {
        let Ok(path) = self.scope_or_err(parent).map(|s| s.path.clone()) else {
            return parent;
        };
        self.new_scope(Some(parent), path, None)
    }

    pub fn new_build_scope_from_parent(
        &mut self,
        parent: ScopeId,
        namespace: &Ustr,
    ) -> Option<ScopeId> {
        let path = self.scope_or_err(parent).ok()?.path.clone();
        let parent_name = path.file_name()?;
        let folder = path.parent()?.to_path_buf();

        let extra = if parent_name == "main.cal" || parent_name == "mod.cal" {
            Ustr::default()
        } else {
            let parent_str = parent_name.to_str()?;
            let base = parent_str.split('.').next()?;
            Ustr::from(&format!("{base}/"))
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
        path: &[Ustr],
        mut parent: Option<ScopeId>,
    ) -> Result<ScopeId, MiddleErr> {
        let mut skip = 0;

        if parent.is_none() {
            parent = self
                .scopes
                .iter()
                .find(|v| v.get().namespace == path[0])
                .and_then(|x| self.get_id(x));

            if parent.is_none() {
                return Err(MiddleErr::Scope(path[0].to_string()));
            }

            skip = 1;
        }

        for name in path.iter().skip(skip) {
            if let Some(p) = parent {
                parent = Some(self.get_scope_from_children(p, name)?);
            }
        }

        parent.ok_or_else(|| {
            MiddleErr::Scope(
                path.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join("::"),
            )
        })
    }

    #[inline(always)]
    pub fn get_scope_from_children(
        &self,
        parent: ScopeId,
        namespace: &Ustr,
    ) -> Result<ScopeId, MiddleErr> {
        if let Some(x) = parent.children(&self.scopes).find(|child| {
            self.scope_or_err(*child)
                .is_ok_and(|x| &x.namespace == namespace)
        }) {
            Ok(x)
        } else if let Some(x) = self
            .scope_or_err(parent)
            .ok()
            .and_then(|x| x.children.get(namespace))
        {
            Ok(*x)
        } else {
            Err(MiddleErr::Scope(namespace.to_string()))
        }
    }

    pub fn new_scope_from_parent(
        &mut self,
        parent: ScopeId,
        namespace: &Ustr,
    ) -> Result<ScopeId, MiddleErr> {
        if let Ok(scope) = self.get_scope_from_children(parent, namespace) {
            return Ok(scope);
        }

        let path = self.scope_or_err(parent)?.path.clone();

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

    #[inline(always)]
    pub fn collect_defers_until(
        &self,
        scope: ScopeId,
        stop_scope: Option<ScopeId>,
    ) -> Vec<AstNode> {
        let mut out = Vec::new();

        scope
            .ancestors(&self.scopes)
            .take_while(|id| !stop_scope.is_some_and(|stop| &stop == id))
            .for_each(|x| {
                if let Ok(s) = self.scope_or_err(x) {
                    out.extend(s.defers.clone());
                }
            });

        out
    }
}

#[derive(Debug, Clone)]
pub struct LoopContext {
    pub label: Option<Ustr>,
    pub result_target: Option<Ustr>,
    pub broke_target: Option<Ustr>,
    pub continue_inject: Option<AstNode>,
    pub scope_id: ScopeId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ScopeMacro {
    pub name: Ustr,
    pub args: Vec<(PotentialDollarIdentifier, AstNode)>,
    pub body: Vec<AstNode>,
    pub create_new_scope: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleScope {
    pub mappings: UstrMap<Ustr>,
    pub type_mappings: UstrMap<ParserInnerType>,
    pub macros: UstrMap<ScopeMacro>,
    pub macro_args: UstrMap<AstNode>,
    pub children: UstrMap<NodeId>,
    pub namespace: Ustr,
    pub path: PathBuf,
    pub defers: Vec<AstNode>,
    pub built: bool,
}

impl MiddleScope {
    #[inline(always)]
    pub fn path_or_fallback(&self) -> Ustr {
        let file = Ustr::from(&self.path.to_string_lossy());
        if file.is_empty() {
            Ustr::from("unknown")
        } else {
            file
        }
    }
}
