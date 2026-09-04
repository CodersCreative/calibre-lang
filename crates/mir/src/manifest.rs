use crate::{
    environment::MiddleEnvironment,
    scoping::{MiddleScope, ScopeId, Scoping},
    symbols::{MiddleOverload, MiddleVariable, Symbols},
    tags::{Tagging, context::PackageMetadata},
    typing::Typing,
};
use calibre_parser::ast::nodes::AstNode;
use calibre_parser::ast::types::ParserInnerType;
use indextree::Arena;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use ustr::{Ustr, UstrMap, UstrSet};

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct Manifest {
    pub metadata: PackageMetadata,
    pub scoping: ManifestScoping,
    pub symbols: ManifestSymbols,
    pub typing: Typing,
    pub tagging: ManifestTagging,
}

// TODO Convert to a TryFrom
// Makes the assertion that package_metadata is set... could be bad...
impl From<&MiddleEnvironment> for Manifest {
    fn from(value: &MiddleEnvironment) -> Self {
        Self {
            metadata: value
                .context
                .package_metadata
                .clone()
                .expect("Package Metadata needs to be set to build a Manifest"),
            scoping: ManifestScoping::from(&value.scoping),
            symbols: ManifestSymbols::from(&value.symbols),
            tagging: ManifestTagging::from(&value.tagging),
            typing: value.typing.clone(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct ManifestTagging {
    pub init_functions: Vec<(i32, Ustr)>,
    pub fin_functions: Vec<(i32, Ustr)>,
}

impl From<&Tagging> for ManifestTagging {
    fn from(value: &Tagging) -> Self {
        Self {
            init_functions: value.init_functions.clone(),
            fin_functions: value.fin_functions.clone(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct ManifestSymbols {
    pub variables: UstrMap<MiddleVariable>,
    pub native_mappings: UstrMap<Ustr>,
    pub overloads: Vec<MiddleOverload>,
    pub generic_fn_templates: UstrMap<(
        Vec<Ustr>,
        calibre_parser::ast::nodes::FunctionHeader,
        AstNode,
    )>,
    pub fn_specializations: UstrMap<Ustr>,
}

impl From<&Symbols> for ManifestSymbols {
    fn from(value: &Symbols) -> Self {
        Self {
            variables: value.variables.clone(),
            native_mappings: value.native_mappings.clone(),
            overloads: value.overloads.clone(),
            generic_fn_templates: value.generic_fn_templates.clone(),
            fn_specializations: value.fn_specializations.clone(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct ManifestScope {
    pub namespace: Ustr,
    pub mappings: UstrMap<Ustr>,
    pub type_mappings: UstrMap<ParserInnerType>,
    pub children: UstrMap<ScopeId>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct ManifestScoping {
    pub all_time_generics: UstrSet,
    pub scopes: Arena<ManifestScope>,
}

impl From<&Scoping> for ManifestScoping {
    fn from(value: &Scoping) -> Self {
        let mut scopes = Arena::default();
        let mut ids = FxHashMap::default();

        for node in value.scopes.iter() {
            let original_id = value.scopes.get_node_id(node).unwrap();
            let scope = node.get();

            let manifest = ManifestScope {
                namespace: scope.namespace,
                mappings: scope.mappings.clone(),
                type_mappings: scope.type_mappings.clone(),
                children: scope.children.clone(),
            };

            let new_id = scopes.new_node(manifest);
            ids.insert(original_id, new_id);
        }

        for node in value.scopes.iter() {
            let original_id = value.scopes.get_node_id(node).unwrap();
            if let Some(new_id) = ids.get(&original_id)
                && let Some(scope_mut) = scopes.get_mut(*new_id)
            {
                let children = scope_mut
                    .get()
                    .children
                    .iter()
                    .filter_map(|(k, v)| ids.get(v).map(|&new_v| (*k, new_v)))
                    .collect();
                scope_mut.get_mut().children = children;
            }
        }

        for node in value.scopes.iter() {
            let original_id = value.scopes.get_node_id(node).unwrap();

            if let Some(new_id) = ids.get(&original_id)
                && let Some(original_parent) = original_id.parent(&value.scopes)
                && let Some(new_parent_id) = ids.get(&original_parent)
            {
                new_parent_id.append(*new_id, &mut scopes);
            }
        }

        Self {
            all_time_generics: value.all_time_generics.clone(),
            scopes,
        }
    }
}

impl Scoping {
    pub fn append_manifest(&mut self, value: ManifestScoping) {
        let mut ids = FxHashMap::default();

        for node in value.scopes.iter() {
            let original_id = value.scopes.get_node_id(node).unwrap();
            let scope = node.get();

            let middle_scope = MiddleScope {
                namespace: scope.namespace,
                mappings: scope.mappings.clone(),
                type_mappings: scope.type_mappings.clone(),
                macros: Default::default(),
                macro_args: Default::default(),
                children: scope.children.clone(),
                path: Default::default(),
                defers: Default::default(),
                built: true,
            };

            let new_id = self.scopes.new_node(middle_scope);
            ids.insert(original_id, new_id);
        }

        for node in value.scopes.iter() {
            let original_id = value.scopes.get_node_id(node).unwrap();
            if let Some(new_id) = ids.get(&original_id)
                && let Some(scope_mut) = self.scopes.get_mut(*new_id)
            {
                let children = scope_mut
                    .get()
                    .children
                    .iter()
                    .filter_map(|(k, v)| ids.get(v).map(|&new_v| (*k, new_v)))
                    .collect();
                scope_mut.get_mut().children = children;
            }
        }

        for node in value.scopes.iter() {
            let original_id = value.scopes.get_node_id(node).unwrap();

            if let Some(new_id) = ids.get(&original_id)
                && let Some(original_parent) = original_id.parent(&value.scopes)
                && let Some(new_parent_id) = ids.get(&original_parent)
            {
                new_parent_id.append(*new_id, &mut self.scopes);
            }
        }

        self.all_time_generics.extend(value.all_time_generics);
    }
}

impl From<ManifestScoping> for Scoping {
    fn from(value: ManifestScoping) -> Self {
        let mut scoping = Self::default();
        scoping.append_manifest(value);
        scoping
    }
}
