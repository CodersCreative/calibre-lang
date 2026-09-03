use crate::{
    environment::MiddleEnvironment,
    scoping::Scoping,
    symbols::{MiddleOverload, MiddleVariable, Symbols},
    tags::context::PackageMetadata,
    typing::Typing,
};
use calibre_parser::ast::types::ParserInnerType;
use indextree::Arena;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use ustr::{Ustr, UstrMap, UstrSet};

#[derive(Serialize, Deserialize, Default)]
pub struct Manifest {
    pub metadata: Option<PackageMetadata>,
    pub scoping: ManifestScoping,
    pub symbols: ManifestSymbols,
    pub typing: Typing,
}

impl From<&MiddleEnvironment> for Manifest {
    fn from(value: &MiddleEnvironment) -> Self {
        Self {
            metadata: value.context.package_metadata.clone(),
            scoping: ManifestScoping::from(&value.scoping),
            symbols: ManifestSymbols::from(&value.symbols),
            typing: value.typing.clone(),
        }
    }
}

#[derive(Serialize, Deserialize, Default)]
pub struct ManifestSymbols {
    pub variables: UstrMap<MiddleVariable>,
    pub native_mappings: UstrMap<Ustr>,
    pub overloads: Vec<MiddleOverload>,
}

impl From<&Symbols> for ManifestSymbols {
    fn from(value: &Symbols) -> Self {
        Self {
            variables: value.variables.clone(),
            native_mappings: value.native_mappings.clone(),
            overloads: value.overloads.clone(),
        }
    }
}

#[derive(Serialize, Deserialize, Default)]
pub struct ManifestScope {
    pub namespace: Ustr,
    pub mappings: UstrMap<Ustr>,
    pub type_mappings: UstrMap<ParserInnerType>,
}

#[derive(Serialize, Deserialize, Default)]
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
            };

            let new_id = scopes.new_node(manifest);
            ids.insert(original_id, new_id);
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
