use std::fmt::Display;

use crate::environment::MiddleEnvironment;
use calibre_parser::{
    Location,
    ast::{
        ObjectMap, ObjectType,
        idents::ParserText,
        nodes::{Node, Overload, TypeDefType},
        types::{ParserDataType, ParserInnerType},
    },
};
use rustc_hash::{FxHashMap, FxHashSet};

#[derive(Debug, Clone, Default)]
pub struct Typing {
    pub objects: FxHashMap<String, MiddleObject>,
    pub impls: FxHashMap<ParserInnerType, MiddleImpl>,
    pub trait_defs: FxHashMap<String, MiddleTrait>,
    pub generic_type_templates: FxHashMap<String, (Vec<String>, TypeDefType, Vec<Overload>)>,
    pub type_specializations: FxHashMap<String, String>,
}

impl Typing {
    pub fn find_impl_for_type(&self, ty: &ParserDataType) -> Option<&MiddleImpl> {
        let key = ty.key();
        if let Some(imp) = self.impls.get(&key) {
            return Some(imp);
        }
        let target = ty.key();
        self.impls.values().find(|imp| {
            imp.data_type
                .data_type
                .matches(&target, &imp.generic_params)
        })
    }

    pub fn find_impl_for_type_mut(&mut self, ty: &ParserDataType) -> Option<&mut MiddleImpl> {
        let key = ty.key();
        if self.impls.contains_key(&key) {
            return self.impls.get_mut(&key);
        }
        let target = ty.key();
        let key = self
            .impls
            .iter()
            .find(|(_, imp)| {
                imp.data_type
                    .data_type
                    .matches(&target, &imp.generic_params)
            })
            .map(|(k, _)| k.clone())?;
        self.impls.get_mut(&key)
    }

    pub fn find_impl_member(&self, ty: &ParserDataType, member: &str) -> Option<&MiddleImplMember> {
        self.find_impl_for_type(ty)?.members.get(member)
    }

    pub fn ensure_concrete_impl(
        &mut self,
        ty: ParserDataType,
        location: Option<Location>,
    ) -> ParserInnerType {
        let key = ty.key();
        if self
            .impls
            .get(&key)
            .is_some_and(|imp| !imp.members.is_empty())
        {
            return key;
        }

        let template = self.find_impl_for_type(&ty).cloned();
        if let Some(template) = template {
            if template.data_type.key() != key {
                let mut new_impl = MiddleImpl {
                    data_type: ty,
                    generic_params: template.generic_params.clone(),
                    members: template.members.clone(),
                    traits: template.traits.clone(),
                    assoc_types: template.assoc_types.clone(),
                    location: location.or(template.location),
                };

                Self::populate_trait_members(&self.trait_defs, &mut new_impl);

                self.impls.insert(key.clone(), new_impl);
            }
        } else if !self.impls.contains_key(&key) {
            self.get_or_create_impl(ty, Vec::new(), location);
            if let Some(imp) = self.impls.get_mut(&key) {
                Self::populate_trait_members(&self.trait_defs, imp);
            }
        }

        key
    }

    fn populate_trait_members(trait_defs: &FxHashMap<String, MiddleTrait>, imp: &mut MiddleImpl) {
        let mut provided_members = rustc_hash::FxHashSet::default();
        for member_name in imp.members.keys() {
            provided_members.insert(member_name.clone());
        }

        for trait_name in &imp.traits.clone() {
            let default_members =
                Self::collect_trait_default_members(trait_defs, trait_name, &provided_members);
            for (member_name, _trait_member) in default_members {
                let symbol_name = format!("{}.{}", trait_name, member_name);
                imp.insert_member(member_name.clone(), symbol_name, false);
                provided_members.insert(member_name);
            }
        }
    }

    pub fn collect_trait_default_members(
        trait_defs: &FxHashMap<String, MiddleTrait>,
        root_trait: &str,
        provided: &FxHashSet<String>,
    ) -> Vec<(String, MiddleTraitMember)> {
        let mut out = Vec::new();
        let mut seen_members = FxHashSet::default();
        let mut stack = vec![root_trait.to_string()];
        let mut visited_traits = FxHashSet::default();

        while let Some(current) = stack.pop() {
            if !visited_traits.insert(current.clone()) {
                continue;
            }

            let Some(def) = trait_defs.get(&current) else {
                continue;
            };

            for implied in &def.implied_traits {
                stack.push(implied.clone());
            }

            for (name, member) in &def.members {
                if member.default.is_none()
                    || provided.contains(name)
                    || seen_members.contains(name)
                {
                    continue;
                }

                seen_members.insert(name.clone());
                out.push((name.clone(), member.clone()));
            }
        }

        out
    }

    pub fn find_object_for_struct_name(&self, struct_name: &str) -> Option<&MiddleObject> {
        if let Some(obj) = self.objects.get(struct_name) {
            return Some(obj);
        }
        None
    }

    pub fn resolve_associated_type(
        &self,
        base: &ParserDataType,
        name: &str,
    ) -> Option<ParserDataType> {
        self.find_impl_for_type(base)
            .and_then(|imp| imp.assoc_types.get(name).cloned())
    }

    pub fn get_or_create_impl(
        &mut self,
        ty: ParserDataType,
        generic_params: Vec<String>,
        location: Option<Location>,
    ) -> ParserInnerType {
        let key = ty.key();
        if self.impls.contains_key(&key) {
            return key;
        }

        self.impls.insert(
            key.clone(),
            MiddleImpl {
                data_type: ty,
                generic_params,
                members: FxHashMap::default(),
                traits: Vec::new(),
                assoc_types: FxHashMap::default(),
                location,
            },
        );

        key
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleObject {
    pub object_type: MiddleTypeDefType,
    pub variables: FxHashMap<String, (String, bool)>,
    pub traits: Vec<String>,
    pub location: Option<Location>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleImplMember {
    pub symbol_name: String,
    pub dependant: bool,
}

impl MiddleImplMember {
    pub fn new(symbol_name: String, dependant: bool) -> Self {
        Self {
            symbol_name,
            dependant,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleImpl {
    pub data_type: ParserDataType,
    pub generic_params: Vec<String>,
    pub members: FxHashMap<String, MiddleImplMember>,
    pub traits: Vec<String>,
    pub assoc_types: FxHashMap<String, ParserDataType>,
    pub location: Option<Location>,
}

impl MiddleImpl {
    pub fn insert_member(&mut self, name: String, symbol_name: String, dependant: bool) {
        self.members
            .insert(name, MiddleImplMember::new(symbol_name, dependant));
    }

    pub fn register_member_placeholder(&mut self, name: &str, symbol_name: String) {
        self.members
            .entry(name.to_string())
            .or_insert_with(|| MiddleImplMember::new(symbol_name, false));
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleTraitMember {
    pub data_type: ParserDataType,
    pub default: Option<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleTrait {
    pub implied_traits: Vec<String>,
    pub members: FxHashMap<String, MiddleTraitMember>,
    pub assoc_types: FxHashMap<String, ParserDataType>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MiddleTypeDefType {
    Enum {
        variants: Vec<(ParserText, Option<ParserDataType>)>,
        default_variant: Option<usize>,
        default_value: Option<Box<Node>>,
    },
    Struct(ObjectMap<(ParserDataType, Option<Box<Node>>)>),
    NewType(ParserDataType),
    Trait,
}

impl Display for MiddleTypeDefType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl MiddleTypeDefType {
    pub fn from_type_def_type(
        env: &mut MiddleEnvironment,
        scope: &u64,
        value: TypeDefType,
    ) -> Self {
        match value {
            TypeDefType::Enum {
                variants,
                default_variant,
                default_value,
            } => MiddleTypeDefType::Enum {
                variants: {
                    let mut lst = Vec::new();

                    for (k, v) in variants {
                        lst.push((
                            env.resolve_dollar_ident_only(scope, &k)
                                .unwrap_or_else(|| ParserText::from(k.to_string()).into()),
                            if let Some(v) = v {
                                Some(env.resolve_potential_new_type(scope, v))
                            } else {
                                None
                            },
                        ));
                    }
                    lst
                },
                default_variant,
                default_value,
            },
            TypeDefType::Struct { fields } => MiddleTypeDefType::Struct({
                let mut map = Vec::new();

                match fields {
                    ObjectType::Map(field_map) => {
                        for (k, (t, v)) in field_map {
                            let resolved_type = env.resolve_potential_new_type(scope, t);
                            map.push((k, (resolved_type, v.map(Box::new))));
                        }
                    }
                    ObjectType::Tuple(types) => {
                        for (t, v) in types {
                            let resolved_type = env.resolve_potential_new_type(scope, t);
                            map.push((format!("{}", map.len()), (resolved_type, v.map(Box::new))));
                        }
                    }
                }

                ObjectMap(map)
            }),
            TypeDefType::NewType(x) => {
                MiddleTypeDefType::NewType(env.resolve_potential_new_type(scope, *x))
            }
        }
    }
}
