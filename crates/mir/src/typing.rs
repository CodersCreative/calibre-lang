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
use rustc_hash::FxHashMap;

#[derive(Debug, Clone, Default)]
pub struct Typing {
    pub objects: FxHashMap<String, MiddleObject>,
    pub impls: FxHashMap<ParserInnerType, MiddleImpl>,
    pub type_aliases: FxHashMap<String, ParserDataType>,
    pub trait_defs: FxHashMap<String, MiddleTrait>,
    pub generic_type_templates: FxHashMap<String, (Vec<String>, TypeDefType, Vec<Overload>)>,
    pub type_specializations: FxHashMap<String, String>,
}

impl Typing {
    pub fn member_fn_candidates(&self, ty: &ParserDataType, member: &str) -> Vec<String> {
        let mut candidates = Vec::new();

        if let Some(imp) = self.find_impl_for_type(ty)
            && let Some((mapped_name, _)) = imp.variables.get(member)
        {
            candidates.push(mapped_name.clone());
        }

        for base in ty.member_base_name_candidates() {
            candidates.push(format!("{base}::{member}"));
        }
        candidates.dedup();
        candidates
    }

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

    pub fn find_object_for_struct_name(&self, struct_name: &str) -> Option<&MiddleObject> {
        if let Some(obj) = self.objects.get(struct_name) {
            return Some(obj);
        }
        let base = calibre_parser::qualified_name_base(struct_name);
        if base != struct_name {
            return self.objects.get(base);
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
                variables: FxHashMap::default(),
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
pub struct MiddleImpl {
    pub data_type: ParserDataType,
    pub generic_params: Vec<String>,
    pub variables: FxHashMap<String, (String, bool)>,
    pub traits: Vec<String>,
    pub assoc_types: FxHashMap<String, ParserDataType>,
    pub location: Option<Location>,
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
