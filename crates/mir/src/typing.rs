use crate::{environment::MiddleEnvironment, symbols::resolve::ResolutionOptions};
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
use std::fmt::Display;
use tracing::{debug, instrument, trace};

#[derive(Debug, Clone, Default)]
pub struct Typing {
    pub objects: FxHashMap<String, MiddleObject>,
    pub impls: FxHashMap<ParserInnerType, MiddleImpl>,
    pub trait_defs: FxHashMap<String, MiddleTrait>,
    pub generic_type_templates: FxHashMap<String, (Vec<String>, TypeDefType, Vec<Overload>)>,
    pub type_specializations: FxHashMap<String, String>,
}

impl Typing {
    #[instrument(skip_all, fields(ty = %ty))]
    pub fn find_impl_for_type(&self, ty: &ParserDataType) -> Option<&MiddleImpl> {
        trace!("finding impl for type");
        if let Some(x) = self.impls.get(&ty.key()) {
            debug!("found impl by direct key");
            Some(x)
        } else if let Some(x) = self.impls.get(&ty.clone().unwrap_all_refs().key()) {
            debug!("found impl by unwrapped refs");
            Some(x)
        } else {
            debug!("no impl found for type");
            None
        }
    }

    #[instrument(skip_all, fields(ty = %ty, member = %member.to_string()))]
    pub fn find_impl_member(
        &self,
        ty: &ParserDataType,
        member: &impl ToString,
    ) -> Option<&MiddleImplMember> {
        let generic_params: Vec<String> = match &ty.data_type {
            ParserInnerType::StructWithGenerics { generic_types, .. } => {
                generic_types.iter().collect()
            }
            ParserInnerType::Ptr(x) => vec![&**x],
            ParserInnerType::List(x) => vec![&**x],
            _ => Vec::new(),
        }
        .into_iter()
        .map(|x| x.impl_name())
        .collect();

        if let Some(implementation) = self.find_impl_for_type(ty) {
            return implementation.get_member(member, &generic_params);
        }

        let identifier = ty.impl_name();

        if let Some(implementation) = self.find_impl_for_type(&ParserDataType {
            data_type: ParserInnerType::Struct(identifier.clone()),
            span: ty.span,
        }) {
            return implementation.get_member(member, &generic_params);
        }

        // TODO Remove, its the worst case scenario
        self.impls.values().find_map(|implementation| {
            let name = &implementation.data_type.impl_name();
            name.contains(&identifier)
                .then(|| implementation.get_member(member, &generic_params))
                .flatten()
        })
    }

    #[instrument(skip_all, fields(root_trait = %root_trait))]
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

    #[instrument(skip_all, fields(struct_name = %struct_name))]
    pub fn find_object_for_struct_name(&self, struct_name: &str) -> Option<&MiddleObject> {
        trace!("finding object for struct name");
        self.objects.get(struct_name)
    }

    #[instrument(skip_all, fields(base = %base, name = %name))]
    pub fn resolve_associated_type(
        &self,
        base: &ParserDataType,
        name: &str,
    ) -> Option<ParserDataType> {
        trace!("resolving associated type");
        self.find_impl_for_type(base)
            .and_then(|imp| imp.assoc_types.get(name).cloned())
    }

    #[instrument(skip_all, fields(ty = %ty))]
    pub fn get_or_create_impl(
        &mut self,
        ty: ParserDataType,
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
    pub generic_params: Vec<String>,
    pub dependant: bool,
}

impl MiddleImplMember {
    pub fn new(symbol_name: String, generic_params: Vec<String>, dependant: bool) -> Self {
        Self {
            symbol_name,
            generic_params,
            dependant,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleImpl {
    pub data_type: ParserDataType,
    members: FxHashMap<String, Vec<MiddleImplMember>>,
    pub traits: Vec<String>,
    pub assoc_types: FxHashMap<String, ParserDataType>,
    pub location: Option<Location>,
}

impl MiddleImpl {
    fn normalize_member_name(name: &impl ToString) -> String {
        let name = ParserText::get_temp_name_suffix(name).unwrap_or(name.to_string());
        name.rsplit_once('.')
            .map(|x| x.1.to_string())
            .unwrap_or(name)
    }

    pub fn insert_member(&mut self, name: &impl ToString, member: MiddleImplMember) {
        let entry = self
            .members
            .entry(Self::normalize_member_name(name))
            .or_default();

        if let Some(x) = entry
            .iter_mut()
            .find(|x| x.generic_params == member.generic_params)
        {
            *x = member;
        } else {
            entry.push(member);
        }
    }

    pub fn insert_member_placeholder(
        &mut self,
        name: &impl ToString,
        symbol_name: String,
        generic_params: Vec<String>,
    ) {
        let entry = self
            .members
            .entry(Self::normalize_member_name(name))
            .or_default();

        if entry
            .iter_mut()
            .find(|x| x.generic_params == generic_params)
            .is_none()
        {
            entry.push(MiddleImplMember::new(symbol_name, generic_params, false));
        }
    }

    pub fn get_member(
        &self,
        name: &impl ToString,
        generic_params: &[String],
    ) -> Option<&MiddleImplMember> {
        let members = self.members.get(&Self::normalize_member_name(name))?;

        if let Some(x) = members.iter().find(|x| x.generic_params == generic_params) {
            Some(x)
        } else if let Some(x) = members.iter().find(|x| x.generic_params.is_empty()) {
            Some(x)
        } else {
            members
                .iter()
                .find(|x| x.generic_params.len() == generic_params.len())
        }
    }

    pub fn get_all_members(&self) -> Vec<(&String, &MiddleImplMember)> {
        let mut members = Vec::new();

        for (name, member) in &self.members {
            for value in member {
                members.push((name, value));
            }
        }

        members
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
        match self {
            MiddleTypeDefType::Enum {
                variants,
                default_variant,
                default_value,
            } => {
                writeln!(f, "enum {{")?;
                for (i, (name, data_type)) in variants.iter().enumerate() {
                    if let Some(idx) = default_variant {
                        if i == *idx {
                            writeln!(f, "\t@default")?;
                        }
                    }
                    write!(f, "\t{}", name.text)?;
                    if let Some(dt) = data_type {
                        write!(f, " : {}", dt)?;
                    }

                    if let Some(idx) = default_variant
                        && i == *idx
                        && default_value.is_some()
                    {
                        write!(f, " = {:?}", default_value)?;
                    }
                    writeln!(f, ",")?;
                }
                write!(f, "}}")
            }
            MiddleTypeDefType::Struct(fields) => {
                let is_tuple = fields
                    .0
                    .iter()
                    .all(|(name, _)| name.chars().all(|c| c.is_ascii_digit()));

                if fields.0.is_empty() {
                    write!(f, "struct {{}}")
                } else if is_tuple {
                    let types: Vec<String> = fields
                        .0
                        .iter()
                        .map(|(_, (data_type, default_val))| {
                            if let Some(val) = default_val {
                                format!("{} = {:?}", data_type, val)
                            } else {
                                format!("{}", data_type)
                            }
                        })
                        .collect();
                    write!(f, "({})", types.join(", "))
                } else {
                    writeln!(f, "struct {{")?;
                    for (name, (data_type, default_val)) in fields.0.iter() {
                        write!(f, "\t{} : {}", name, data_type)?;
                        if let Some(val) = default_val {
                            write!(f, " = {:?}", val)?;
                        }
                        writeln!(f, ",")?;
                    }
                    write!(f, "}}")
                }
            }
            MiddleTypeDefType::NewType(data_type) => {
                write!(f, "type {}", data_type)
            }
            MiddleTypeDefType::Trait => {
                write!(f, "trait")
            }
        }
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
                            ParserText::from(
                                env.resolve(scope, &k, ResolutionOptions::default().with_dollar())
                                    .unwrap_or_else(|_| k.to_string()),
                            ),
                            if let Some(v) = v {
                                Some(
                                    env.resolve_data_type(scope, &v, ResolutionOptions::typing())
                                        .unwrap_or(v),
                                )
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
                            let resolved_type = env
                                .resolve_data_type(scope, &t, ResolutionOptions::typing())
                                .unwrap_or(t);
                            map.push((k, (resolved_type, v.map(Box::new))));
                        }
                    }
                    ObjectType::Tuple(types) => {
                        for (t, v) in types {
                            let resolved_type = env
                                .resolve_data_type(scope, &t, ResolutionOptions::typing())
                                .unwrap_or(t);
                            map.push((format!("{}", map.len()), (resolved_type, v.map(Box::new))));
                        }
                    }
                }

                ObjectMap(map)
            }),
            TypeDefType::NewType(x) => MiddleTypeDefType::NewType(
                env.resolve_data_type(scope, x.as_ref(), ResolutionOptions::typing())
                    .unwrap_or(*x),
            ),
        }
    }
}
