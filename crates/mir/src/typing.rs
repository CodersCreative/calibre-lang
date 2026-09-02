use crate::{
    environment::MiddleEnvironment, scoping::ScopeId, symbols::resolve::ResolutionOptions,
};
use calibre_parser::{
    Location,
    ast::{
        ObjectMap, ObjectType,
        idents::ParserText,
        nodes::{AstNode, Overload, TypeDefType},
        types::{ParserDataType, ParserInnerType},
    },
};
use rustc_hash::FxHashSet;
use std::fmt::Display;
use tracing::{debug, instrument, trace};
use ustr::{Ustr, UstrMap, UstrSet};

#[derive(Debug, Clone, Default)]
pub struct Typing {
    pub objects: UstrMap<MiddleObject>,
    pub impls: UstrMap<MiddleImpl>,
    pub trait_defs: UstrMap<MiddleTrait>,
    pub generic_type_templates: UstrMap<(Vec<Ustr>, TypeDefType, Vec<Overload>)>,
}

impl Typing {
    #[instrument(skip_all, fields(ty = %ty))]
    pub fn find_impl_for_type(&self, ty: &Ustr) -> Option<&MiddleImpl> {
        trace!("finding impl for type");
        if let Some(x) = self.impls.get(ty) {
            debug!("found impl by direct key");
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
        let generic_params: Vec<Ustr> = match &ty.data_type {
            ParserInnerType::StructWithGenerics { generic_types, .. } => {
                generic_types.iter().collect()
            }
            ParserInnerType::Ptr(x) => vec![&**x],
            ParserInnerType::List(x) => vec![&**x],
            ParserInnerType::Gen(x) => vec![&**x],
            _ => Vec::new(),
        }
        .into_iter()
        .map(|x| Ustr::from(&x.impl_name()))
        .collect();

        let identifier = Ustr::from(&ty.impl_name());

        if let Some(implementation) = self.find_impl_for_type(&identifier) {
            return implementation.get_member(member, &generic_params);
        }

        // TODO Remove, its the worst case scenario
        self.impls.iter().find_map(|(name, implementation)| {
            name.contains(identifier.as_str())
                .then(|| implementation.get_member(member, &generic_params))
                .flatten()
        })
    }

    #[instrument(skip_all, fields(root_trait = %root_trait))]
    pub fn collect_trait_default_members(
        trait_defs: &UstrMap<MiddleTrait>,
        root_trait: &Ustr,
        provided: &UstrSet,
    ) -> Vec<(Ustr, MiddleTraitMember)> {
        let mut out = Vec::new();
        let mut seen_members = FxHashSet::default();
        let mut stack = vec![root_trait];
        let mut visited_traits = FxHashSet::default();

        let mut depth = 32;

        while let Some(current) = stack.pop() {
            if depth <= 0 {
                trace!("trait resolution exceeded max depth, stopping");
                break;
            }
            depth -= 1;

            if !visited_traits.insert(current) {
                continue;
            }

            let Some(def) = trait_defs.get(current) else {
                continue;
            };

            for implied in &def.implied_traits {
                stack.push(implied);
            }

            for (name, member) in &def.members {
                if member.default.is_none()
                    || provided.contains(name)
                    || seen_members.contains(name)
                {
                    continue;
                }

                seen_members.insert(name);
                out.push((*name, member.clone()));
            }
        }

        out
    }

    #[instrument(skip_all, fields(struct_name = %struct_name))]
    pub fn find_object_for_struct_name(&self, struct_name: &Ustr) -> Option<&MiddleObject> {
        trace!("finding object for struct name");
        self.objects.get(struct_name)
    }

    #[instrument(skip_all, fields(base = %base, name = %name))]
    pub fn resolve_associated_type(
        &self,
        base: &ParserDataType,
        name: &Ustr,
    ) -> Option<ParserDataType> {
        trace!("resolving associated type");

        if let ParserInnerType::Struct(trait_name) = &base.data_type
            && let Some(trait_def) = self.trait_defs.get(&Ustr::from(trait_name))
            && let Some(assoc_type) = trait_def.assoc_types.get(name)
        {
            return Some(assoc_type.clone());
        }

        if let Some(imp) = self.find_impl_for_type(&Ustr::from(&base.impl_name())) {
            if let Some(assoc_type) = imp.assoc_types.get(name) {
                return Some(assoc_type.clone());
            }

            for trait_name in imp.traits.iter() {
                if let Some(trait_def) = self.trait_defs.get(trait_name)
                    && let Some(assoc_type) = trait_def.assoc_types.get(name)
                {
                    return Some(assoc_type.clone());
                }
            }
        }
        None
    }

    #[instrument(skip_all, fields(name = %name))]
    pub fn get_or_create_impl(&mut self, name: Ustr, location: Option<Location>) {
        self.impls.entry(name).or_insert(MiddleImpl {
            members: UstrMap::default(),
            traits: Vec::new(),
            assoc_types: UstrMap::default(),
            location,
        });
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleObject {
    pub object_type: MiddleTypeDefType,
    pub variables: UstrMap<(Ustr, bool)>,
    pub traits: Vec<Ustr>,
    pub location: Option<Location>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleImplMember {
    pub symbol_name: Ustr,
    pub generic_params: Vec<Ustr>,
    pub dependant: bool,
}

impl MiddleImplMember {
    pub fn new(symbol_name: Ustr, generic_params: Vec<Ustr>, dependant: bool) -> Self {
        Self {
            symbol_name,
            generic_params,
            dependant,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleImpl {
    members: UstrMap<Vec<MiddleImplMember>>,
    pub traits: Vec<Ustr>,
    pub assoc_types: UstrMap<ParserDataType>,
    pub location: Option<Location>,
}

impl MiddleImpl {
    fn normalize_member_name(name: &impl ToString) -> Ustr {
        let name = ParserText::get_temp_name_suffix(name).unwrap_or(name.to_string());
        Ustr::from(
            &name
                .rsplit_once('.')
                .map(|x| x.1.to_string())
                .unwrap_or(name),
        )
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
        symbol_name: Ustr,
        generic_params: Vec<Ustr>,
    ) {
        let entry = self
            .members
            .entry(Self::normalize_member_name(name))
            .or_default();

        if !entry.iter_mut().any(|x| x.generic_params == generic_params) {
            entry.push(MiddleImplMember::new(symbol_name, generic_params, false));
        }
    }

    pub fn get_member(
        &self,
        name: &impl ToString,
        generic_params: &[Ustr],
    ) -> Option<&MiddleImplMember> {
        let members = self.members.get(&Self::normalize_member_name(name))?;

        if let Some(x) = members.iter().find(|x| x.generic_params == generic_params) {
            Some(x)
        } else if let Some(x) = members.iter().find(|x| x.generic_params.is_empty()) {
            Some(x)
        } else if let Some(x) = members
            .iter()
            .find(|x| x.generic_params.len() == generic_params.len())
        {
            Some(x)
        } else {
            members.first()
        }
    }

    pub fn get_all_members(&self) -> Vec<(&Ustr, &MiddleImplMember)> {
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
    pub default: Option<AstNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleTrait {
    pub implied_traits: Vec<Ustr>,
    pub members: UstrMap<MiddleTraitMember>,
    pub assoc_types: UstrMap<ParserDataType>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MiddleTypeDefType {
    Enum {
        variants: Vec<(Ustr, Option<ParserDataType>)>,
        default_variant: Option<usize>,
        default_value: Option<Box<AstNode>>,
    },
    Struct(ObjectMap<(ParserDataType, Option<Box<AstNode>>)>),
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
                    if let Some(idx) = default_variant
                        && i == *idx
                    {
                        writeln!(f, "\t@default")?;
                    }
                    write!(f, "\t{}", name)?;
                    if let Some(dt) = data_type {
                        write!(f, " : {}", dt)?;
                    }

                    if let Some(idx) = default_variant
                        && let Some(v) = default_value
                        && i == *idx
                    {
                        write!(f, " = {}", v)?;
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
        scope: ScopeId,
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
                            env.resolve(scope, &k, ResolutionOptions::default().with_dollar())
                                .unwrap_or_else(|_| Ustr::from(&k.to_string())),
                            v.map(|v| {
                                env.resolve_data_type(scope, &v, ResolutionOptions::typing())
                                    .unwrap_or(v)
                            }),
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
