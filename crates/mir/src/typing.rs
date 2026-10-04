use crate::{
    ast::types::{MirDataType, unify::TypeImplKey},
    environment::MiddleEnvironment,
    scoping::ScopeId,
    symbols::{TypeKey, VariableKey, resolve::ResolutionOptions},
};
use calibre_parser::{
    Location,
    ast::{
        ObjectMap, ObjectType,
        nodes::{AstNode, types::TypeDefType},
    },
};
use rustc_hash::{FxHashMap, FxHashSet};
use serde::{Deserialize, Serialize};
use std::fmt::Display;
use tracing::{instrument, trace};
use ustr::{Ustr, UstrMap, UstrSet};

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Typing {
    pub objects: FxHashMap<TypeKey, MiddleObject>,
    pub inherent_impls: FxHashMap<TypeImplKey, Vec<MiddleImpl>>,
    pub generic_type_templates: UstrMap<(Vec<Ustr>, TypeDefType)>,
}

impl Typing {
    pub fn all_impls(&self) -> impl Iterator<Item = &MiddleImpl> {
        self.inherent_impls.values().flatten()
    }

    pub fn add_inherent_impl(&mut self, imp: MiddleImpl) {
        let key = TypeImplKey::from(&imp.target);
        self.inherent_impls.entry(key).or_default().push(imp);
    }

    #[instrument(skip_all, fields(ty = %ty))]
    pub fn find_inherent_impl_for_type(&self, ty: &MirDataType) -> Option<&MiddleImpl> {
        trace!("finding inherent impl for type");

        let key = TypeImplKey::from(ty);
        let candidates = self.inherent_impls.get(&key)?;
        let mut best: Option<(usize, &MiddleImpl)> = None;
        let mut bindings = FxHashMap::default();

        for imp in candidates {
            bindings.clear();
            if imp.target.can_unify(ty, &imp.generic_params, &mut bindings) {
                let score = imp.target.specificity(&imp.generic_params);
                if best
                    .as_ref()
                    .is_none_or(|(best_score, _)| score > *best_score)
                {
                    best = Some((score, imp));
                }
            }
        }
        best.map(|(_, imp)| imp)
    }

    pub fn find_impl_for_type(&self, ty: &TypeKey) -> Option<&MiddleImpl> {
        let target = MirDataType::object(ty.clone());
        self.find_inherent_impl_for_type(&target)
    }

    #[instrument(skip_all, fields(ty = %ty, member = %member.as_ref()))]
    pub fn find_impl_member(
        &self,
        ty: &MirDataType,
        member: impl AsRef<str>,
    ) -> Option<&MiddleImplMember> {
        let member_name = MiddleImpl::normalize_member_name(member);
        let key = TypeImplKey::from(ty);

        let mut best: Option<(usize, &MiddleImplMember)> = None;
        let mut bindings = FxHashMap::default();

        if let Some(candidates) = self.inherent_impls.get(&key) {
            for imp in candidates {
                bindings.clear();
                if (imp.target.can_unify(ty, &imp.generic_params, &mut bindings) || best.is_none())
                    && let Some(m) = imp.get_member(member_name, &[])
                {
                    let score = imp.target.specificity(&imp.generic_params);
                    if best
                        .as_ref()
                        .is_none_or(|(best_score, _)| score > *best_score)
                    {
                        best = Some((score, m));
                    }
                }
            }

            if best.is_some() {
                return best.map(|(_, m)| m);
            }
        }

        best.map(|(_, m)| m)
    }

    pub fn find_impl_member_with_subst(
        &self,
        ty: &MirDataType,
        member: impl AsRef<str>,
    ) -> Option<(&MiddleImplMember, FxHashMap<String, MirDataType>)> {
        let member_name = MiddleImpl::normalize_member_name(member);
        let key = TypeImplKey::from(ty);

        let mut best: Option<(usize, &MiddleImplMember, FxHashMap<String, MirDataType>)> = None;
        let mut bindings = FxHashMap::default();

        if let Some(candidates) = self.inherent_impls.get(&key) {
            for imp in candidates {
                bindings.clear();
                if imp.target.can_unify(ty, &imp.generic_params, &mut bindings)
                    && let Some(m) = imp.get_member(member_name, &[])
                {
                    let score = imp.target.specificity(&imp.generic_params);
                    if best
                        .as_ref()
                        .is_none_or(|(best_score, _, _)| score > *best_score)
                    {
                        best = Some((score, m, bindings.clone()));
                    }
                }
            }
            if let Some((_, m, b)) = best {
                return Some((m, b));
            }
        }

        best.map(|(_, m, b)| (m, b))
    }

    #[instrument(skip_all, fields(root_trait = %root_trait))]
    pub fn collect_trait_default_members(
        trait_defs: &FxHashMap<TypeKey, MiddleTrait>,
        root_trait: &TypeKey,
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
                    || !seen_members.insert(*name)
                {
                    continue;
                }

                out.push((*name, member.clone()));
            }
        }

        out
    }

    #[instrument(skip_all, fields(struct_name = %struct_name))]
    pub fn find_object_for_struct_name(&self, struct_name: &TypeKey) -> Option<&MiddleObject> {
        trace!("finding object for struct name");
        self.objects.get(struct_name)
    }

    #[instrument(skip_all, fields(name = %name))]
    pub fn get_or_create_impl(&mut self, name: TypeKey, location: Option<Location>) {
        let target = MirDataType::object(name);
        let key = TypeImplKey::from(&target);
        let list = self.inherent_impls.entry(key).or_default();
        if !list.iter().any(|i| i.target == target) {
            list.push(MiddleImpl::new_inherent(target, Vec::new(), location));
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MiddleObject {
    pub object_type: MiddleTypeDefType,
    pub variables: UstrMap<(VariableKey, bool)>,
    pub traits: Vec<TypeKey>,
    pub location: Option<Location>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MiddleImplMember {
    pub symbol_name: VariableKey,
    pub generic_params: Vec<Ustr>,
    pub dependant: bool,
}

impl MiddleImplMember {
    pub fn new(symbol_name: VariableKey, generic_params: Vec<Ustr>, dependant: bool) -> Self {
        Self {
            symbol_name,
            generic_params,
            dependant,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MiddleImpl {
    pub target: MirDataType,
    pub generic_params: Vec<Ustr>,
    pub trait_key: Option<TypeKey>,
    pub traits: Vec<TypeKey>,
    pub members: UstrMap<Vec<MiddleImplMember>>,
    pub assoc_types: UstrMap<MirDataType>,
    pub location: Option<Location>,
}

impl MiddleImpl {
    pub fn new_inherent(
        target: MirDataType,
        generic_params: Vec<Ustr>,
        location: Option<Location>,
    ) -> Self {
        Self {
            target,
            generic_params,
            trait_key: None,
            traits: Vec::new(),
            members: UstrMap::default(),
            assoc_types: UstrMap::default(),
            location,
        }
    }

    pub fn new_trait(
        trait_key: TypeKey,
        target: MirDataType,
        generic_params: Vec<Ustr>,
        location: Option<Location>,
    ) -> Self {
        Self {
            target,
            generic_params,
            trait_key: Some(trait_key.clone()),
            traits: vec![trait_key],
            members: UstrMap::default(),
            assoc_types: UstrMap::default(),
            location,
        }
    }

    #[inline]
    pub fn normalize_member_name(name: impl AsRef<str>) -> Ustr {
        let name_str = name.as_ref();
        let short_name = name_str
            .rsplit_once("::")
            .map(|x| x.1)
            .or_else(|| name_str.rsplit_once('.').map(|x| x.1))
            .unwrap_or(name_str);
        Ustr::from(short_name)
    }

    pub fn insert_member(&mut self, name: impl AsRef<str>, member: MiddleImplMember) {
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
        name: impl AsRef<str>,
        symbol_name: VariableKey,
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
        name: impl AsRef<str>,
        generic_params: &[Ustr],
    ) -> Option<&MiddleImplMember> {
        let members = self.members.get(&Self::normalize_member_name(name))?;

        if !generic_params.is_empty()
            && let Some(x) = members.iter().find(|x| x.generic_params == generic_params)
        {
            return Some(x);
        }

        members.first()
    }

    pub fn get_all_members(&self) -> Vec<(&Ustr, &MiddleImplMember)> {
        let total_count: usize = self.members.values().map(|v| v.len()).sum();
        let mut members = Vec::with_capacity(total_count);

        for (name, member_list) in &self.members {
            for value in member_list {
                members.push((name, value));
            }
        }

        members
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MiddleTraitMember {
    pub data_type: MirDataType,
    pub default: Option<AstNode>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MiddleTrait {
    pub implied_traits: Vec<TypeKey>,
    pub members: UstrMap<MiddleTraitMember>,
    pub type_members: UstrMap<MirDataType>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum MiddleTypeDefType {
    Enum {
        variants: Vec<(Ustr, Option<MirDataType>)>,
        default_variant: Option<usize>,
        default_value: Option<Box<AstNode>>,
    },
    Struct(ObjectMap<(MirDataType, Option<Box<AstNode>>)>),
    NewType(MirDataType),
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
                    write!(f, "(")?;
                    for (i, (_, (data_type, default_val))) in fields.0.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", data_type)?;
                        if let Some(val) = default_val {
                            write!(f, " = {:?}", val)?;
                        }
                    }
                    write!(f, ")")
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
                    let mut lst = Vec::with_capacity(variants.len());

                    for (_, k, v) in variants {
                        let name = match env.resolve(
                            scope,
                            &k,
                            ResolutionOptions::default().with_dollar(),
                        ) {
                            Ok(key) => key.unwrap_dollar(),
                            Err(_) => Ustr::from(k.text()),
                        };
                        let ty = v.map(|v| {
                            env.resolve_data_type(scope, &v, ResolutionOptions::typing())
                                .unwrap_or_else(|_| MirDataType::from(v))
                        });
                        lst.push((name, ty));
                    }
                    lst
                },
                default_variant,
                default_value,
            },
            TypeDefType::Struct { fields } => MiddleTypeDefType::Struct({
                match fields {
                    ObjectType::Map(field_map) => {
                        let mut map = Vec::with_capacity(field_map.len());
                        for (k, (_, t, v)) in field_map {
                            let resolved_type = env
                                .resolve_data_type(scope, &t, ResolutionOptions::typing())
                                .unwrap_or_else(|_| MirDataType::from(t));
                            map.push((k, (resolved_type, v.map(Box::new))));
                        }
                        ObjectMap(map)
                    }
                    ObjectType::Tuple(types) => {
                        let mut map = Vec::with_capacity(types.len());
                        for (_, t, v) in types {
                            let resolved_type = env
                                .resolve_data_type(scope, &t, ResolutionOptions::typing())
                                .unwrap_or_else(|_| MirDataType::from(t));
                            let idx_str = map.len().to_string();
                            map.push((Ustr::from(&idx_str), (resolved_type, v.map(Box::new))));
                        }
                        ObjectMap(map)
                    }
                }
            }),
            TypeDefType::NewType(x) => MiddleTypeDefType::NewType(
                env.resolve_data_type(scope, x.as_ref(), ResolutionOptions::typing())
                    .unwrap_or_else(|_| MirDataType::from(*x)),
            ),
        }
    }
}
