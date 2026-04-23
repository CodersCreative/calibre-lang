use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    ast::hm::{self, Subst, Type, TypeEnv, TypeGenerator, TypeScheme},
    environment::{MiddleEnvironment, MiddleVariable},
    infer,
};
use calibre_parser::{
    Span,
    ast::{
        Node, ParserDataType, ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier,
    },
};

pub struct TypingContext {
    pub store: TypingStore,
    pub subst: Subst,
}

#[derive(Debug, Clone, Default)]
pub struct TypingStore {
    pub hm_env: TypeEnv,
    pub type_cache: FxHashMap<Type, ParserDataType>,
    pub active_node_inference: FxHashSet<(u64, usize)>,
}

impl TypingStore {
    pub fn contains_scheme(&self, name: &str) -> bool {
        self.hm_env.contains_key(name)
    }

    pub fn scheme(&self, name: &str) -> Option<&TypeScheme> {
        self.hm_env.get(name)
    }

    pub fn scheme_cloned(&self, name: &str) -> Option<TypeScheme> {
        self.hm_env.get(name).cloned()
    }

    pub fn find_scheme_by_tail(&self, tail: &str) -> Option<TypeScheme> {
        self.hm_env
            .iter()
            .find(|(k, _)| calibre_parser::qualified_name_tail(k) == tail)
            .map(|(_, v)| v.clone())
    }

    pub fn insert_scheme(&mut self, name: String, scheme: TypeScheme) {
        self.hm_env.insert(name, scheme);
    }

    pub fn parser_type(&mut self, ty: &Type) -> ParserDataType {
        hm::to_parser_data_type(ty, &mut self.type_cache)
    }

    pub fn parser_type_with_span(&mut self, ty: &Type, span: Span) -> ParserDataType {
        hm::to_parser_data_type_with_span(ty, &mut self.type_cache, span)
    }

    pub fn generalize(&self, ty: &Type) -> TypeScheme {
        hm::generalize(&self.hm_env, ty)
    }

    pub fn apply_subst(&mut self, subst: &Subst) {
        hm::apply_subst_env(&mut self.hm_env, subst);
    }

    pub fn begin_node_inference(&mut self, scope: u64, node: &Node) -> bool {
        self.active_node_inference
            .insert((scope, (node as *const Node) as usize))
    }

    pub fn end_node_inference(&mut self, scope: u64, node: &Node) {
        self.active_node_inference
            .remove(&(scope, (node as *const Node) as usize));
    }

    pub fn is_node_inference_active(&self, scope: u64, node: &Node) -> bool {
        self.active_node_inference
            .contains(&(scope, (node as *const Node) as usize))
    }
}

impl TypingContext {
    pub fn from_middle(env: &MiddleEnvironment) -> Self {
        Self {
            store: env.typing.clone(),
            subst: Subst::default(),
        }
    }

    pub fn ensure_variable_schemes(&mut self, variables: &FxHashMap<String, MiddleVariable>) {
        let mut tg = hm::TypeGenerator::default();
        for (name, var) in variables {
            if self.store.contains_scheme(name) {
                continue;
            }
            let ty = hm::try_from_parser_data_type(&var.data_type, &mut tg)
                .unwrap_or_else(|_| hm::from_parser_data_type(&var.data_type, &mut tg));
            self.store
                .insert_scheme(name.clone(), hm::generalize(&self.store.hm_env, &ty));
        }
    }

    pub fn parser_type(&mut self, ty: &Type, span: Span) -> ParserDataType {
        self.store.parser_type_with_span(ty, span)
    }

    pub fn apply_subst_to_env(&mut self) {
        self.store.apply_subst(&self.subst);
    }

    pub fn insert_scheme(&mut self, name: String, scheme: TypeScheme) {
        self.store.insert_scheme(name, scheme);
    }

    pub fn scheme(&self, name: &str) -> Option<&TypeScheme> {
        self.store.scheme(name)
    }

    pub fn commit_to_middle(self, env: &mut MiddleEnvironment) {
        env.typing = self.store;
    }
}

pub trait TypeResolver {
    fn hm_env(&self) -> &TypeEnv;
    fn objects_contains(&self, name: &str) -> bool;
    fn member_fn_candidates(&self, ty: &ParserDataType, member: &str) -> Vec<String>;
    fn resolve_data_type_for_typing(
        &mut self,
        scope: &u64,
        data_type: ParserDataType,
    ) -> ParserDataType;
    fn resolve_member_field_type_for_typing(
        &mut self,
        scope: &u64,
        base: &ParserDataType,
        member: &str,
        span: Span,
    ) -> Option<ParserDataType>;
    fn resolve_potential_generic_ident_for_typing(
        &self,
        scope: &u64,
        ident: &PotentialGenericTypeIdentifier,
    ) -> Option<ParserText>;
    fn resolve_potential_generic_ident_to_data_type_for_typing(
        &mut self,
        scope: &u64,
        ident: &PotentialGenericTypeIdentifier,
    ) -> Option<ParserDataType>;
    fn resolve_potential_dollar_ident_for_typing(
        &self,
        scope: &u64,
        ident: &PotentialDollarIdentifier,
    ) -> Option<ParserText>;
    fn overloads_for_typing(&self) -> &[crate::environment::MiddleOverload];
}

impl TypeResolver for MiddleEnvironment {
    fn hm_env(&self) -> &TypeEnv {
        &self.typing.hm_env
    }

    fn objects_contains(&self, name: &str) -> bool {
        self.objects.contains_key(name)
    }

    fn member_fn_candidates(&self, ty: &ParserDataType, member: &str) -> Vec<String> {
        MiddleEnvironment::member_fn_candidates(self, ty, member)
    }

    fn resolve_data_type_for_typing(
        &mut self,
        scope: &u64,
        data_type: ParserDataType,
    ) -> ParserDataType {
        self.resolve_data_type(scope, data_type)
    }

    fn resolve_member_field_type_for_typing(
        &mut self,
        scope: &u64,
        base: &ParserDataType,
        member: &str,
        span: Span,
    ) -> Option<ParserDataType> {
        self.resolve_member_field_type(scope, base, member, span)
    }

    fn resolve_potential_generic_ident_for_typing(
        &self,
        scope: &u64,
        ident: &PotentialGenericTypeIdentifier,
    ) -> Option<ParserText> {
        self.resolve_potential_generic_ident(scope, ident)
    }

    fn resolve_potential_generic_ident_to_data_type_for_typing(
        &mut self,
        scope: &u64,
        ident: &PotentialGenericTypeIdentifier,
    ) -> Option<ParserDataType> {
        self.resolve_potential_generic_ident_to_data_type(scope, ident)
    }

    fn resolve_potential_dollar_ident_for_typing(
        &self,
        scope: &u64,
        ident: &PotentialDollarIdentifier,
    ) -> Option<ParserText> {
        self.resolve_potential_dollar_ident(scope, ident)
    }

    fn overloads_for_typing(&self) -> &[crate::environment::MiddleOverload] {
        &self.overloads
    }
}

impl MiddleEnvironment {
    pub fn infer_parser_type_for_node(
        &mut self,
        scope: &u64,
        node: &Node,
    ) -> Option<ParserDataType> {
        if self.typing.is_node_inference_active(*scope, node) {
            return None;
        }
        let started = self.typing.begin_node_inference(*scope, node);
        if !started {
            return None;
        }
        let out = infer_parser_type(self, scope, node);
        self.typing.end_node_inference(*scope, node);
        out
    }

    pub fn infer_hm_type_for_node(&mut self, scope: &u64, node: &Node) -> Option<(Type, Subst)> {
        if self.typing.is_node_inference_active(*scope, node) {
            return None;
        }
        let started = self.typing.begin_node_inference(*scope, node);
        if !started {
            return None;
        }
        let out = infer_hm_type(self, scope, node);
        self.typing.end_node_inference(*scope, node);
        out
    }
}

pub fn infer_parser_type(
    env: &mut MiddleEnvironment,
    scope: &u64,
    node: &Node,
) -> Option<ParserDataType> {
    let mut typing = TypingContext::from_middle(env);
    typing.ensure_variable_schemes(&env.variables);
    let mut tg = TypeGenerator::default();

    match infer::visit_internal(
        node,
        env,
        scope,
        &mut tg,
        &mut typing.store.hm_env,
        &mut typing.subst,
    ) {
        Ok(t) => Some(typing.parser_type(&hm::apply_subst(&typing.subst, &t), node.span)),
        Err(_) => None,
    }
}

pub fn infer_hm_type(
    env: &mut MiddleEnvironment,
    scope: &u64,
    node: &Node,
) -> Option<(Type, Subst)> {
    let mut typing = TypingContext::from_middle(env);
    typing.ensure_variable_schemes(&env.variables);
    let mut tg = TypeGenerator::default();

    let mut var_types: FxHashMap<String, Type> = FxHashMap::default();
    for (k, v) in env.variables.iter() {
        if !typing.store.contains_scheme(k) {
            if v.data_type.contains_auto() {
                let t = tg.fresh();
                var_types.insert(k.clone(), t.clone());
                typing
                    .store
                    .insert_scheme(k.clone(), TypeScheme::new(Vec::new(), t));
            } else {
                let t = hm::try_from_parser_data_type(&v.data_type, &mut tg)
                    .unwrap_or_else(|_| hm::from_parser_data_type(&v.data_type, &mut tg));
                var_types.insert(k.clone(), t.clone());
                typing
                    .store
                    .insert_scheme(k.clone(), hm::generalize(&FxHashMap::default(), &t));
            }
        }
    }

    let mut tenv = typing.store.hm_env.clone();
    match infer::visit_internal(node, env, scope, &mut tg, &mut tenv, &mut typing.subst) {
        Ok(t) => {
            typing.store.hm_env = tenv;
            typing.apply_subst_to_env();

            for (k, v) in env.variables.iter_mut() {
                if v.data_type.contains_auto()
                    && let Some(scheme) = typing.store.scheme(k)
                {
                    let ty = scheme.ty.clone();
                    v.data_type = typing.parser_type(&ty, node.span);
                }
            }

            for (k, orig_t) in &var_types {
                let applied = hm::apply_subst(&typing.subst, orig_t);
                if let Some(var) = env.variables.get_mut(k)
                    && var.data_type.contains_auto()
                {
                    var.data_type = typing.parser_type(&applied, node.span);
                }
                typing.insert_scheme(k.clone(), TypeScheme::new(Vec::new(), applied));
            }

            hm::normalize_env(&mut typing.store.hm_env);
            let inferred = hm::apply_subst(&typing.subst, &t);
            let subst = typing.subst.clone();
            typing.commit_to_middle(env);
            Some((inferred, subst))
        }
        Err(_) => None,
    }
}
