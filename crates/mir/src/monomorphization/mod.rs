use crate::{
    MirRenamable, MirRenameState,
    ast::types::MirDataType,
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::{FullyQualifiedPath, ScopeId},
    symbols::{MiddleOverload, TypeKey, VariableKey, resolve::ResolutionOptions},
    translate::MirLowering,
};
use calibre_parser::ast::{
    Substitutable,
    idents::{PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
    nodes::{
        AstNode, AstNodeType, VarType,
        declaration::AstDeclaration,
        functions::{AstFunction, FunctionHeader},
        types::{AstType, TypeDefType},
    },
    types::ParserDataType,
};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use ustr::{Ustr, UstrSet};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeTemplate {
    pub generic_params: Vec<Ustr>,
    pub type_def: TypeDefType,
    pub impls: Vec<AstNode>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Monomorphizer {
    pub generic_type_templates: FxHashMap<MirDataType, TypeTemplate>,
    pub generic_fn_templates: FxHashMap<VariableKey, (Vec<Ustr>, FunctionHeader, AstNode)>,

    pub functions: FxHashMap<(VariableKey, Vec<MirDataType>), VariableKey>,
    pub types: FxHashMap<(MirDataType, Vec<MirDataType>), MirDataType>,

    #[serde(skip)]
    pub awaiting_lowering: Vec<(AstNode, ScopeId)>,
}

impl Monomorphizer {
    pub fn function_template_key(&self, key: &VariableKey) -> Option<&VariableKey> {
        self.generic_fn_templates
            .get_key_value(key)
            .map(|(key, _)| key)
            .or_else(|| {
                self.generic_fn_templates
                    .keys()
                    .find(|candidate| candidate.to_string() == key.to_string())
            })
    }

    pub fn function_template(
        &self,
        key: &VariableKey,
    ) -> Option<&(Vec<Ustr>, FunctionHeader, AstNode)> {
        let template_key = self.function_template_key(key)?;
        self.generic_fn_templates.get(template_key)
    }

    pub fn rename(&mut self, state: &mut MirRenameState) {
        let generic_fn_templates = std::mem::take(&mut self.generic_fn_templates);
        for (mut key, template) in generic_fn_templates {
            key.rename(state);
            self.generic_fn_templates.entry(key).or_insert(template);
        }

        let generic_type_templates = std::mem::take(&mut self.generic_type_templates);
        for (mut key, template) in generic_type_templates {
            key.rename(state);
            self.generic_type_templates.entry(key).or_insert(template);
        }

        let functions = std::mem::take(&mut self.functions);
        for ((mut key, mut args), mut value) in functions {
            key.rename(state);
            for arg in &mut args {
                arg.rename(state);
            }
            value.rename(state);
            self.functions.entry((key, args)).or_insert(value);
        }

        let types = std::mem::take(&mut self.types);
        for ((mut key, mut args), mut value) in types {
            key.rename(state);
            for arg in &mut args {
                arg.rename(state);
            }
            value.rename(state);
            self.types.entry((key, args)).or_insert(value);
        }
    }

    pub fn append(&mut self, other: Self) {
        for (key, value) in other.generic_fn_templates {
            self.generic_fn_templates.entry(key).or_insert(value);
        }

        for (key, value) in other.generic_type_templates {
            self.generic_type_templates.entry(key).or_insert(value);
        }

        for (key, value) in other.functions {
            self.functions.entry(key).or_insert(value);
        }

        for (key, value) in other.types {
            self.types.entry(key).or_insert(value);
        }

        self.awaiting_lowering.extend(other.awaiting_lowering);
    }
}

fn mangle_name(base: &str, concrete_args: &[MirDataType]) -> String {
    format!(
        "{}_{}",
        base,
        concrete_args
            .iter()
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join("_")
    )
}

fn is_function_impl_member(node: &AstNode) -> bool {
    match &node.node_type {
        AstNodeType::VariableDeclaration(AstDeclaration { value, .. }) => value.is_callable(),
        AstNodeType::Tag(tag) => is_function_impl_member(&tag.node),
        _ => false,
    }
}

impl MiddleEnvironment {
    pub fn monomorphize_overload(
        &mut self,
        scope: ScopeId,
        overload: &MiddleOverload,
        arg_types: Vec<MirDataType>,
        expected_return: Option<&MirDataType>,
    ) -> Result<VariableKey, MiddleErr> {
        let concrete_args = self.infer_concrete_type_args(
            scope,
            &overload.generic_params,
            &overload.parameters,
            &overload.return_type,
            &arg_types,
            expected_return,
        )?;

        let key = self.monomorphize_function(scope, overload.func.clone(), concrete_args)?;
        Ok(key)
    }

    pub fn infer_concrete_type_args(
        &mut self,
        scope: ScopeId,
        generic_params: &[Ustr],
        param_types: &[MirDataType],
        return_type: &MirDataType,
        arg_types: &[MirDataType],
        expected_return: Option<&MirDataType>,
    ) -> Result<Vec<MirDataType>, MiddleErr> {
        let mut subst = FxHashMap::default();

        for (param, arg) in param_types.iter().zip(arg_types.iter()) {
            self.unify_types(scope, param, arg, generic_params, &mut subst)?;
        }

        if let Some(expected) = expected_return {
            self.unify_types(scope, return_type, expected, generic_params, &mut subst)?;
        }

        Ok(generic_params
            .iter()
            .map(|param| {
                if let Some(ty) = subst.get(param.as_str()) {
                    ty.clone()
                } else {
                    MirDataType::Struct {
                        identifier: TypeKey {
                            fully_qualified_path: FullyQualifiedPath::combine(None, *param),
                        },
                        generic_types: Vec::new(),
                    }
                }
            })
            .collect())
    }

    fn unify_types(
        &mut self,
        _scope: ScopeId,
        pattern: &MirDataType,
        concrete: &MirDataType,
        generic_params: &[Ustr],
        subst: &mut FxHashMap<String, MirDataType>,
    ) -> Result<(), MiddleErr> {
        let generic_param_set: UstrSet = generic_params.iter().cloned().collect();

        match pattern {
            MirDataType::Struct {
                identifier,
                generic_types,
            } => {
                if generic_types.is_empty() {
                    let name = identifier.fully_qualified_path.name.unwrap_or_default();

                    if generic_param_set.contains(&name) {
                        if let Some(existing) = subst.get(name.as_str()) {
                            if existing != concrete {
                                return Err(MiddleErr::Object(format!(
                                    "Type parameter {} has conflicting types: {} and {}",
                                    name, existing, concrete
                                )));
                            }
                        } else {
                            subst.insert(name.to_string(), concrete.clone());
                        }
                        return Ok(());
                    }
                }

                if let MirDataType::Struct {
                    identifier: concrete_id,
                    generic_types: concrete_gens,
                } = concrete
                    && identifier.fully_qualified_path.name == concrete_id.fully_qualified_path.name
                {
                    for (g1, g2) in generic_types.iter().zip(concrete_gens.iter()) {
                        self.unify_types(_scope, g1, g2, generic_params, subst)?;
                    }
                }
            }
            MirDataType::List(inner) => {
                if let MirDataType::List(concrete_inner) = concrete {
                    self.unify_types(_scope, inner, concrete_inner, generic_params, subst)?;
                }
            }
            MirDataType::Tuple(items) => {
                if let MirDataType::Tuple(concrete_items) = concrete {
                    for (i1, i2) in items.iter().zip(concrete_items.iter()) {
                        self.unify_types(_scope, i1, i2, generic_params, subst)?;
                    }
                }
            }
            MirDataType::Function {
                return_type,
                parameters,
            } => {
                if let MirDataType::Function {
                    return_type: concrete_ret,
                    parameters: concrete_params,
                } = concrete
                {
                    self.unify_types(_scope, return_type, concrete_ret, generic_params, subst)?;
                    for (p1, p2) in parameters.iter().zip(concrete_params.iter()) {
                        self.unify_types(_scope, p1, p2, generic_params, subst)?;
                    }
                }
            }
            MirDataType::Option(inner) => {
                if let MirDataType::Option(concrete_inner) = concrete {
                    self.unify_types(_scope, inner, concrete_inner, generic_params, subst)?;
                }
            }
            MirDataType::Result { ok, err } => {
                if let MirDataType::Result {
                    ok: concrete_ok,
                    err: concrete_err,
                } = concrete
                {
                    self.unify_types(_scope, ok, concrete_ok, generic_params, subst)?;
                    self.unify_types(_scope, err, concrete_err, generic_params, subst)?;
                }
            }
            _ => {
                if pattern != concrete {
                    // TODO Handle types not matching
                }
            }
        }

        Ok(())
    }

    pub fn monomorphize_function(
        &mut self,
        scope: ScopeId,
        original_key: VariableKey,
        concrete_args: Vec<MirDataType>,
    ) -> Result<VariableKey, MiddleErr> {
        let cache_key: (VariableKey, Vec<MirDataType>) =
            (original_key.clone(), concrete_args.clone());

        if let Some(cached) = self.monomorphizer.functions.get(&cache_key) {
            return Ok(cached.clone());
        }

        let mono_key = self.instantiate_function(scope, original_key, &concrete_args)?;
        self.monomorphizer
            .functions
            .insert(cache_key, mono_key.clone());

        Ok(mono_key)
    }

    fn instantiate_function(
        &mut self,
        scope: ScopeId,
        original_key: VariableKey,
        concrete_args: &[MirDataType],
    ) -> Result<VariableKey, MiddleErr> {
        let template_key = self
            .monomorphizer
            .function_template_key(&original_key)
            .cloned()
            .ok_or_else(|| MiddleErr::Variable("Generic function not found".to_string()))?;

        let (generic_params, header, body) = self
            .monomorphizer
            .generic_fn_templates
            .get(&template_key)
            .cloned()
            .ok_or_else(|| MiddleErr::Variable("Generic function not found".to_string()))?;

        let subst = generic_params
            .iter()
            .zip(concrete_args.iter())
            .map(|(param, concrete)| {
                (
                    param.as_str().to_string(),
                    ParserDataType::from(concrete.clone()),
                )
            })
            .collect();

        let base_name = original_key.name();
        let name = Ustr::from(&mangle_name(base_name.as_str(), concrete_args));

        let span = body.span;

        let original_scope = self
            .symbols
            .variables
            .get(&template_key)
            .map(|x| x.scope)
            .unwrap_or(scope);

        let header = header.substitute(&subst);

        let data_type = header.type_of(self, original_scope, span).unwrap();

        let key =
            self.register_variable(original_scope, name, data_type.clone(), VarType::Constant)?;

        let substituted_body = body.substitute(&subst);

        self.process_parameter_defaults(scope, &header, key.clone());

        let func = AstFunction {
            header,
            body: Box::new(substituted_body),
        };

        let node = AstNode::new(
            span,
            AstNodeType::VariableDeclaration(AstDeclaration {
                var_type: VarType::Constant,
                identifier: PotentialDollarIdentifier::new(span, name),
                value: Box::new(AstNode::new(span, AstNodeType::FunctionDeclaration(func))),
                data_type: ParserDataType::from(data_type),
                declared: true,
            }),
        );

        self.monomorphizer
            .awaiting_lowering
            .push((node, original_scope));

        Ok(key)
    }

    pub(crate) fn add_type_template(
        &mut self,
        _scope: ScopeId,
        name: MirDataType,
        generic_params: Vec<Ustr>,
        type_def: TypeDefType,
    ) -> bool {
        if generic_params.is_empty() {
            return false;
        }

        self.monomorphizer
            .generic_type_templates
            .entry(name)
            .or_insert(TypeTemplate {
                generic_params,
                type_def,
                impls: Vec::new(),
            });

        true
    }

    pub(crate) fn add_function_template(
        &mut self,
        scope: ScopeId,
        name: VariableKey,
        header: &AstFunction,
        mut generic_params: Vec<Ustr>,
    ) -> bool {
        for param in self.scoping.generic_param_stack.iter().flatten() {
            if !generic_params.contains(param) {
                generic_params.push(*param);
            }
        }

        let generics: Vec<Ustr> = header
            .header
            .generics
            .0
            .iter()
            .filter_map(|generic| {
                self.resolve(scope, generic, ResolutionOptions::default().with_dollar())
                    .ok()
                    .map(|key| key.unwrap_dollar())
            })
            .collect();

        for param in generics {
            if !generic_params.contains(&param) {
                generic_params.push(param);
            }
        }

        if generic_params.is_empty() {
            return false;
        }

        self.monomorphizer
            .generic_fn_templates
            .entry(name)
            .and_modify(|(existing_params, _, _)| {
                for param in &generic_params {
                    if !existing_params.contains(param) {
                        existing_params.push(*param);
                    }
                }
            })
            .or_insert((
                generic_params,
                header.header.clone(),
                (*header.body).clone(),
            ));

        true
    }
}

impl MiddleEnvironment {
    pub fn monomorphize_type_impls(
        &mut self,
        scope: ScopeId,
        original_type: MirDataType,
        concrete_args: Vec<MirDataType>,
    ) -> Result<(), MiddleErr> {
        let (generic_params, impls) = {
            let template = self
                .monomorphizer
                .generic_type_templates
                .get(&original_type)
                .ok_or_else(|| MiddleErr::Variable("Type template not found".to_string()))?;
            (template.generic_params.clone(), template.impls.clone())
        };

        let subst = generic_params
            .iter()
            .zip(concrete_args.iter())
            .map(|(param, concrete)| {
                (
                    param.as_str().to_string(),
                    ParserDataType::from(concrete.clone()),
                )
            })
            .collect();

        let scope = match &original_type {
            MirDataType::Struct { identifier, .. } => {
                self.typing.objects.get(identifier).map(|x| x.scope)
            }
            _ => None,
        }
        .unwrap_or(scope);

        for node in impls {
            let span = node.span;
            let AstNodeType::ImplDeclaration(mut implementation) =
                node.substitute(&subst).node_type
            else {
                continue;
            };

            implementation
                .variables
                .retain(|member| !is_function_impl_member(member));

            if !implementation.variables.is_empty() {
                self.monomorphizer.awaiting_lowering.push((
                    AstNode::new(span, AstNodeType::ImplDeclaration(implementation)),
                    scope,
                ));
            }
        }

        Ok(())
    }

    pub fn monomorphize_type(
        &mut self,
        scope: ScopeId,
        original_type: MirDataType,
        concrete_args: Vec<MirDataType>,
    ) -> Result<MirDataType, MiddleErr> {
        let cache_key = (original_type.clone(), concrete_args.clone());

        if let Some(cached) = self.monomorphizer.types.get(&cache_key) {
            return Ok(cached.clone());
        }

        let template = self
            .monomorphizer
            .generic_type_templates
            .get(&original_type)
            .ok_or_else(|| MiddleErr::Variable("Type template not found".to_string()))?;

        let subst = template
            .generic_params
            .iter()
            .zip(concrete_args.iter())
            .map(|(param, concrete)| {
                (
                    param.as_str().to_string(),
                    ParserDataType::from(concrete.clone()),
                )
            })
            .collect();

        let base_name = match &original_type {
            MirDataType::Struct { identifier, .. } => identifier
                .fully_qualified_path
                .name
                .map(|x| x.to_string())
                .unwrap_or_else(|| "unknown".to_string()),
            _ => "unknown".to_string(),
        };

        let mono_name = mangle_name(&base_name, &concrete_args);

        let parent_path = match &original_type {
            MirDataType::Struct { identifier, .. } => identifier.fully_qualified_path.clone(),
            _ => FullyQualifiedPath::combine(None, Ustr::from("unknown")),
        };

        let type_key = MirDataType::Struct {
            identifier: TypeKey {
                fully_qualified_path: Arc::new(FullyQualifiedPath {
                    name: Some(Ustr::from(&mono_name)),
                    parent: Some(parent_path),
                }),
            },
            generic_types: Vec::new(),
        };

        // TODO Find a way to get the original type span
        let span = self.context.current_span();

        let type_node = AstNode::new(
            span,
            AstNodeType::TypeDeclaration(AstType {
                identifier: PotentialGenericTypeIdentifier::Identifier(
                    PotentialDollarIdentifier::new(span, Ustr::from(&mono_name)),
                ),
                object: template.type_def.clone().substitute(&subst),
            }),
        );

        let type_scope = match &original_type {
            MirDataType::Struct { identifier, .. } => {
                self.typing.objects.get(identifier).map(|x| x.scope)
            }
            _ => None,
        }
        .unwrap_or(scope);

        self.monomorphizer
            .awaiting_lowering
            .push((type_node, type_scope));

        self.monomorphizer.types.insert(cache_key, type_key.clone());

        self.monomorphize_type_impls(scope, original_type, concrete_args)?;

        Ok(type_key)
    }
}
