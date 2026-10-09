use crate::{
    ast::types::MirDataType,
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::{FullyQualifiedPath, ScopeId},
    symbols::{MiddleOverload, TypeKey, VariableKey, resolve::ResolutionOptions},
    translate::MirLowering,
};
use calibre_parser::ast::{
    Substitutable,
    idents::PotentialDollarIdentifier,
    nodes::{
        AstNode, AstNodeType, VarType,
        declaration::AstDeclaration,
        functions::{AstFunction, FunctionHeader},
        types::TypeDefType,
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
    pub fn append(&mut self, other: Self) {}
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

        let mut concrete_args = Vec::with_capacity(generic_params.len());

        for param in generic_params {
            let param_str = param.as_str();
            if let Some(ty) = subst.get(param_str)
                && !concrete_args.contains(ty)
            {
                concrete_args.push(ty.clone());
            } else {
                // TODO Probably deal with this better
                concrete_args.push(MirDataType::Struct {
                    identifier: TypeKey {
                        fully_qualified_path: FullyQualifiedPath::combine(None, *param),
                    },
                    generic_types: Vec::new(),
                });
            }
        }

        Ok(concrete_args)
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
        let (generic_params, mut header, body) = self
            .monomorphizer
            .generic_fn_templates
            .get(&original_key)
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

        header = header.substitute(&subst);

        let name = Ustr::from(&format!(
            "{}_{}",
            original_key,
            generic_params
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join("_")
        ));

        let span = body.span;

        let data_type = header.type_of(self, scope, span).unwrap();

        let original_scope = self
            .symbols
            .variables
            .get(&original_key)
            .map(|x| x.scope)
            .unwrap();

        let key =
            self.register_variable(original_scope, name, data_type.clone(), VarType::Constant)?;

        let node = AstNode::new(
            span,
            AstNodeType::VariableDeclaration(AstDeclaration {
                var_type: VarType::Constant,
                identifier: PotentialDollarIdentifier::new(span, name),
                value: Box::new(body.substitute(&subst)),
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

    pub(crate) fn add_type_impl(
        &mut self,
        _scope: ScopeId,
        name: &MirDataType,
        generic_params: Vec<Ustr>,
        value: AstNode,
    ) -> bool {
        if generic_params.is_empty() {
            return false;
        }

        if let Some(template) = self.monomorphizer.generic_type_templates.get_mut(name) {
            template.impls.push(value.clone());
        } else {
            return false;
        }

        let scope = match name {
            MirDataType::Struct { identifier, .. } => {
                self.typing.objects.get(identifier).unwrap().scope
            }
            _ => self.scoping.get_global_scope().unwrap(),
        };

        self.monomorphizer.awaiting_lowering.extend(
            self.monomorphizer
                .types
                .keys()
                .filter(|x| &x.0 == name)
                .map(|(_, concrete_args)| {
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

                    let value = value.clone().substitute(&subst);

                    (value, scope)
                }),
        );

        true
    }

    pub(crate) fn add_function_template(
        &mut self,
        scope: ScopeId,
        name: VariableKey,
        header: &AstFunction,
    ) -> bool {
        if header.header.generics.0.is_empty() {
            return false;
        }

        let generic_params: Vec<Ustr> = header
            .header
            .generics
            .0
            .iter()
            .map(|g| {
                self.resolve(scope, g, ResolutionOptions::default().with_dollar())
                    .map(|x| x.unwrap_dollar())
            })
            .collect::<Result<Vec<_>, MiddleErr>>()
            .unwrap_or_default();

        self.monomorphizer
            .generic_fn_templates
            .entry(name)
            .or_insert((
                generic_params,
                header.header.clone(),
                (*header.body).clone(),
            ));

        true
    }

    pub fn monomorphize_type_impls(
        &mut self,
        scope: ScopeId,
        original_type: MirDataType,
        concrete_args: Vec<MirDataType>,
    ) -> Result<(), MiddleErr> {
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

        let scope = match &original_type {
            MirDataType::Struct { identifier, .. } => {
                self.typing.objects.get(identifier).map(|x| x.scope)
            }
            _ => None,
        }
        .unwrap_or(scope);

        self.monomorphizer.awaiting_lowering.extend(
            template
                .impls
                .iter()
                .map(|node| (node.clone().substitute(&subst), scope)),
        );

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

        // TODO Re-add this to be evaluated normally
        let _mono_type_def = template.type_def.clone().substitute(&subst);

        let base_name = match &original_type {
            MirDataType::Struct { identifier, .. } => identifier
                .fully_qualified_path
                .name
                .map(|x| x.to_string())
                .unwrap_or_else(|| "unknown".to_string()),
            _ => "unknown".to_string(),
        };

        let mono_name = format!(
            "{}_{}",
            base_name,
            template
                .generic_params
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join("_")
        );

        let mono_identifier = TypeKey {
            fully_qualified_path: Arc::new(FullyQualifiedPath {
                name: Some(Ustr::from(&mono_name)),
                parent: None,
            }),
        };

        let mono_type = MirDataType::Struct {
            identifier: mono_identifier,
            generic_types: vec![],
        };

        self.monomorphizer
            .types
            .insert(cache_key, mono_type.clone());

        self.monomorphize_type_impls(scope, original_type, concrete_args)?;

        Ok(mono_type)
    }
}
