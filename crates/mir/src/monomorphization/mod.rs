use crate::{
    ast::types::MirDataType,
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    symbols::{MiddleOverload, VariableKey, resolve::ResolutionOptions},
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
use ustr::Ustr;

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
        concrete_args: Vec<MirDataType>,
    ) -> Result<VariableKey, MiddleErr> {
        let key = self.monomorphize_function(scope, overload.func.clone(), concrete_args)?;
        Ok(key)
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
}
