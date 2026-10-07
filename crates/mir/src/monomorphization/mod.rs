use crate::{
    ast::types::MirDataType,
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    symbols::{MiddleOverload, TypeKey, VariableKey},
    translate::MirLowering,
};
use calibre_parser::ast::{
    idents::PotentialDollarIdentifier,
    nodes::{AstNode, AstNodeType, VarType, declaration::AstDeclaration},
    types::ParserDataType,
};
use rustc_hash::FxHashMap;
use ustr::Ustr;

#[derive(Debug, Clone, Default)]
pub struct Monomorphizer {
    pub awaiting_lowering: Vec<(AstNode, ScopeId)>,
    pub functions: FxHashMap<(VariableKey, Vec<MirDataType>), VariableKey>,
    pub types: FxHashMap<(TypeKey, Vec<MirDataType>), TypeKey>,
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
            .symbols
            .generic_fn_templates
            .get(&original_key)
            .cloned()
            .ok_or_else(|| MiddleErr::Variable("Generic function not found".to_string()))?;

        let mut subst = FxHashMap::default();
        for (param, concrete) in generic_params.iter().zip(concrete_args.iter()) {
            subst.insert(
                param.as_str().to_string(),
                ParserDataType::from(concrete.clone()),
            );
        }

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
                value: Box::new(body.clone().substitute(&subst)),
                data_type: ParserDataType::from(data_type),
                declared: true,
            }),
        );

        self.monomorphizer
            .awaiting_lowering
            .push((node, original_scope));

        Ok(key)
    }
}
