use crate::{
    ast::types::MirDataType, environment::MiddleEnvironment, errors::MiddleErr, scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
};
use calibre_parser::ast::{
    idents::{PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
    nodes::{
        AstNode, AstNodeType, VarType, declaration::AstDeclaration, functions::AstExtern,
        misc::AstTag, types::AstType,
    },
};
use tracing::instrument;
use ustr::Ustr;

impl MiddleEnvironment {
    #[instrument(skip_all)]
    pub fn predeclare_nodes(&mut self, scope: ScopeId, nodes: &mut [AstNode]) {
        for node in nodes {
            let _ = self.predeclare_node(scope, node);
        }
    }

    fn predeclare_node(&mut self, scope: ScopeId, node: &mut AstNode) -> Result<(), MiddleErr> {
        match &mut node.node_type {
            AstNodeType::Tag(AstTag { node: inner, .. }) => {
                self.predeclare_node(scope, inner.as_mut())
            }
            AstNodeType::TypeDeclaration(AstType {
                identifier:
                    PotentialGenericTypeIdentifier::Identifier(PotentialDollarIdentifier::Identifier(_)),
                ..
            }) => {
                // TODO Account for types

                Ok(())
            }
            AstNodeType::VariableDeclaration(AstDeclaration {
                var_type,
                identifier: PotentialDollarIdentifier::Identifier(ident),
                value,
                data_type,
                declared,
            }) if *var_type == VarType::Constant => {
                let name = Ustr::from(&ident.text);
                if *declared {
                    return Ok(());
                }

                *data_type = if data_type.is_auto() {
                    self.resolve_type_from_node(scope, value).ok_or_else(|| {
                        self.context
                            .err_at_current(MiddleErr::CannotInferVariableType(ident.to_string()))
                    })?
                } else {
                    self.resolve_data_type(scope, &*data_type, ResolutionOptions::typing())?
                }
                .into();

                self.register_variable_with_temp_scope(
                    scope,
                    name,
                    data_type.clone().into(),
                    VarType::Constant,
                    false,
                )?;

                *declared = true;
                Ok(())
            }
            AstNodeType::ExternFunctionDeclaration(AstExtern {
                identifier: PotentialDollarIdentifier::Identifier(ident),
                parameters,
                return_type,
                declared,
                ..
            }) => {
                let name = Ustr::from(&ident.text);
                if *declared {
                    return Ok(());
                }

                let mut params = Vec::new();
                for ty in parameters.clone() {
                    params.push(self.resolve_data_type(
                        scope,
                        &ty.resolve_ffi(),
                        ResolutionOptions::typing(),
                    )?);
                }

                let return_type = self.resolve_data_type(
                    scope,
                    &return_type.clone().resolve_ffi(),
                    ResolutionOptions::typing(),
                )?;

                let data_type = MirDataType::function(params, return_type);

                self.register_variable_with_temp_scope(
                    scope,
                    name,
                    data_type,
                    VarType::Mutable,
                    false,
                )?;

                *declared = true;
                Ok(())
            }
            _ => Ok(()),
        }
    }
}
