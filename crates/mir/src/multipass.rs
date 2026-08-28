use crate::{
    environment::MiddleEnvironment, errors::MiddleErr, scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
};
use calibre_parser::ast::{
    idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
    nodes::{AstNode, AstNodeType, VarType},
    types::ParserDataType,
};

impl MiddleEnvironment {
    pub fn predeclare_nodes(&mut self, scope: ScopeId, nodes: &mut [AstNode]) {
        for node in nodes {
            let _ = self.predeclare_node(scope, node);
        }
    }

    fn predeclare_node(&mut self, scope: ScopeId, node: &mut AstNode) -> Result<(), MiddleErr> {
        match &mut node.node_type {
            AstNodeType::Tag { node: inner, .. } => self.predeclare_node(scope, inner.as_mut()),
            AstNodeType::TypeDeclaration {
                identifier:
                    PotentialGenericTypeIdentifier::Identifier(PotentialDollarIdentifier::Identifier(_)),
                ..
            } => {
                // TODO Account for types

                Ok(())
            }
            AstNodeType::VariableDeclaration {
                var_type,
                identifier: PotentialDollarIdentifier::Identifier(ident),
                value,
                data_type,
            } if *var_type == VarType::Constant => {
                let new_name = ParserText::temp_name_with_suffix(&ident, node.span);

                if self.symbols.variables.contains_key(&new_name.text) {
                    return Ok(());
                }

                *data_type = if data_type.is_auto() {
                    self.resolve_type_from_node(scope, value)
                        .ok_or_else(|| self.context.err_at_current(MiddleErr::InferImpossible))?
                } else {
                    self.resolve_data_type(scope, &*data_type, ResolutionOptions::typing())?
                };

                self.register_variable(
                    scope,
                    &ident.text,
                    new_name.text.clone(),
                    data_type.clone(),
                    VarType::Constant,
                )?;

                *ident = new_name;

                Ok(())
            }
            AstNodeType::ExternFunctionDeclaration {
                identifier: PotentialDollarIdentifier::Identifier(ident),
                parameters,
                return_type,
                ..
            } => {
                let new_name = ParserText::temp_name_with_suffix(&ident, node.span);

                if self.symbols.variables.contains_key(&new_name.text) {
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

                let data_type = ParserDataType::function(
                    self.context.current_span(),
                    params.clone(),
                    return_type.clone(),
                );

                self.register_variable(
                    scope,
                    &ident.text,
                    new_name.text.clone(),
                    data_type.clone(),
                    VarType::Mutable,
                )?;

                *ident = new_name;

                Ok(())
            }
            _ => Ok(()),
        }
    }
}
