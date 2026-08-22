use crate::{environment::MiddleEnvironment, errors::MiddleErr, symbols::MiddleVariable};
use calibre_parser::ast::{
    idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
    nodes::{Node, NodeType, VarType},
    types::{ParserDataType, ParserInnerType},
};

impl MiddleEnvironment {
    pub fn predeclare_nodes(&mut self, scope: &u64, nodes: &mut [Node]) -> Result<(), MiddleErr> {
        for node in nodes {
            self.predeclare_node(scope, node)?;
        }

        Ok(())
    }

    fn predeclare_node(&mut self, scope: &u64, node: &mut Node) -> Result<(), MiddleErr> {
        match &mut node.node_type {
            NodeType::Tag { node: inner, .. } => self.predeclare_node(scope, inner.as_mut()),
            NodeType::TypeDeclaration {
                identifier:
                    PotentialGenericTypeIdentifier::Identifier(PotentialDollarIdentifier::Identifier(
                        ident,
                    )),
                ..
            } => {
                // TODO Account for types

                Ok(())
            }
            NodeType::VariableDeclaration {
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
                        .unwrap_or_else(|| {
                            ParserDataType::new(node.span, ParserInnerType::Auto(None))
                        })
                } else {
                    self.resolve_data_type(scope, data_type.clone())
                };

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
            NodeType::ExternFunctionDeclaration {
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
                    params.push(self.resolve_ffi_data_type(scope, ty));
                }

                let return_type = self.resolve_ffi_data_type(scope, return_type.clone());

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
