use crate::{
    ast::{MiddleNode, MiddleNodeType},
    environment::{MiddleEnvironment, MiddleVariable, get_disamubiguous_name},
    errors::MiddleErr,
};
use calibre_parser::{
    Span,
    ast::{
        Node, NodeType, ParserInnerType, ParserText, PotentialDollarIdentifier, PotentialNewType,
        VarType,
    },
};

impl MiddleEnvironment {
    pub fn evaluate_var_declaration(
        &mut self,
        scope: &u64,
        span: Span,
        var_type: VarType,
        identifier: PotentialDollarIdentifier,
        value: Node,
        data_type: PotentialNewType,
    ) -> Result<MiddleNode, MiddleErr> {
        let identifier = self
            .resolve_dollar_ident_only(scope, &identifier)
            .ok_or_else(|| self.err_at_current(MiddleErr::Scope(identifier.to_string())))?;

        let new_name = if identifier.text.contains("->") || identifier.text.contains("::") {
            identifier.text.clone()
        } else {
            get_disamubiguous_name(scope, Some(identifier.text.trim()), Some(&var_type))
        };

        let function_decl = match &value.node_type {
            NodeType::FunctionDeclaration { header, body, .. } => Some((header, body)),
            _ => None,
        };
        if let Some((header, body)) = function_decl
            && !header.generics.0.is_empty()
        {
            let base_name = new_name.clone();
            let template_params: Vec<String> = header
                .generics
                .0
                .iter()
                .map(|g| g.identifier.to_string())
                .collect();
            self.generic_fn_templates.entry(base_name).or_insert((
                template_params,
                (*header).clone(),
                (**body).clone(),
            ));
        }

        let current_location = self.current_location.clone();

        let data_type = if data_type.is_auto() {
            let err = self.err_at_current(MiddleErr::InferImpossible);
            self.resolve_type_from_node(scope, &value).ok_or(err)?
        } else {
            self.resolve_potential_new_type(scope, data_type)
        };

        let mut value = if let Some((header, _)) = function_decl {
            self.variables.insert(
                new_name.clone(),
                MiddleVariable {
                    data_type: data_type.clone(),
                    var_type,
                    location: current_location.clone(),
                },
            );

            let err = self.err_at_current(MiddleErr::Scope(scope.to_string()));
            self.scopes
                .get_mut(scope)
                .ok_or(err)?
                .mappings
                .insert(identifier.text.clone(), new_name.clone());

            let new_scope = self.new_scope_from_parent_shallow(*scope);

            for param in header.parameters.iter() {
                let og_name = self
                    .resolve_dollar_ident_only(scope, &param.0)
                    .ok_or_else(|| self.err_at_current(MiddleErr::Scope(param.0.to_string())))?;

                let new_name =
                    get_disamubiguous_name(scope, Some(og_name.trim()), Some(&VarType::Mutable));

                let data_type = self.resolve_potential_new_type(scope, param.1.clone());

                self.variables.insert(
                    new_name.clone(),
                    MiddleVariable {
                        data_type: data_type.clone(),
                        var_type: VarType::Mutable,
                        location: current_location.clone(),
                    },
                );

                let err = self.err_at_current(MiddleErr::Scope(new_scope.to_string()));
                let scope_ref = self.scopes.get_mut(&new_scope).ok_or(err)?;
                scope_ref
                    .mappings
                    .insert(og_name.text.clone(), new_name.clone());
                scope_ref.defined.push(new_name.clone());
            }

            self.evaluate(&new_scope, value)
        } else {
            self.evaluate(scope, value)
        };

        if matches!(data_type.data_type, ParserInnerType::DynamicTraits(_)) {
            value = MiddleNode::new(
                MiddleNodeType::AsExpression {
                    value: Box::new(value),
                    data_type: data_type.clone(),
                    failure_mode: calibre_parser::ast::AsFailureMode::Panic,
                },
                span,
            );
        }

        Ok(MiddleNode {
            node_type: MiddleNodeType::VariableDeclaration {
                var_type,
                identifier: ParserText {
                    text: new_name,
                    span: identifier.span,
                },
                value: Box::new(value),
                data_type,
            },
            span,
        })
    }
}
