use crate::{
    ast::{
        MiddleNode, MiddleNodeType,
        hm::{self, Type, TypeGenerator, TypeScheme},
    },
    environment::{MiddleEnvironment, MiddleVariable, get_disamubiguous_name},
    errors::MiddleErr,
    infer::infer_node_hm,
};
use calibre_parser::{
    ast::{
        Node, NodeType, ParserInnerType, ParserText, PotentialDollarIdentifier, PotentialNewType,
        VarType,
    },
    lexer::Span,
};
use rustc_hash::FxHashMap;

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
        let identifier = self.resolve_dollar_ident_only(scope, &identifier).unwrap();

        let new_name = if identifier.text.contains("->") {
            identifier.text.clone()
        } else {
            get_disamubiguous_name(scope, Some(identifier.text.trim()), Some(&var_type))
        };

        let original_value_node = value.clone();

        if let NodeType::FunctionDeclaration {
            ref header,
            ref body,
            ..
        } = original_value_node.node_type
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
                header.clone(),
                (**body).clone(),
            ));
        }

        let mut data_type = if data_type.is_auto() {
            self.resolve_type_from_node(scope, &value)
                .unwrap_or(self.resolve_potential_new_type(scope, data_type))
        } else {
            self.resolve_potential_new_type(scope, data_type)
        };

        if let NodeType::FunctionDeclaration { ref header, .. } = original_value_node.node_type {
            let mut tg = TypeGenerator::default();
            let ret_pd = self.resolve_potential_new_type(scope, header.return_type.clone());

            let mut ret_hm = if matches!(ret_pd.data_type, ParserInnerType::Auto(_)) {
                tg.fresh()
            } else {
                hm::from_parser_data_type(&ret_pd, &mut tg)
            };

            for param in header.parameters.iter().rev() {
                let p_pd = self.resolve_potential_new_type(scope, param.1.clone());
                let p_hm = if matches!(p_pd.data_type, ParserInnerType::Auto(_)) {
                    tg.fresh()
                } else {
                    hm::from_parser_data_type(&p_pd, &mut tg)
                };

                ret_hm = Type::TArrow(Box::new(p_hm), Box::new(ret_hm));
            }

            if !self.hm_env.contains_key(&new_name) {
                self.hm_env
                    .insert(new_name.clone(), TypeScheme::new(Vec::new(), ret_hm));
            }
        }

        let mut value = if let NodeType::FunctionDeclaration { ref header, .. } =
            original_value_node.node_type
        {
            self.variables.insert(
                new_name.clone(),
                MiddleVariable {
                    data_type: data_type.clone(),
                    var_type: var_type.clone(),
                    location: self.current_location.clone(),
                },
            );

            self.scopes
                .get_mut(scope)
                .unwrap()
                .mappings
                .insert(identifier.text.clone(), new_name.clone());

            let new_scope = self.new_scope_from_parent_shallow(*scope);

            for param in header.parameters.iter() {
                let og_name = self.resolve_dollar_ident_only(scope, &param.0).unwrap();

                let new_name =
                    get_disamubiguous_name(scope, Some(og_name.trim()), Some(&VarType::Mutable));

                let data_type = self.resolve_potential_new_type(scope, param.1.clone());

                self.variables.insert(
                    new_name.clone(),
                    MiddleVariable {
                        data_type: data_type.clone(),
                        var_type: VarType::Mutable,
                        location: self.current_location.clone(),
                    },
                );

                self.scopes
                    .get_mut(&new_scope)
                    .unwrap()
                    .mappings
                    .insert(og_name.text.clone(), new_name.clone());

                self.scopes
                    .get_mut(&new_scope)
                    .unwrap()
                    .defined
                    .push(new_name.clone());
            }

            self.evaluate(&new_scope, value)
        } else {
            self.evaluate(scope, value)
        };

        if !matches!(
            original_value_node.node_type,
            NodeType::FunctionDeclaration { .. }
        ) {
            self.variables.insert(
                new_name.clone(),
                MiddleVariable {
                    data_type: data_type.clone(),
                    var_type: var_type.clone(),
                    location: self.current_location.clone(),
                },
            );

            self.scopes
                .get_mut(scope)
                .unwrap()
                .mappings
                .insert(identifier.text, new_name.clone());
        }

        if data_type.contains_auto()
            && let Some((hm_t, subst)) = infer_node_hm(self, scope, &original_value_node)
        {
            let t_applied = hm::apply_subst(&subst, &hm_t);

            let mut tenv: FxHashMap<String, hm::TypeScheme> = FxHashMap::default();
            for (k, s) in self.hm_env.iter() {
                tenv.insert(k.clone(), s.clone());
            }

            let parser_ty = hm::to_parser_data_type(&t_applied);
            let scheme = match &parser_ty.data_type {
                calibre_parser::ast::ParserInnerType::Function { .. }
                    if parser_ty.contains_auto() =>
                {
                    hm::TypeScheme::new(Vec::new(), t_applied.clone())
                }
                _ => hm::generalize(&tenv, &t_applied),
            };
            self.hm_env.insert(new_name.clone(), scheme);

            if let Some(v) = self.variables.get_mut(&new_name) {
                v.data_type = parser_ty.clone();
                data_type = parser_ty.clone();

                if let MiddleNodeType::FunctionDeclaration {
                    parameters: ref mut params,
                    return_type: ref mut ret_type,
                    ..
                } = value.node_type
                    && let ParserInnerType::Function {
                        return_type: inferred_ret,
                        parameters: inferred_params,
                        is_async: _,
                    } = parser_ty.data_type
                {
                    for (i, (_name, p_ty)) in params.iter_mut().enumerate() {
                        if i < inferred_params.len() {
                            *p_ty = inferred_params[i].clone();
                        }
                    }

                    *ret_type = *inferred_ret.clone();
                }
            }
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
