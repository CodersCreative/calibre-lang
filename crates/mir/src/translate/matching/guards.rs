use super::patterns::BindingDeclaration;
use crate::{environment::MiddleEnvironment, traversal::NodeVisitor};
use calibre_parser::ast::nodes::{AstNode, AstNodeType};
use ustr::Ustr;

struct GuardRewriter<'a> {
    bindings: &'a [(Ustr, AstNode)],
}

impl<'a> NodeVisitor for GuardRewriter<'a> {
    fn visit_node_type(&mut self, node_type: AstNodeType) -> AstNodeType {
        match node_type {
            AstNodeType::Identifier(id) => {
                if let Some((_, replacement)) = self
                    .bindings
                    .iter()
                    .find(|(name, _)| *name == id.value.get_ident().text())
                {
                    replacement.node_type.clone()
                } else {
                    AstNodeType::Identifier(id)
                }
            }
            other => self.visit_children(other),
        }
    }
}

pub struct GuardProcessor;

impl GuardProcessor {
    pub fn rewrite_guard_bindings(
        env: &mut MiddleEnvironment,
        guards: &[AstNode],
        bindings: &[BindingDeclaration],
    ) -> AstNode {
        let bindings: Vec<(Ustr, AstNode)> =
            bindings.iter().map(|b| (b.name, b.value.clone())).collect();

        Self::rewrite_guards(env, guards, &bindings)
    }

    pub fn rewrite_guards(
        env: &mut MiddleEnvironment,
        guards: &[AstNode],
        bindings: &[(Ustr, AstNode)],
    ) -> AstNode {
        env.fold_and_conditions(
            guards
                .iter()
                .map(|guard| {
                    let mut rewriter = GuardRewriter { bindings };

                    rewriter.visit(guard.clone())
                })
                .collect(),
        )
    }

    pub fn combine_with_guards(
        env: &mut MiddleEnvironment,
        pattern_condition: AstNode,
        guards: &[AstNode],
        bindings: &[BindingDeclaration],
    ) -> AstNode {
        let guard_condition = Self::rewrite_guard_bindings(env, guards, bindings);
        env.bool_and_nodes(pattern_condition, guard_condition)
    }
}
