/*
This file handles :
VariableDeclaration,
ScopeDeclaration,
FunctionDeclaration,
ExternFunction
*/

use crate::{
    ast::{LirClosure, LirDeclare, LirExtern, LirNode, LirNodeType},
    environment::{LirEnvironment, LirFunction, LirGlobal},
    translate::LirLowering,
};
use calibre_mir::{
    ast::{MiddleNodeType, MirExtern, MirFunction, MirScopeDecl, MirVarDecl, types::MirDataType},
    symbols::VariableKey,
};
use calibre_parser::{Span, ast::nodes::VarType};
use rustc_hash::FxHashSet;

impl LirLowering for MirVarDecl {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirNodeType {
        if let MiddleNodeType::FunctionDeclaration { .. } = self.value.node_type {
            env.last_ident = Some(self.identifier.clone());
        } else {
            env.last_ident = None;
        }

        let val = env.lower_node(*self.value);

        // This works but theres definitely potential to reduce the number of referenced variables substantially
        let is_referenced = self.var_type == VarType::Mutable
            || env.referenced_identifiers.contains(&self.identifier);

        env.add_instr(LirNode::new(
            span,
            LirNodeType::Declare(LirDeclare {
                dest: self.identifier,
                data_type: self.data_type,
                value: Box::new(val),
                is_referenced,
            }),
        ));

        LirNodeType::null()
    }
}

impl LirLowering for MirScopeDecl {
    #[inline(always)]
    fn lower<'a>(mut self, env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        if self.function_body || (!env.allow_global_hoist && !self.is_temp) {
            env.lower_scope_items(self.body);
            return LirNodeType::null();
        } else if self.is_temp {
            let body = std::mem::take(&mut self.body);
            let mut body = body.into_vec();
            let last = body.pop();

            env.lower_scope_items(body);

            let Some(last) = last else {
                return LirNodeType::null();
            };

            return env.lower_node(last.clone());
        }

        for stmt in self.body {
            let is_non_fn_var_decl = matches!(
                &stmt.node_type,
                MiddleNodeType::VariableDeclaration(MirVarDecl { value, .. }) if !value.is_function()
            );

            if is_non_fn_var_decl {
                if let MiddleNodeType::VariableDeclaration(MirVarDecl {
                    identifier,
                    data_type,
                    ..
                }) = &stmt.node_type
                {
                    let global_type = data_type.clone();
                    let identifier = identifier.clone();

                    let mut sub_lowerer =
                        LirEnvironment::new_with_hoist(env.env, env.counter, false);

                    let _ = sub_lowerer.lower_node(stmt);

                    env.counter = sub_lowerer.counter;

                    env.registry.append(sub_lowerer.registry);

                    env.registry.globals.insert(
                        identifier.clone(),
                        LirGlobal {
                            name: identifier,
                            data_type: global_type,
                            blocks: sub_lowerer.blocks.into_iter().map(Some).collect(),
                        },
                    );
                }
            } else {
                env.lower_and_add_node(stmt);
            }
        }

        LirNodeType::null()
    }
}

impl LirLowering for MirFunction {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        let referenced_names = self.body.identifiers_referenced(true, false);
        let mut referenced_params = 0;
        let param_names: FxHashSet<VariableKey> = self
            .parameters
            .iter()
            .enumerate()
            .map(|(i, (name, _, _))| {
                if referenced_names.contains(name) {
                    referenced_params |= 1 << i;
                }
                name.clone()
            })
            .collect();

        let captures: Vec<(VariableKey, MirDataType)> = self
            .body
            .captured()
            .into_iter()
            .filter(|x| !param_names.contains(x))
            .map(|cap| {
                (
                    cap.clone(),
                    env.env
                        .symbols
                        .variables
                        .get(cap)
                        .map(|v| v.data_type.clone())
                        .unwrap_or_else(|| MirDataType::Dynamic),
                )
            })
            .collect();

        let internal_name = env.next_function_label();

        let mut sub_lowerer = LirEnvironment::new_with_hoist(env.env, env.counter, false);
        sub_lowerer.referenced_identifiers = referenced_names;

        let body_span = self.body.span;

        let (has_body_value, body_val) = {
            let body = sub_lowerer.lower_node(*self.body);
            (!body.is_null(), body)
        };

        if sub_lowerer
            .blocks
            .last()
            .map(|b| b.terminator.is_none())
            .unwrap_or(false)
            && has_body_value
        {
            sub_lowerer.emit_return_value(body_span, Some(body_val));
        }

        env.counter = sub_lowerer.counter;
        env.registry.append(sub_lowerer.registry);

        let mut capture_names = Vec::with_capacity(captures.len());
        let mut captures_for_func = Vec::with_capacity(captures.len());

        for (n, t) in captures.into_iter() {
            capture_names.push(n.clone());
            captures_for_func.push((n, t));
        }

        let mut memo_params = 0;
        for (i, param) in self.parameters.iter().enumerate() {
            if self.memo_params.contains(param.0.name()) {
                memo_params |= 1 << i;
            }
        }

        env.registry.functions.insert(
            internal_name.clone(),
            LirFunction {
                name: internal_name.clone(),
                params: self
                    .parameters
                    .into_iter()
                    .map(|x| (x.0, x.1))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                captures: captures_for_func.into_boxed_slice(),
                return_type: self.return_type,
                blocks: sub_lowerer.blocks.into_iter().map(Some).collect(),
                pure: self.pure,
                referenced_params,
                memo_params,
                memo: self.memo,
            },
        );

        LirNodeType::Closure(LirClosure {
            label: internal_name,
            captures: capture_names.into_boxed_slice(),
        })
    }
}

impl LirLowering for MirExtern {
    #[inline(always)]
    fn lower<'a>(self, _env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        let mut memo_params = 0;
        for entry in &self.memo_params {
            if let Ok(index) = entry.parse::<usize>()
                && index < self.parameters.len()
            {
                memo_params |= 1 << index;
            }
        }

        LirNodeType::ExternFunction(LirExtern {
            abi: self.abi,
            library: self.library,
            symbol: self.symbol,
            parameters: self.parameters,
            return_type: self.return_type,
            memo: self.memo,
            pure: self.pure,
            memo_params,
        })
    }
}
