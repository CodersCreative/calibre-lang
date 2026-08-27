/*
This file handles :
VariableDeclaration,
ScopeDeclaration,
FunctionDeclaration,
ExternFunction
*/

use crate::{
    ast::{LirClosure, LirDeclare, LirExtern, LirLoad, LirNode, LirNodeType},
    environment::{LirEnvironment, LirFunction, LirGlobal},
    translate::LirLowering,
};
use calibre_mir::ast::{
    MiddleNodeType, MirExtern, MirFunction, MirReturn, MirScopeDecl, MirVarDecl,
};
use calibre_parser::{
    Span,
    ast::types::{ParserDataType, ParserInnerType},
};
use rustc_hash::FxHashSet;

impl LirLowering for MirVarDecl {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        if let MiddleNodeType::FunctionDeclaration { .. } = self.value.node_type {
            env.last_ident = Some(self.identifier.to_string());
        } else {
            env.last_ident = None;
        }

        let val = env.lower_node(*self.value);

        env.add_instr(LirNode::new(
            self.identifier.span,
            LirNodeType::Declare(LirDeclare {
                dest: self.identifier.to_string().into_boxed_str(),
                data_type: self.data_type,
                value: Box::new(val),
            }),
        ));

        LirNodeType::null()
    }
}

impl LirLowering for MirScopeDecl {
    #[inline(always)]
    fn lower<'a>(mut self, env: &mut LirEnvironment<'a>, span: Span) -> LirNodeType {
        if !self.is_temp {
            if !env.allow_global_hoist {
                env.lower_scope_items(self.body);
                return LirNodeType::null();
            }

            for stmt in self.body {
                if let MiddleNodeType::VariableDeclaration(MirVarDecl {
                    identifier,
                    data_type,
                    ..
                }) = &stmt.node_type
                {
                    let global_name = identifier.to_string();
                    let global_type = data_type.clone();
                    let mut sub_lowerer = LirEnvironment::new_with_hoist(env.env, false);

                    let _ = sub_lowerer.lower_node(stmt);

                    env.registry.append(sub_lowerer.registry);

                    env.registry.globals.insert(
                        global_name.clone(),
                        LirGlobal {
                            name: global_name.into_boxed_str(),
                            data_type: global_type,
                            blocks: sub_lowerer.blocks.into_boxed_slice(),
                        },
                    );
                } else {
                    env.lower_and_add_node(stmt);
                }
            }

            LirNodeType::null()
        } else {
            let last = self.body.pop();
            env.lower_scope_items(self.body);

            let Some(last) = last else {
                return LirNodeType::null();
            };

            let temp = env.get_temp();
            let lowered = env.lower_node(last.clone());

            if lowered.is_null() {
                env.lower_and_add_node(last);
                return LirNodeType::null();
            }

            env.add_instr(LirNode::new(
                span,
                LirNodeType::Declare(LirDeclare {
                    dest: temp.clone().into_boxed_str(),
                    data_type: ParserDataType::auto(span),
                    value: Box::new(lowered),
                }),
            ));

            LirNodeType::Load(LirLoad {
                value: temp.into_boxed_str(),
            })
        }
    }
}

impl LirLowering for MirFunction {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirNodeType {
        let param_names: FxHashSet<String> = self
            .parameters
            .iter()
            .map(|(name, _, _)| name.text.clone())
            .collect();

        let captures: Vec<(String, ParserDataType)> = self
            .body
            .captured()
            .into_iter()
            .filter(|x| !param_names.contains(x.as_str()))
            .map(|cap| {
                (
                    cap.clone(),
                    env.env
                        .symbols
                        .variables
                        .get(cap)
                        .map(|v| v.data_type.clone())
                        .unwrap_or_else(|| {
                            ParserDataType::new(Span::default(), ParserInnerType::Dynamic)
                        }),
                )
            })
            .collect();

        let internal_name = env.next_function_label();
        let mut sub_lowerer = LirEnvironment::new_with_hoist(env.env, false);

        let body_span = self.body.span;
        let is_temp_body = matches!(
            self.body.node_type,
            MiddleNodeType::ScopeDeclaration(MirScopeDecl { is_temp: true, .. })
        );
        let fallback_expr = match &self.body.node_type {
            MiddleNodeType::ScopeDeclaration(MirScopeDecl { body, .. }) => body.last().cloned(),
            _ => None,
        };

        let (mut has_body_value, mut body_val) =
            if let MiddleNodeType::Conditional { .. } = &self.body.node_type {
                let _ = MirReturn {
                    value: Some(self.body.clone()),
                }
                .lower(env, span);
                (false, LirNodeType::null())
            } else {
                let body = sub_lowerer.lower_node(*self.body);
                if is_temp_body {
                    (false, LirNodeType::null())
                } else {
                    (!body.is_null(), body)
                }
            };

        if !has_body_value && let Some(expr) = fallback_expr {
            if expr.node_type.is_simple_function_fallback() {
                body_val = sub_lowerer.lower_node(expr);
                has_body_value = true;
            } else if is_temp_body {
                sub_lowerer.lower_and_add_node(expr);
            }
        }

        if sub_lowerer
            .blocks
            .last()
            .map(|b| b.terminator.is_none())
            .unwrap_or(false)
            && has_body_value
        {
            sub_lowerer.emit_return_value(body_span, Some(body_val));
        }

        env.registry.append(sub_lowerer.registry);

        let mut capture_names = Vec::with_capacity(captures.len());
        let mut captures_for_func = Vec::with_capacity(captures.len());

        for (n, t) in captures.into_iter() {
            capture_names.push(n.clone().into_boxed_str());
            captures_for_func.push((n.into_boxed_str(), t));
        }

        env.registry.functions.insert(
            internal_name.clone(),
            LirFunction {
                name: internal_name.clone().into_boxed_str(),
                params: self
                    .parameters
                    .into_iter()
                    .map(|x| (x.0.text.into_boxed_str(), x.1))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                captures: captures_for_func.into_boxed_slice(),
                return_type: self.return_type,
                blocks: sub_lowerer.blocks.into_boxed_slice(),
            },
        );

        LirNodeType::Closure(LirClosure {
            label: internal_name.into_boxed_str(),
            captures: capture_names,
        })
    }
}

impl LirLowering for MirExtern {
    #[inline(always)]
    fn lower<'a>(self, _env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        LirNodeType::ExternFunction(LirExtern {
            abi: self.abi.into_boxed_str(),
            library: self.library.into_boxed_str(),
            symbol: self.symbol.into_boxed_str(),
            parameters: self.parameters,
            return_type: self.return_type,
        })
    }
}
