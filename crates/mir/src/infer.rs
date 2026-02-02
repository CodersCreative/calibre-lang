use crate::environment::MiddleEnvironment;
use calibre_mir_ty::hm::{self, Subst, Type, TypeCon, TypeEnv, TypeGenerator, TypeScheme};
use calibre_parser::ast::{
    Node, NodeType, ParserDataType, ParserInnerType, PotentialNewType, binary::BinaryOperator,
};
use rustc_hash::FxHashMap;

pub fn infer_node_type(
    env: &mut MiddleEnvironment,
    scope: &u64,
    node: &Node,
) -> Option<ParserDataType> {
    let mut tg = TypeGenerator::default();
    let mut subst = Subst::default();

    let mut tenv: TypeEnv = FxHashMap::default();
    for (k, s) in env.hm_env.iter() {
        tenv.insert(k.clone(), s.clone());
    }

    for (k, v) in env.variables.iter() {
        if !tenv.contains_key(k) {
            tenv.insert(
                k.clone(),
                hm::generalize(
                    &FxHashMap::default(),
                    &hm::from_parser_data_type(&v.data_type, &mut tg),
                ),
            );
        }
    }

    match visit(node, env, scope, &mut tg, &mut tenv, &mut subst) {
        Ok(t) => Some(hm::to_parser_data_type(&hm::apply_subst(&subst, &t))),
        Err(_) => None,
    }
}

fn visit(
    node: &Node,
    env: &mut MiddleEnvironment,
    scope: &u64,
    tg: &mut TypeGenerator,
    tenv: &mut TypeEnv,
    subst: &mut hm::Subst,
) -> Result<Type, String> {
    match &node.node_type {
        NodeType::IntLiteral(_) => Ok(Type::TCon(TypeCon::Int)),
        NodeType::FloatLiteral(_) => Ok(Type::TCon(TypeCon::Float)),
        NodeType::StringLiteral(_) => Ok(Type::TCon(TypeCon::Str)),
        NodeType::CharLiteral(_) => Ok(Type::TCon(TypeCon::Char)),
        NodeType::Null => Ok(Type::TCon(TypeCon::Null)),
        NodeType::Identifier(id) => {
            if let Some(pd) = env.resolve_potential_generic_ident_to_data_type(scope, id) {
                return Ok(hm::from_parser_data_type(&pd, tg));
            }

            if let Some(iden) = env.resolve_potential_generic_ident(scope, id) {
                if let Some(scheme) = tenv.get(&iden.text) {
                    if scheme.vars.is_empty() {
                        return Ok(scheme.ty.clone());
                    } else {
                        return Ok(hm::instantiate(scheme, tg));
                    }
                }
            }
            Ok(Type::TCon(TypeCon::Dyn))
        }
        NodeType::ListLiteral(_, items) => {
            let mut elem_ty: Option<Type> = None;
            for it in items.iter() {
                let t = visit(it, env, scope, tg, tenv, subst)?;
                let t_applied = hm::apply_subst(subst, &t);
                if let Some(e) = elem_ty {
                    let e_applied = hm::apply_subst(subst, &e);
                    *subst = hm::unify(subst.clone(), &e_applied, &t_applied).map_err(|e| e)?;
                    elem_ty = Some(hm::apply_subst(subst, &e_applied));
                } else {
                    elem_ty = Some(t_applied);
                }
            }
            Ok(Type::TList(Box::new(elem_ty.unwrap_or(tg.fresh()))))
        }
        NodeType::CallExpression {
            caller,
            args,
            reverse_args,
            ..
        } => {
            let mut all_args: Vec<Node> = args.iter().map(|a| a.clone().into()).collect();
            all_args.append(&mut reverse_args.clone());
            let mut caller_t = visit(caller, env, scope, tg, tenv, subst)?;
            for arg in all_args.iter() {
                let arg_t = visit(arg, env, scope, tg, tenv, subst)?;
                let arg_t_applied = hm::apply_subst(subst, &arg_t);
                let caller_t_applied = hm::apply_subst(subst, &caller_t);
                match caller_t_applied {
                    Type::TArrow(param, rest) => {
                        *subst = hm::unify(subst.clone(), &param, &arg_t_applied).map_err(|e| e)?;
                        caller_t = hm::apply_subst(subst, &*rest);
                    }
                    _ => {
                        let fresh_ret = tg.fresh();
                        let fn_t =
                            Type::TArrow(Box::new(arg_t_applied), Box::new(fresh_ret.clone()));
                        *subst =
                            hm::unify(subst.clone(), &caller_t_applied, &fn_t).map_err(|e| e)?;
                        caller_t = fresh_ret;
                    }
                }
            }
            Ok(hm::apply_subst(subst, &caller_t))
        }
        NodeType::BinaryExpression {
            left,
            right,
            operator,
        } => {
            let lt = visit(left, env, scope, tg, tenv, subst)?;
            let rt = visit(right, env, scope, tg, tenv, subst)?;
            let lt_applied = hm::apply_subst(subst, &lt);
            let rt_applied = hm::apply_subst(subst, &rt);

            match operator {
                BinaryOperator::Add
                | BinaryOperator::Sub
                | BinaryOperator::Mul
                | BinaryOperator::Div
                | BinaryOperator::Mod => {
                    let int_t = Type::TCon(TypeCon::Int);
                    *subst = hm::unify(subst.clone(), &lt_applied, &int_t).map_err(|e| e)?;
                    *subst = hm::unify(subst.clone(), &rt_applied, &int_t).map_err(|e| e)?;
                    Ok(int_t)
                }
                _ => {
                    *subst = hm::unify(subst.clone(), &lt_applied, &rt_applied).map_err(|e| e)?;
                    Ok(hm::apply_subst(subst, &lt_applied))
                }
            }
        }
        NodeType::ComparisonExpression {
            left,
            right,
            operator: _,
        } => {
            let lt = visit(left, env, scope, tg, tenv, subst)?;
            let rt = visit(right, env, scope, tg, tenv, subst)?;
            let lt_applied = hm::apply_subst(subst, &lt);
            let rt_applied = hm::apply_subst(subst, &rt);
            *subst = hm::unify(subst.clone(), &lt_applied, &rt_applied).map_err(|e| e)?;
            Ok(Type::TCon(TypeCon::Bool))
        }
        NodeType::NotExpression { value } => {
            let vt = visit(value, env, scope, tg, tenv, subst)?;
            let vt_applied = hm::apply_subst(subst, &vt);
            *subst =
                hm::unify(subst.clone(), &vt_applied, &Type::TCon(TypeCon::Bool)).map_err(|e| e)?;
            Ok(Type::TCon(TypeCon::Bool))
        }
        NodeType::Return { value } => {
            if let Some(v) = value {
                visit(v, env, scope, tg, tenv, subst)
            } else {
                Ok(Type::TCon(TypeCon::Null))
            }
        }
        NodeType::ScopeDeclaration { body, .. } => {
            if let Some(stmts) = body {
                let mut last = Type::TCon(TypeCon::Null);
                for s in stmts.iter() {
                    last = visit(s, env, scope, tg, tenv, subst)?;
                }
                Ok(last)
            } else {
                Ok(Type::TCon(TypeCon::Null))
            }
        }
        NodeType::IfStatement {
            then, otherwise, ..
        } => {
            let tt = visit(then, env, scope, tg, tenv, subst)?;
            let tt_applied = hm::apply_subst(subst, &tt);
            if let Some(o) = otherwise {
                let ot = visit(o, env, scope, tg, tenv, subst)?;
                let ot_applied = hm::apply_subst(subst, &ot);
                *subst = hm::unify(subst.clone(), &tt_applied, &ot_applied).map_err(|e| e)?;
                Ok(hm::apply_subst(subst, &tt_applied))
            } else {
                Ok(tt_applied)
            }
        }
        NodeType::ParenExpression { value } => visit(value, env, scope, tg, tenv, subst),
        NodeType::FunctionDeclaration { header, body } => {
            let declared_ret_pd = match &header.return_type {
                PotentialNewType::DataType(pd) => pd.clone(),
                _ => ParserDataType::from(ParserInnerType::Auto(None)),
            };
            let declared_ret_t = if matches!(declared_ret_pd.data_type, ParserInnerType::Auto(_)) {
                tg.fresh()
            } else {
                hm::from_parser_data_type(&declared_ret_pd, tg)
            };

            let mut param_types = Vec::new();
            for param in header.parameters.iter() {
                let resolved_name = env
                    .resolve_potential_dollar_ident(scope, &param.0)
                    .map(|p| p.text)
                    .unwrap_or_else(|| param.0.to_string());

                let p_t = if let Some(scheme) = tenv.get(&resolved_name) {
                    if scheme.vars.is_empty() {
                        scheme.ty.clone()
                    } else {
                        hm::instantiate(scheme, tg)
                    }
                } else {
                    let p_pd = match &param.1 {
                        PotentialNewType::DataType(pd) => pd.clone(),
                        _ => ParserDataType::from(ParserInnerType::Auto(None)),
                    };

                    let p_t = if matches!(p_pd.data_type, ParserInnerType::Auto(_)) {
                        tg.fresh()
                    } else {
                        hm::from_parser_data_type(&p_pd, tg)
                    };

                    tenv.insert(
                        resolved_name.clone(),
                        hm::TypeScheme::new(Vec::new(), p_t.clone()),
                    );

                    p_t
                };
                param_types.push(p_t);
            }

            let body_t = visit(body, env, scope, tg, tenv, subst)?;
            let body_t_applied = hm::apply_subst(subst, &body_t);
            *subst = hm::unify(subst.clone(), &body_t_applied, &declared_ret_t).map_err(|e| e)?;

            let mut fn_t = hm::apply_subst(subst, &declared_ret_t);
            for p_t in param_types.iter().rev() {
                let p_t_applied = hm::apply_subst(subst, p_t);
                fn_t = Type::TArrow(Box::new(p_t_applied), Box::new(fn_t));
            }

            Ok(hm::apply_subst(subst, &fn_t))
        }
        _ => Ok(Type::TCon(TypeCon::Dyn)),
    }
}

pub fn infer_node_hm(
    env: &mut MiddleEnvironment,
    scope: &u64,
    node: &Node,
) -> Option<(Type, hm::Subst)> {
    let mut tg = TypeGenerator::default();
    let mut subst = hm::Subst::default();

    let mut tenv: TypeEnv = FxHashMap::default();

    for (k, s) in env.hm_env.iter() {
        tenv.insert(k.clone(), s.clone());
    }

    let mut var_types: FxHashMap<String, Type> = FxHashMap::default();
    for (k, v) in env.variables.iter() {
        if !tenv.contains_key(k) {
            fn contains_auto(pd: &ParserDataType) -> bool {
                match &pd.data_type {
                    ParserInnerType::Auto(_) => true,
                    ParserInnerType::Tuple(xs) => xs.iter().any(|x| contains_auto(x)),
                    ParserInnerType::List(x) => contains_auto(x),
                    ParserInnerType::Option(x) => contains_auto(x),
                    ParserInnerType::Result { ok, err } => contains_auto(ok) || contains_auto(err),
                    ParserInnerType::Function {
                        return_type,
                        parameters,
                        ..
                    } => contains_auto(return_type) || parameters.iter().any(|p| contains_auto(p)),
                    ParserInnerType::Ref(x, _) => contains_auto(x),
                    ParserInnerType::StructWithGenerics { generic_types, .. } => {
                        generic_types.iter().any(|g| contains_auto(g))
                    }
                    ParserInnerType::Scope(xs) => xs.iter().any(|x| contains_auto(x)),
                    _ => false,
                }
            }

            if contains_auto(&v.data_type) {
                let t = tg.fresh();
                var_types.insert(k.clone(), t.clone());
                let scheme = TypeScheme::new(Vec::new(), t.clone());
                tenv.insert(k.clone(), scheme);
            } else {
                let t = hm::from_parser_data_type(&v.data_type, &mut tg);
                var_types.insert(k.clone(), t.clone());
                let scheme = hm::generalize(&FxHashMap::default(), &t);
                tenv.insert(k.clone(), scheme);
            }
        }
    }

    let mut tenv2 = tenv.clone();
    match visit(node, env, scope, &mut tg, &mut tenv2, &mut subst) {
        Ok(t) => {
            for (_, scheme) in env.hm_env.iter_mut() {
                *scheme = TypeScheme::new(scheme.vars.clone(), hm::apply_subst(&subst, &scheme.ty));
            }

            for (k, v) in env.variables.iter_mut() {
                if let Some(scheme) = env.hm_env.get(k) {
                    v.data_type = hm::to_parser_data_type(&scheme.ty);
                }
            }

            for (k, orig_t) in var_types.iter() {
                if let Some(var) = env.variables.get_mut(k) {
                    var.data_type = hm::to_parser_data_type(&hm::apply_subst(&subst, orig_t));
                }
            }

            Some((hm::apply_subst(&subst, &t), subst))
        }
        Err(_) => None,
    }
}
