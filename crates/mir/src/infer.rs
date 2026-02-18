use crate::ast::hm::{self, Subst, Type, TypeCon, TypeEnv, TypeGenerator, TypeScheme};
use crate::environment::{MiddleEnvironment, Operator};
use calibre_parser::ast::{
    CallArg, IfComparisonType, Node, NodeType, ParserDataType, ParserInnerType, PotentialNewType,
    binary::BinaryOperator,
};
use rustc_hash::FxHashMap;

pub fn infer_node_type(
    env: &mut MiddleEnvironment,
    scope: &u64,
    node: &Node,
) -> Option<ParserDataType> {
    let mut tg = TypeGenerator::default();
    let mut subst = Subst::default();

    let mut tenv: TypeEnv = env.hm_env.clone();

    for (k, v) in env.variables.iter() {
        if !tenv.contains_key(k) {
            tenv.insert(
                k.clone(),
                hm::generalize(&tenv, &hm::from_parser_data_type(&v.data_type, &mut tg)),
            );
        }
    }

    match visit(node, env, scope, &mut tg, &mut tenv, &mut subst) {
        Ok(t) => Some(hm::to_parser_data_type(
            &hm::apply_subst(&subst, &t),
            &mut env.type_cache,
        )),
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
    fn resolve_member_fn_type(
        env: &MiddleEnvironment,
        tenv: &TypeEnv,
        base_ty: &Type,
        member: &str,
        tg: &mut TypeGenerator,
    ) -> Option<Type> {
        let base_pd = hm::to_parser_data_type(base_ty, &mut FxHashMap::default()).unwrap_all_refs();
        for key in env.member_fn_candidates(&base_pd, member) {
            if let Some(scheme) = tenv.get(&key).or_else(|| env.hm_env.get(&key)) {
                return Some(hm::instantiate(scheme, tg));
            }
        }
        None
    }

    fn resolve_member_field_type(
        env: &mut MiddleEnvironment,
        scope: &u64,
        base_t: &Type,
        member: &str,
        span: calibre_parser::Span,
    ) -> Option<Type> {
        let base_pd = env
            .resolve_data_type(
                scope,
                hm::to_parser_data_type(base_t, &mut FxHashMap::default()),
            )
            .unwrap_all_refs();
        env.resolve_member_field_type(scope, &base_pd, member, span)
            .map(|v| hm::from_parser_data_type(&v, &mut TypeGenerator::default()))
    }

    fn apply_call_iter<'a, I>(
        mut caller_t: Type,
        call_args: I,
        env: &mut MiddleEnvironment,
        scope: &u64,
        tg: &mut TypeGenerator,
        tenv: &mut TypeEnv,
        subst: &mut hm::Subst,
    ) -> Result<Type, String>
    where
        I: IntoIterator<Item = &'a Node>,
    {
        for arg in call_args {
            let arg_t = visit(arg, env, scope, tg, tenv, subst)?;
            let arg_t_applied = hm::apply_subst(subst, &arg_t);
            let caller_t_applied = hm::apply_subst(subst, &caller_t);
            match caller_t_applied {
                Type::TArrow(param, rest) => {
                    *subst =
                        hm::unify(std::mem::take(subst), &param, &arg_t_applied).map_err(|e| e)?;
                    caller_t = hm::apply_subst(subst, &*rest);
                }
                _ => {
                    let fresh_ret = tg.fresh();
                    let fn_t = Type::TArrow(
                        std::sync::Arc::new(arg_t_applied),
                        std::sync::Arc::new(fresh_ret.clone()),
                    );
                    *subst = hm::unify(std::mem::take(subst), &caller_t_applied, &fn_t)
                        .map_err(|e| e)?;
                    caller_t = fresh_ret;
                }
            }
        }

        Ok(hm::apply_subst(subst, &caller_t))
    }

    fn apply_call_args(
        caller_t: Type,
        args: &[CallArg],
        reverse_args: &[Node],
        env: &mut MiddleEnvironment,
        scope: &u64,
        tg: &mut TypeGenerator,
        tenv: &mut TypeEnv,
        subst: &mut hm::Subst,
    ) -> Result<Type, String> {
        let positional = args.iter().map(|arg| match arg {
            CallArg::Value(n) => n,
            CallArg::Named(_, n) => n,
        });
        apply_call_iter(
            caller_t,
            positional.chain(reverse_args.iter()),
            env,
            scope,
            tg,
            tenv,
            subst,
        )
    }

    fn apply_call_types(
        mut caller_t: Type,
        arg_types: impl IntoIterator<Item = Type>,
        tg: &mut TypeGenerator,
        subst: &mut hm::Subst,
    ) -> Result<Type, String> {
        for arg_t in arg_types {
            let arg_t_applied = hm::apply_subst(subst, &arg_t);
            let caller_t_applied = hm::apply_subst(subst, &caller_t);
            match caller_t_applied {
                Type::TArrow(param, rest) => {
                    *subst =
                        hm::unify(std::mem::take(subst), &param, &arg_t_applied).map_err(|e| e)?;
                    caller_t = hm::apply_subst(subst, &*rest);
                }
                _ => {
                    let fresh_ret = tg.fresh();
                    let fn_t = Type::TArrow(
                        std::sync::Arc::new(arg_t_applied),
                        std::sync::Arc::new(fresh_ret.clone()),
                    );
                    *subst = hm::unify(std::mem::take(subst), &caller_t_applied, &fn_t)
                        .map_err(|e| e)?;
                    caller_t = fresh_ret;
                }
            }
        }

        Ok(hm::apply_subst(subst, &caller_t))
    }

    match &node.node_type {
        NodeType::IntLiteral(_) => Ok(Type::TCon(TypeCon::Int)),
        NodeType::FloatLiteral(_) => Ok(Type::TCon(TypeCon::Float)),
        NodeType::StringLiteral(_) => Ok(Type::TCon(TypeCon::Str)),
        NodeType::CharLiteral(_) => Ok(Type::TCon(TypeCon::Char)),
        NodeType::Null => Ok(Type::TCon(TypeCon::Null)),
        NodeType::Identifier(id) => {
            if let Some(iden) = env.resolve_potential_generic_ident(scope, id) {
                if let Some(scheme) = tenv.get(&iden.text) {
                    if scheme.vars.is_empty() {
                        return Ok(scheme.ty.clone());
                    } else {
                        return Ok(hm::instantiate(scheme, tg));
                    }
                }

                if env.objects.contains_key(&iden.text) {
                    if let Some(pd) = env.resolve_potential_generic_ident_to_data_type(scope, id) {
                        return Ok(hm::from_parser_data_type(&pd, tg));
                    }

                    return Ok(Type::TCon(TypeCon::Struct(iden.text)));
                }
            }

            let raw_name = id.to_string();
            if let Some(scheme) = tenv.get(&raw_name) {
                if scheme.vars.is_empty() {
                    return Ok(scheme.ty.clone());
                } else {
                    return Ok(hm::instantiate(scheme, tg));
                }
            }

            Ok(tg.fresh())
        }
        NodeType::ListLiteral(_, items) => {
            let mut elem_ty: Option<Type> = None;
            for it in items.iter() {
                let t = visit(it, env, scope, tg, tenv, subst)?;
                let t_applied = hm::apply_subst(subst, &t);
                if let Some(e) = elem_ty.as_ref() {
                    let e_applied = hm::apply_subst(subst, e);
                    *subst =
                        hm::unify(std::mem::take(subst), &e_applied, &t_applied).map_err(|e| e)?;
                    elem_ty = Some(hm::apply_subst(subst, &e_applied));
                } else {
                    elem_ty = Some(t_applied);
                }
            }
            Ok(Type::TList(std::sync::Arc::new(
                elem_ty.unwrap_or(tg.fresh()),
            )))
        }
        NodeType::RangeDeclaration { from, to, .. } => {
            let from_t = visit(from, env, scope, tg, tenv, subst)?;
            let to_t = visit(to, env, scope, tg, tenv, subst)?;
            let int_t = Type::TCon(TypeCon::Int);
            *subst = hm::unify(
                std::mem::take(subst),
                &hm::apply_subst(subst, &from_t),
                &int_t,
            )
            .map_err(|e| e)?;
            *subst = hm::unify(
                std::mem::take(subst),
                &hm::apply_subst(subst, &to_t),
                &int_t,
            )
            .map_err(|e| e)?;
            Ok(Type::TCon(TypeCon::Range))
        }
        NodeType::CallExpression {
            caller,
            args,
            reverse_args,
            ..
        } => {
            let caller_t = visit(caller, env, scope, tg, tenv, subst)?;
            apply_call_args(caller_t, args, reverse_args, env, scope, tg, tenv, subst)
        }
        NodeType::BinaryExpression {
            left,
            right,
            operator,
        } => {
            fn try_unify(subst: &hm::Subst, a: &Type, b: &Type) -> Option<hm::Subst> {
                hm::unify(subst.clone(), a, b).ok()
            }

            fn try_unify3(
                subst: &hm::Subst,
                a1: &Type,
                b1: &Type,
                a2: &Type,
                b2: &Type,
            ) -> Option<hm::Subst> {
                let s1 = try_unify(subst, a1, b1)?;
                try_unify(&s1, a2, b2)
            }

            fn infer_from_overloads(
                env: &mut MiddleEnvironment,
                scope: &u64,
                tg: &mut TypeGenerator,
                subst: &mut hm::Subst,
                op: &BinaryOperator,
                lt: &Type,
                rt: &Type,
            ) -> Option<Type> {
                let lt_applied = hm::apply_subst(subst, lt);
                let rt_applied = hm::apply_subst(subst, rt);

                for overload in env.overloads.iter() {
                    if overload.operator != Operator::Binary(op.clone()) {
                        continue;
                    }
                    if overload.parameters.len() != 2 {
                        continue;
                    }

                    let p0 = hm::from_parser_data_type(&overload.parameters[0], tg);
                    let p1 = hm::from_parser_data_type(&overload.parameters[1], tg);
                    let ret = hm::from_parser_data_type(&overload.return_type, tg);

                    let subst_try = try_unify3(subst, &lt_applied, &p0, &rt_applied, &p1)?;
                    *subst = subst_try;
                    return Some(hm::apply_subst(subst, &ret));
                }

                let _ = scope;
                None
            }

            fn infer_builtin(
                subst: &mut hm::Subst,
                _tg: &mut TypeGenerator,
                op: &BinaryOperator,
                lt: &Type,
                rt: &Type,
            ) -> Result<Type, String> {
                let lt_applied = hm::apply_subst(subst, lt);
                let rt_applied = hm::apply_subst(subst, rt);

                let int_t = Type::TCon(TypeCon::Int);
                let float_t = Type::TCon(TypeCon::Float);
                let bool_t = Type::TCon(TypeCon::Bool);
                let str_t = Type::TCon(TypeCon::Str);
                let char_t = Type::TCon(TypeCon::Char);

                match op {
                    BinaryOperator::Add
                    | BinaryOperator::Sub
                    | BinaryOperator::Mul
                    | BinaryOperator::Div
                    | BinaryOperator::Pow => {
                        if let Some(s2) =
                            try_unify3(subst, &lt_applied, &int_t, &rt_applied, &int_t)
                        {
                            *subst = s2;
                            return Ok(int_t);
                        }

                        if let Some(s2) =
                            try_unify3(subst, &lt_applied, &float_t, &rt_applied, &float_t)
                        {
                            *subst = s2;
                            return Ok(float_t);
                        }

                        if let Some(s2) =
                            try_unify3(subst, &lt_applied, &float_t, &rt_applied, &int_t)
                        {
                            *subst = s2;
                            return Ok(float_t);
                        }

                        if let Some(s2) =
                            try_unify3(subst, &lt_applied, &int_t, &rt_applied, &float_t)
                        {
                            *subst = s2;
                            return Ok(float_t);
                        }

                        Err(format!("cannot infer numeric operator {:?}", op))
                    }
                    BinaryOperator::Mod => {
                        *subst =
                            hm::unify(std::mem::take(subst), &lt_applied, &int_t).map_err(|e| e)?;
                        *subst =
                            hm::unify(std::mem::take(subst), &rt_applied, &int_t).map_err(|e| e)?;
                        Ok(int_t)
                    }
                    BinaryOperator::BitXor
                    | BinaryOperator::BitOr
                    | BinaryOperator::BitAnd
                    | BinaryOperator::Shl
                    | BinaryOperator::Shr => {
                        if matches!(op, BinaryOperator::BitAnd) {
                            if let Some(s2) = try_unify(subst, &lt_applied, &str_t) {
                                *subst = s2;
                                return Ok(str_t);
                            }
                            if let Some(s2) = try_unify(subst, &lt_applied, &char_t) {
                                *subst = s2;
                                return Ok(str_t);
                            }
                            if let Some(s2) = try_unify(subst, &rt_applied, &str_t) {
                                *subst = s2;
                                return Ok(str_t);
                            }
                            if let Some(s2) = try_unify(subst, &rt_applied, &char_t) {
                                *subst = s2;
                                return Ok(str_t);
                            }
                        }

                        if let Some(s2) =
                            try_unify3(subst, &lt_applied, &int_t, &rt_applied, &int_t)
                        {
                            *subst = s2;
                            return Ok(int_t);
                        }
                        if let Some(s2) =
                            try_unify3(subst, &lt_applied, &int_t, &rt_applied, &bool_t)
                        {
                            *subst = s2;
                            return Ok(int_t);
                        }
                        if let Some(s2) =
                            try_unify3(subst, &lt_applied, &bool_t, &rt_applied, &int_t)
                        {
                            *subst = s2;
                            return Ok(int_t);
                        }
                        if let Some(s2) =
                            try_unify3(subst, &lt_applied, &bool_t, &rt_applied, &bool_t)
                        {
                            *subst = s2;
                            return Ok(int_t);
                        }

                        *subst = hm::unify(std::mem::take(subst), &lt_applied, &rt_applied)
                            .map_err(|e| e)?;
                        Ok(hm::apply_subst(subst, &lt_applied))
                    }
                }
            }

            let lt = visit(left, env, scope, tg, tenv, subst)?;
            let rt = visit(right, env, scope, tg, tenv, subst)?;

            if let Some(t) = infer_from_overloads(env, scope, tg, subst, operator, &lt, &rt) {
                return Ok(t);
            }

            infer_builtin(subst, tg, operator, &lt, &rt)
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
            *subst = hm::unify(std::mem::take(subst), &lt_applied, &rt_applied).map_err(|e| e)?;
            Ok(Type::TCon(TypeCon::Bool))
        }
        NodeType::NotExpression { value } => {
            let vt = visit(value, env, scope, tg, tenv, subst)?;
            let vt_applied = hm::apply_subst(subst, &vt);
            *subst = hm::unify(
                std::mem::take(subst),
                &vt_applied,
                &Type::TCon(TypeCon::Bool),
            )
            .map_err(|e| e)?;
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
                let old_env = tenv.clone();
                let mut last = Type::TCon(TypeCon::Null);
                for s in stmts.iter() {
                    last = visit(s, env, scope, tg, tenv, subst)?;
                }
                *tenv = old_env;
                Ok(last)
            } else {
                Ok(Type::TCon(TypeCon::Null))
            }
        }
        NodeType::IfStatement {
            comparison,
            then,
            otherwise,
            ..
        } => {
            match comparison.as_ref() {
                IfComparisonType::If(cond) => {
                    let ct = visit(cond, env, scope, tg, tenv, subst)?;
                    let ct_applied = hm::apply_subst(subst, &ct);
                    *subst = hm::unify(
                        std::mem::take(subst),
                        &ct_applied,
                        &Type::TCon(TypeCon::Bool),
                    )
                    .map_err(|e| e)?;
                }
                IfComparisonType::IfLet { value, .. } => {
                    // Ensure scrutinee participates in constraints even if pattern typing is coarse.
                    let _ = visit(value, env, scope, tg, tenv, subst)?;
                }
            }

            let tt = visit(then, env, scope, tg, tenv, subst)?;
            let tt_applied = hm::apply_subst(subst, &tt);
            if let Some(o) = otherwise {
                let ot = visit(o, env, scope, tg, tenv, subst)?;
                let ot_applied = hm::apply_subst(subst, &ot);
                *subst =
                    hm::unify(std::mem::take(subst), &tt_applied, &ot_applied).map_err(|e| e)?;
                Ok(hm::apply_subst(subst, &tt_applied))
            } else {
                Ok(Type::TCon(TypeCon::Null))
            }
        }
        NodeType::ParenExpression { value } => visit(value, env, scope, tg, tenv, subst),
        NodeType::DestructureDeclaration { value, .. } => {
            let _ = visit(value, env, scope, tg, tenv, subst)?;
            Ok(Type::TCon(TypeCon::Null))
        }
        NodeType::DestructureAssignment { value, .. } => {
            let _ = visit(value, env, scope, tg, tenv, subst)?;
            Ok(Type::TCon(TypeCon::Null))
        }
        NodeType::MoveExpression { value } => visit(value, env, scope, tg, tenv, subst),
        NodeType::BooleanExpression { left, right, .. } => {
            let lt = visit(left, env, scope, tg, tenv, subst)?;
            let rt = visit(right, env, scope, tg, tenv, subst)?;
            let bool_t = Type::TCon(TypeCon::Bool);
            *subst = hm::unify(std::mem::take(subst), &hm::apply_subst(subst, &lt), &bool_t)
                .map_err(|e| e)?;
            *subst = hm::unify(std::mem::take(subst), &hm::apply_subst(subst, &rt), &bool_t)
                .map_err(|e| e)?;
            Ok(bool_t)
        }
        NodeType::AsExpression { data_type, .. } => Ok(hm::from_parser_data_type(
            &match data_type {
                PotentialNewType::DataType(dt) => dt.clone(),
                _ => ParserDataType::new(node.span, ParserInnerType::Auto(None)),
            },
            tg,
        )),
        NodeType::VariableDeclaration {
            identifier,
            value,
            data_type,
            ..
        } => {
            let val_t = visit(value, env, scope, tg, tenv, subst)?;
            let declared = match data_type {
                PotentialNewType::DataType(dt) => dt.clone(),
                _ => ParserDataType::new(node.span, ParserInnerType::Auto(None)),
            };
            let out_t = if matches!(declared.data_type, ParserInnerType::Auto(_)) {
                val_t
            } else {
                let decl_t = hm::from_parser_data_type(&declared, tg);
                *subst = hm::unify(
                    std::mem::take(subst),
                    &hm::apply_subst(subst, &decl_t),
                    &hm::apply_subst(subst, &val_t),
                )
                .map_err(|e| e)?;
                decl_t
            };
            tenv.insert(
                identifier.to_string(),
                hm::generalize(tenv, &hm::apply_subst(subst, &out_t)),
            );
            Ok(Type::TCon(TypeCon::Null))
        }
        NodeType::AssignmentExpression { identifier, value } => {
            let id_t = visit(identifier, env, scope, tg, tenv, subst)?;
            let val_t = visit(value, env, scope, tg, tenv, subst)?;
            *subst = hm::unify(
                std::mem::take(subst),
                &hm::apply_subst(subst, &id_t),
                &hm::apply_subst(subst, &val_t),
            )
            .map_err(|e| e)?;
            Ok(Type::TCon(TypeCon::Null))
        }
        NodeType::MemberExpression { path } => {
            if path.is_empty() {
                return Ok(tg.fresh());
            }

            let mut acc_t = visit(&path[0].0, env, scope, tg, tenv, subst)?;
            for (seg, is_index) in path.iter().skip(1) {
                if *is_index {
                    let idx_t = visit(seg, env, scope, tg, tenv, subst)?;
                    *subst = hm::unify(
                        std::mem::take(subst),
                        &hm::apply_subst(subst, &idx_t),
                        &Type::TCon(TypeCon::Int),
                    )
                    .map_err(|e| e)?;
                    let acc_applied = hm::apply_subst(subst, &acc_t);
                    acc_t = match acc_applied {
                        Type::TList(inner) => (*inner).clone(),
                        _ => tg.fresh(),
                    };
                    continue;
                }

                match &seg.node_type {
                    NodeType::Identifier(id) => {
                        let name = id.to_string();
                        if name == "new" {
                            continue;
                        }
                        if let Some(field_t) = resolve_member_field_type(
                            env,
                            scope,
                            &hm::apply_subst(subst, &acc_t),
                            &name,
                            seg.span,
                        ) {
                            acc_t = field_t;
                        } else if let Some(member_fn) = resolve_member_fn_type(
                            env,
                            tenv,
                            &hm::apply_subst(subst, &acc_t),
                            &name,
                            tg,
                        ) {
                            acc_t = member_fn;
                        } else {
                            acc_t = tg.fresh();
                        }
                    }
                    NodeType::CallExpression {
                        caller,
                        args,
                        reverse_args,
                        ..
                    } => {
                        if let NodeType::Identifier(id) = &caller.node_type {
                            let name = id.to_string();
                            if name == "new" {
                                continue;
                            }
                            if let Some(member_fn) = resolve_member_fn_type(
                                env,
                                tenv,
                                &hm::apply_subst(subst, &acc_t),
                                &name,
                                tg,
                            ) {
                                let mut arg_types =
                                    Vec::with_capacity(1 + args.len() + reverse_args.len());
                                arg_types.push(hm::apply_subst(subst, &acc_t));
                                for arg in args {
                                    let arg_node = match arg {
                                        CallArg::Value(n) => n,
                                        CallArg::Named(_, n) => n,
                                    };
                                    let t = visit(arg_node, env, scope, tg, tenv, subst)?;
                                    arg_types.push(t);
                                }
                                for arg in reverse_args {
                                    let t = visit(arg, env, scope, tg, tenv, subst)?;
                                    arg_types.push(t);
                                }
                                acc_t = apply_call_types(member_fn, arg_types, tg, subst)?;
                            } else {
                                acc_t = tg.fresh();
                            }
                        } else {
                            acc_t = tg.fresh();
                        }
                    }
                    _ => {
                        let _ = visit(seg, env, scope, tg, tenv, subst)?;
                        acc_t = tg.fresh();
                    }
                }
            }
            Ok(hm::apply_subst(subst, &acc_t))
        }
        NodeType::TupleLiteral { values } => {
            let mut elems = Vec::with_capacity(values.len());
            for value in values {
                elems.push(std::sync::Arc::new(visit(
                    value, env, scope, tg, tenv, subst,
                )?));
            }
            Ok(Type::TApp(hm::TypeApp::Tuple, elems))
        }
        NodeType::FunctionDeclaration { header, body, .. } => {
            fn unify_returns(
                node: &Node,
                env: &mut MiddleEnvironment,
                scope: &u64,
                tg: &mut TypeGenerator,
                tenv: &mut TypeEnv,
                subst: &mut hm::Subst,
                ret_t: &Type,
                saw_return: &mut bool,
            ) -> Result<(), String> {
                match &node.node_type {
                    NodeType::Return { value } => {
                        *saw_return = true;
                        let vt = if let Some(v) = value {
                            visit(v, env, scope, tg, tenv, subst)?
                        } else {
                            Type::TCon(TypeCon::Null)
                        };
                        let vt_applied = hm::apply_subst(subst, &vt);
                        let ret_applied = hm::apply_subst(subst, ret_t);
                        *subst = hm::unify(std::mem::take(subst), &ret_applied, &vt_applied)
                            .map_err(|e| e)?;
                        Ok(())
                    }
                    NodeType::ScopeDeclaration { body, .. } => {
                        if let Some(stmts) = body {
                            for s in stmts.iter() {
                                unify_returns(s, env, scope, tg, tenv, subst, ret_t, saw_return)?;
                            }
                        }
                        Ok(())
                    }
                    NodeType::IfStatement {
                        then, otherwise, ..
                    } => {
                        unify_returns(then, env, scope, tg, tenv, subst, ret_t, saw_return)?;
                        if let Some(o) = otherwise {
                            unify_returns(o, env, scope, tg, tenv, subst, ret_t, saw_return)?;
                        }
                        Ok(())
                    }
                    _ => Ok(()),
                }
            }

            let declared_ret_pd = match &header.return_type {
                PotentialNewType::DataType(pd) => pd.clone(),
                _ => ParserDataType::new(node.span, ParserInnerType::Auto(None)),
            };
            let declared_ret_t = if matches!(declared_ret_pd.data_type, ParserInnerType::Auto(_)) {
                tg.fresh()
            } else {
                hm::from_parser_data_type(&declared_ret_pd, tg)
            };

            let mut fn_tenv = tenv.clone();
            let mut param_types = Vec::new();
            for param in header.parameters.iter() {
                let resolved_name = env
                    .resolve_potential_dollar_ident(scope, &param.0)
                    .map(|p| p.text)
                    .unwrap_or_else(|| param.0.to_string());

                let p_pd = match &param.1 {
                    PotentialNewType::DataType(pd) => pd.clone(),
                    _ => ParserDataType::new(node.span, ParserInnerType::Auto(None)),
                };
                let p_t = if matches!(p_pd.data_type, ParserInnerType::Auto(_)) {
                    tg.fresh()
                } else {
                    hm::from_parser_data_type(&p_pd, tg)
                };
                fn_tenv.insert(
                    resolved_name.clone(),
                    hm::TypeScheme::new(Vec::new(), p_t.clone()),
                );
                param_types.push(p_t);
            }

            let body_t = visit(body, env, scope, tg, &mut fn_tenv, subst)?;

            let mut saw_return = false;
            unify_returns(
                body,
                env,
                scope,
                tg,
                &mut fn_tenv,
                subst,
                &declared_ret_t,
                &mut saw_return,
            )?;

            let body_t_applied = hm::apply_subst(subst, &body_t);
            let ret_applied = hm::apply_subst(subst, &declared_ret_t);
            *subst =
                hm::unify(std::mem::take(subst), &body_t_applied, &ret_applied).map_err(|e| e)?;

            let mut fn_t = hm::apply_subst(subst, &declared_ret_t);
            for p_t in param_types.iter().rev() {
                let p_t_applied = hm::apply_subst(subst, p_t);
                fn_t = Type::TArrow(std::sync::Arc::new(p_t_applied), std::sync::Arc::new(fn_t));
            }

            Ok(hm::apply_subst(subst, &fn_t))
        }
        _ => Ok(tg.fresh()),
    }
}

pub fn infer_node_hm(
    env: &mut MiddleEnvironment,
    scope: &u64,
    node: &Node,
) -> Option<(Type, hm::Subst)> {
    let mut tg = TypeGenerator::default();
    let mut subst = hm::Subst::default();

    let mut tenv: TypeEnv = env.hm_env.clone();

    let mut var_types: FxHashMap<String, Type> = FxHashMap::default();
    for (k, v) in env.variables.iter() {
        if !tenv.contains_key(k) {
            fn contains_auto(pd: &ParserDataType) -> bool {
                match &pd.data_type {
                    ParserInnerType::Auto(_) => true,
                    ParserInnerType::Tuple(xs) => xs.iter().any(|x| contains_auto(x)),
                    ParserInnerType::List(x) => contains_auto(x),
                    ParserInnerType::Ptr(x) => contains_auto(x),
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
                let applied = hm::apply_subst(&subst, &scheme.ty);
                *scheme = TypeScheme::new(scheme.vars.clone(), applied);
            }

            for (k, v) in env.variables.iter_mut() {
                if v.data_type.contains_auto() {
                    if let Some(scheme) = env.hm_env.get(k) {
                        v.data_type = hm::to_parser_data_type(&scheme.ty, &mut env.type_cache);
                    }
                }
            }

            for (k, orig_t) in var_types.iter() {
                let applied = hm::apply_subst(&subst, orig_t);
                if let Some(var) = env.variables.get_mut(k) {
                    if var.data_type.contains_auto() {
                        var.data_type = hm::to_parser_data_type(&applied, &mut env.type_cache);
                    }
                }

                env.hm_env
                    .insert(k.clone(), TypeScheme::new(Vec::new(), applied));
            }

            Some((hm::apply_subst(&subst, &t), subst))
        }
        Err(_) => None,
    }
}
