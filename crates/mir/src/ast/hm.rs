use calibre_parser::{
    Span,
    ast::{ParserDataType, ParserInnerType, RefMutability},
};
use rustc_hash::{FxHashMap, FxHashSet};
use std::{fmt, sync::Arc};

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum TypeCon {
    Int,
    UInt,
    Byte,
    Float,
    Bool,
    Str,
    Char,
    Null,
    Dyn,
    Range,
    Struct(String),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
    TVar(usize),
    TCon(TypeCon),
    TApp(TypeApp, Vec<Arc<Type>>),
    TArrow(Arc<Type>, Arc<Type>),
    TList(Arc<Type>),
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum TypeApp {
    Tuple,
    Option,
    Result,
    NativeFn,
    Scope,
    Ptr,
    Ref(RefMutability),
    DynamicTraits(Vec<String>),
    NamedGeneric(String),
    StructWithGenerics(String),
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::TVar(n) => write!(f, "t{}", n),
            Type::TCon(tc) => match tc {
                TypeCon::Int => write!(f, "Int"),
                TypeCon::UInt => write!(f, "UInt"),
                TypeCon::Byte => write!(f, "Byte"),
                TypeCon::Float => write!(f, "Float"),
                TypeCon::Bool => write!(f, "Bool"),
                TypeCon::Str => write!(f, "Str"),
                TypeCon::Char => write!(f, "Char"),
                TypeCon::Null => write!(f, "Null"),
                TypeCon::Dyn => write!(f, "Dyn"),
                TypeCon::Range => write!(f, "Range"),
                TypeCon::Struct(s) => write!(f, "{}", s),
            },
            Type::TArrow(a, b) => write!(f, "({:?} -> {:?})", a, b),
            Type::TList(t) => write!(f, "[{:?}]", t),
            Type::TApp(name, args) => {
                let name_str = match name {
                    TypeApp::Tuple => "Tuple".to_string(),
                    TypeApp::Option => "Option".to_string(),
                    TypeApp::Result => "Result".to_string(),
                    TypeApp::NativeFn => "NativeFn".to_string(),
                    TypeApp::Scope => "Scope".to_string(),
                    TypeApp::Ptr => "Ptr".to_string(),
                    TypeApp::Ref(RefMutability::Ref) => "Ref".to_string(),
                    TypeApp::Ref(RefMutability::Value) => "ValueRef".to_string(),
                    TypeApp::Ref(RefMutability::MutRef) => "MutRef".to_string(),
                    TypeApp::Ref(RefMutability::MutValue) => "MutValue".to_string(),
                    TypeApp::DynamicTraits(ids) => format!("Dyn<{}>", ids.join("+")),
                    TypeApp::NamedGeneric(id) => id.clone(),
                    TypeApp::StructWithGenerics(id) => id.clone(),
                };

                if args.is_empty() {
                    write!(f, "{}", name_str)
                } else {
                    write!(f, "{}<", name_str)?;
                    let mut first = true;
                    for a in args.iter() {
                        if !first {
                            write!(f, ", ")?;
                        }
                        first = false;
                        write!(f, "{:?}", a)?;
                    }
                    write!(f, ">")
                }
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeScheme {
    pub vars: Vec<usize>,
    pub ty: Type,
}

impl TypeScheme {
    pub fn new(vars: Vec<usize>, ty: Type) -> Self {
        Self { vars, ty }
    }
}

pub type Subst = FxHashMap<usize, Type>;

pub struct TypeGenerator {
    pub next: usize,
}

impl Default for TypeGenerator {
    fn default() -> Self {
        Self { next: 1024 }
    }
}

impl TypeGenerator {
    pub fn fresh(&mut self) -> Type {
        let id = self.next;
        self.next += 1;
        Type::TVar(id)
    }
}

fn ftv(ty: &Type, acc: &mut FxHashSet<usize>) {
    match ty {
        Type::TVar(n) => {
            acc.insert(*n);
        }
        Type::TCon(_) => {}
        Type::TArrow(a, b) => {
            ftv(a.as_ref(), acc);
            ftv(b.as_ref(), acc);
        }
        Type::TList(t) => ftv(t.as_ref(), acc),
        Type::TApp(_name, args) => {
            for a in args.iter() {
                ftv(a.as_ref(), acc);
            }
        }
    }
}

fn ftv_scheme(s: &TypeScheme) -> FxHashSet<usize> {
    let mut sset = FxHashSet::default();
    ftv(&s.ty, &mut sset);
    for v in &s.vars {
        sset.remove(v);
    }
    sset
}

pub fn apply_subst(subst: &Subst, ty: &Type) -> Type {
    match ty {
        Type::TVar(n) => {
            let mut visited: Vec<usize> = Vec::with_capacity(4);
            let mut current = *n;
            visited.push(current);

            loop {
                if let Some(t) = subst.get(&current) {
                    match t {
                        Type::TVar(m) => {
                            if visited.iter().any(|v| v == m) {
                                return Type::TVar(*n);
                            }
                            visited.push(*m);
                            current = *m;
                            continue;
                        }
                        _ => return apply_subst(subst, t),
                    }
                }
                return Type::TVar(*n);
            }
        }
        Type::TCon(_) => ty.clone(),
        Type::TArrow(a, b) => Type::TArrow(
            Arc::new(apply_subst(subst, a.as_ref())),
            Arc::new(apply_subst(subst, b.as_ref())),
        ),
        Type::TList(t) => Type::TList(Arc::new(apply_subst(subst, t.as_ref()))),
        Type::TApp(name, args) => {
            let newargs = args
                .iter()
                .map(|a| Arc::new(apply_subst(subst, a.as_ref())))
                .collect();
            Type::TApp(name.clone(), newargs)
        }
    }
}

fn occurs_check(v: usize, ty: &Type, subst: &Subst) -> bool {
    let t = apply_subst(subst, ty);
    let mut s = FxHashSet::default();
    ftv(&t, &mut s);
    s.contains(&v)
}

pub fn unify(mut subst: Subst, t1: &Type, t2: &Type) -> Result<Subst, String> {
    let a = apply_subst(&subst, t1);
    let b = apply_subst(&subst, t2);

    match (a, b) {
        (Type::TArrow(a1, b1), Type::TArrow(a2, b2)) => {
            let s1 = unify(subst.clone(), a1.as_ref(), a2.as_ref())?;
            let s2 = unify(
                s1.clone(),
                &apply_subst(&s1, b1.as_ref()),
                &apply_subst(&s1, b2.as_ref()),
            )?;
            Ok(s2)
        }
        (Type::TApp(n1, a1), Type::TApp(n2, a2)) if n1 == n2 && a1.len() == a2.len() => {
            let mut s = subst.clone();
            for (x, y) in a1.iter().zip(a2.iter()) {
                s = unify(s, x.as_ref(), y.as_ref())?;
            }
            Ok(s)
        }
        (Type::TList(x), Type::TList(y)) => unify(subst, x.as_ref(), y.as_ref()),
        (Type::TVar(n), t) | (t, Type::TVar(n)) => {
            if t == Type::TVar(n) {
                return Ok(subst);
            }
            if occurs_check(n, &t, &subst) {
                return Err(format!("occurs check failed: t{} occurs in {:?}", n, t));
            }
            subst.insert(n, t.clone());
            Ok(subst)
        }
        (Type::TCon(a), Type::TCon(b)) => {
            if a == b {
                Ok(subst)
            } else {
                Err(format!("type mismatch: {:?} vs {:?}", a, b))
            }
        }
        (a, b) => Err(format!("cannot unify {:?} with {:?}", a, b)),
    }
}

pub type TypeEnv = FxHashMap<String, TypeScheme>;

pub fn generalize(env: &TypeEnv, ty: &Type) -> TypeScheme {
    let mut env_ftv = FxHashSet::default();
    for (_k, scheme) in env.iter() {
        for v in ftv_scheme(scheme) {
            env_ftv.insert(v);
        }
    }

    let mut ty_ftv = FxHashSet::default();
    ftv(ty, &mut ty_ftv);

    let vars: Vec<usize> = ty_ftv.difference(&env_ftv).cloned().collect();
    TypeScheme::new(vars, ty.clone())
}

pub fn instantiate(scheme: &TypeScheme, tg: &mut TypeGenerator) -> Type {
    let mut subst = Subst::default();
    for v in scheme.vars.iter() {
        let fresh = tg.fresh();
        if let Type::TVar(id) = fresh {
            subst.insert(*v, Type::TVar(id));
        }
    }
    apply_subst(&subst, &scheme.ty)
}

pub fn try_from_parser_data_type(
    pd: &ParserDataType,
    tg: &mut TypeGenerator,
) -> Result<Type, String> {
    match &pd.data_type {
        ParserInnerType::Float => Ok(Type::TCon(TypeCon::Float)),
        ParserInnerType::Int => Ok(Type::TCon(TypeCon::Int)),
        ParserInnerType::UInt => Ok(Type::TCon(TypeCon::UInt)),
        ParserInnerType::Byte => Ok(Type::TCon(TypeCon::Byte)),
        ParserInnerType::Null => Ok(Type::TCon(TypeCon::Null)),
        ParserInnerType::Bool => Ok(Type::TCon(TypeCon::Bool)),
        ParserInnerType::Str => Ok(Type::TCon(TypeCon::Str)),
        ParserInnerType::Char => Ok(Type::TCon(TypeCon::Char)),
        ParserInnerType::Dynamic => Ok(Type::TCon(TypeCon::Dyn)),
        ParserInnerType::DynamicTraits(ids) => {
            Ok(Type::TApp(TypeApp::DynamicTraits(ids.clone()), Vec::new()))
        }
        ParserInnerType::List(x) => Ok(Type::TList(Arc::new(try_from_parser_data_type(x, tg)?))),
        ParserInnerType::Ptr(x) => Ok(Type::TApp(
            TypeApp::Ptr,
            vec![Arc::new(try_from_parser_data_type(x, tg)?)],
        )),
        ParserInnerType::Tuple(types) => Ok(Type::TApp(
            TypeApp::Tuple,
            types
                .iter()
                .map(|t| try_from_parser_data_type(t, tg).map(Arc::new))
                .collect::<Result<Vec<_>, _>>()?,
        )),
        ParserInnerType::Option(x) => Ok(Type::TApp(
            TypeApp::Option,
            vec![Arc::new(try_from_parser_data_type(x, tg)?)],
        )),
        ParserInnerType::Result { ok, err } => Ok(Type::TApp(
            TypeApp::Result,
            vec![
                Arc::new(try_from_parser_data_type(ok, tg)?),
                Arc::new(try_from_parser_data_type(err, tg)?),
            ],
        )),
        ParserInnerType::Function {
            return_type,
            parameters,
        } => {
            let mut arrow = try_from_parser_data_type(return_type, tg)?;
            for p in parameters.iter().rev() {
                let pt = try_from_parser_data_type(p, tg)?;
                arrow = Type::TArrow(Arc::new(pt), Arc::new(arrow));
            }
            Ok(arrow)
        }
        ParserInnerType::Auto(_) => Ok(tg.fresh()),
        ParserInnerType::Ref(inner, mutability) => Ok(Type::TApp(
            TypeApp::Ref(*mutability),
            vec![Arc::new(try_from_parser_data_type(inner, tg)?)],
        )),
        ParserInnerType::Struct(name) => Ok(Type::TCon(TypeCon::Struct(name.clone()))),
        ParserInnerType::StructWithGenerics {
            identifier,
            generic_types,
        } => Ok(Type::TApp(
            TypeApp::StructWithGenerics(identifier.clone()),
            generic_types
                .iter()
                .map(|g| try_from_parser_data_type(g, tg).map(Arc::new))
                .collect::<Result<Vec<_>, _>>()?,
        )),
        ParserInnerType::NativeFunction(x) => {
            let inner = try_from_parser_data_type(x, tg)?;
            Ok(Type::TApp(TypeApp::NativeFn, vec![Arc::new(inner)]))
        }
        ParserInnerType::Scope(vals) => Ok(Type::TApp(
            TypeApp::Scope,
            vals.iter()
                .map(|v| try_from_parser_data_type(v, tg).map(Arc::new))
                .collect::<Result<Vec<_>, _>>()?,
        )),
        ParserInnerType::DollarIdentifier(name) => {
            Ok(Type::TApp(TypeApp::NamedGeneric(name.clone()), Vec::new()))
        }
        ParserInnerType::Range => Ok(Type::TCon(TypeCon::Range)),
        ParserInnerType::FfiType(ffi) => {
            try_from_parser_data_type(&ParserDataType::new(pd.span, ffi.clone().into()), tg)
        }
    }
}

pub fn from_parser_data_type(pd: &ParserDataType, tg: &mut TypeGenerator) -> Type {
    try_from_parser_data_type(pd, tg).unwrap_or_else(|_| tg.fresh())
}

pub fn to_parser_data_type(
    ty: &Type,
    cache: &mut FxHashMap<Type, ParserDataType>,
) -> ParserDataType {
    to_parser_data_type_with_span(ty, cache, Span::default())
}

pub fn to_parser_data_type_with_span(
    ty: &Type,
    cache: &mut FxHashMap<Type, ParserDataType>,
    default_span: Span,
) -> ParserDataType {
    if let Some(pd) = cache.get(ty) {
        return pd.clone();
    }

    let res = match ty {
        Type::TVar(_) => ParserDataType::new(default_span, ParserInnerType::Auto(None)),
        Type::TCon(tc) => match tc {
            TypeCon::Int => ParserDataType::new(default_span, ParserInnerType::Int),
            TypeCon::UInt => ParserDataType::new(default_span, ParserInnerType::UInt),
            TypeCon::Byte => ParserDataType::new(default_span, ParserInnerType::Byte),
            TypeCon::Float => ParserDataType::new(default_span, ParserInnerType::Float),
            TypeCon::Bool => ParserDataType::new(default_span, ParserInnerType::Bool),
            TypeCon::Str => ParserDataType::new(default_span, ParserInnerType::Str),
            TypeCon::Char => ParserDataType::new(default_span, ParserInnerType::Char),
            TypeCon::Null => ParserDataType::new(default_span, ParserInnerType::Null),
            TypeCon::Dyn => ParserDataType::new(default_span, ParserInnerType::Dynamic),
            TypeCon::Range => ParserDataType::new(default_span, ParserInnerType::Range),
            TypeCon::Struct(s) => {
                ParserDataType::new(default_span, ParserInnerType::Struct(s.clone()))
            }
        },
        Type::TList(inner) => ParserDataType::new(
            default_span,
            ParserInnerType::List(Box::new(to_parser_data_type_with_span(
                inner.as_ref(),
                cache,
                default_span,
            ))),
        ),
        Type::TApp(name, args) => match name {
            TypeApp::Tuple => ParserDataType::new(
                default_span,
                ParserInnerType::Tuple(
                    args.iter()
                        .map(|a| to_parser_data_type_with_span(a.as_ref(), cache, default_span))
                        .collect(),
                ),
            ),
            TypeApp::Option => ParserDataType::new(
                default_span,
                ParserInnerType::Option(Box::new(to_parser_data_type_with_span(
                    args[0].as_ref(),
                    cache,
                    default_span,
                ))),
            ),
            TypeApp::Ptr => ParserDataType::new(
                default_span,
                ParserInnerType::Ptr(Box::new(to_parser_data_type_with_span(
                    args[0].as_ref(),
                    cache,
                    default_span,
                ))),
            ),
            TypeApp::Result => ParserDataType::new(
                default_span,
                ParserInnerType::Result {
                    ok: Box::new(to_parser_data_type_with_span(
                        args[0].as_ref(),
                        cache,
                        default_span,
                    )),
                    err: Box::new(to_parser_data_type_with_span(
                        args[1].as_ref(),
                        cache,
                        default_span,
                    )),
                },
            ),
            TypeApp::NativeFn => ParserDataType::new(
                default_span,
                ParserInnerType::NativeFunction(Box::new(to_parser_data_type_with_span(
                    args[0].as_ref(),
                    cache,
                    default_span,
                ))),
            ),
            TypeApp::Scope => ParserDataType::new(
                default_span,
                ParserInnerType::Scope(
                    args.iter()
                        .map(|a| to_parser_data_type_with_span(a.as_ref(), cache, default_span))
                        .collect(),
                ),
            ),
            TypeApp::Ref(mutability) => ParserDataType::new(
                default_span,
                ParserInnerType::Ref(
                    Box::new(to_parser_data_type_with_span(
                        args[0].as_ref(),
                        cache,
                        default_span,
                    )),
                    *mutability,
                ),
            ),
            TypeApp::DynamicTraits(ids) => {
                ParserDataType::new(default_span, ParserInnerType::DynamicTraits(ids.clone()))
            }
            TypeApp::NamedGeneric(id) => {
                ParserDataType::new(default_span, ParserInnerType::DollarIdentifier(id.clone()))
            }
            TypeApp::StructWithGenerics(id) => ParserDataType::new(
                default_span,
                ParserInnerType::StructWithGenerics {
                    identifier: id.clone(),
                    generic_types: args
                        .iter()
                        .map(|a| to_parser_data_type_with_span(a.as_ref(), cache, default_span))
                        .collect(),
                },
            ),
        },
        Type::TArrow(_, _) => {
            let mut params = Vec::new();
            let mut cur = ty;
            let mut ret = None;
            while let Type::TArrow(l, r) = cur {
                params.push(to_parser_data_type_with_span(
                    l.as_ref(),
                    cache,
                    default_span,
                ));
                ret = Some(to_parser_data_type_with_span(
                    r.as_ref(),
                    cache,
                    default_span,
                ));
                cur = r.as_ref();
            }
            ParserDataType::new(
                default_span,
                ParserInnerType::Function {
                    return_type: Box::new(
                        ret.unwrap_or(ParserDataType::new(default_span, ParserInnerType::Null)),
                    ),
                    parameters: params,
                },
            )
        }
    };

    cache.insert(ty.clone(), res.clone());
    res
}

pub fn recompute_scheme_vars_all(env: &mut TypeEnv) {
    let mut ty_ftvs: FxHashMap<String, FxHashSet<usize>> = FxHashMap::default();

    for (k, s) in env.iter() {
        let mut sset = FxHashSet::default();
        ftv(&s.ty, &mut sset);
        ty_ftvs.insert(k.clone(), sset);
    }

    for (k, s) in env.iter_mut() {
        let mut others = FxHashSet::default();
        for (k2, set) in ty_ftvs.iter() {
            if k2 != k {
                for v in set.iter() {
                    others.insert(*v);
                }
            }
        }

        let mut new_vars: Vec<usize> = Vec::new();
        if let Some(myset) = ty_ftvs.get(k) {
            for v in myset.iter() {
                if !others.contains(v) {
                    new_vars.push(*v);
                }
            }
        }

        s.vars = new_vars;
    }
}

pub fn normalize_env(env: &mut TypeEnv) {
    let mut counts: FxHashMap<usize, usize> = FxHashMap::default();
    let mut ty_ftvs: FxHashMap<String, FxHashSet<usize>> = FxHashMap::default();

    for (key, scheme) in env.iter() {
        let mut vars = FxHashSet::default();
        ftv(&scheme.ty, &mut vars);
        for var in vars.iter() {
            *counts.entry(*var).or_insert(0) += 1;
        }
        ty_ftvs.insert(key.clone(), vars);
    }

    for (key, scheme) in env.iter_mut() {
        let vars = ty_ftvs.get(key).cloned().unwrap_or_default();
        let mut quantified: Vec<usize> = vars
            .into_iter()
            .filter(|var| counts.get(var).copied().unwrap_or(0) <= 1)
            .collect();
        quantified.sort_unstable();
        scheme.vars = quantified;
    }
}

pub fn apply_subst_env(env: &mut TypeEnv, subst: &Subst) {
    for scheme in env.values_mut() {
        scheme.ty = apply_subst(subst, &scheme.ty);
    }
    normalize_env(env);
}
