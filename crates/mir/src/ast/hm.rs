use calibre_parser::{
    Span,
    ast::{ParserDataType, ParserInnerType},
};
use rustc_hash::{FxHashMap, FxHashSet};
use std::{fmt, sync::Arc};

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum TypeCon {
    Int,
    UInt,
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
    StructWithGenerics(String),
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::TVar(n) => write!(f, "t{}", n),
            Type::TCon(tc) => match tc {
                TypeCon::Int => write!(f, "Int"),
                TypeCon::UInt => write!(f, "UInt"),
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

pub fn from_parser_data_type(pd: &ParserDataType, tg: &mut TypeGenerator) -> Type {
    match &pd.data_type {
        ParserInnerType::Float => Type::TCon(TypeCon::Float),
        ParserInnerType::Int => Type::TCon(TypeCon::Int),
        ParserInnerType::UInt => Type::TCon(TypeCon::UInt),
        ParserInnerType::Null => Type::TCon(TypeCon::Null),
        ParserInnerType::Bool => Type::TCon(TypeCon::Bool),
        ParserInnerType::Str => Type::TCon(TypeCon::Str),
        ParserInnerType::Char => Type::TCon(TypeCon::Char),
        ParserInnerType::Dynamic => Type::TCon(TypeCon::Dyn),
        ParserInnerType::List(x) => Type::TList(Arc::new(from_parser_data_type(x, tg))),
        ParserInnerType::Ptr(x) => {
            Type::TApp(TypeApp::Ptr, vec![Arc::new(from_parser_data_type(x, tg))])
        }
        ParserInnerType::Tuple(types) => Type::TApp(
            TypeApp::Tuple,
            types
                .iter()
                .map(|t| Arc::new(from_parser_data_type(t, tg)))
                .collect(),
        ),
        ParserInnerType::Option(x) => Type::TApp(
            TypeApp::Option,
            vec![Arc::new(from_parser_data_type(x, tg))],
        ),
        ParserInnerType::Result { ok, err } => Type::TApp(
            TypeApp::Result,
            vec![
                Arc::new(from_parser_data_type(ok, tg)),
                Arc::new(from_parser_data_type(err, tg)),
            ],
        ),
        ParserInnerType::Function {
            return_type,
            parameters,
        } => {
            let mut arrow = from_parser_data_type(return_type, tg);
            for p in parameters.iter().rev() {
                let pt = from_parser_data_type(p, tg);
                arrow = Type::TArrow(Arc::new(pt), Arc::new(arrow));
            }
            arrow
        }
        ParserInnerType::Auto(_) => tg.fresh(),
        ParserInnerType::Ref(inner, _mut) => from_parser_data_type(inner, tg),
        ParserInnerType::Struct(name) => Type::TCon(TypeCon::Struct(name.clone())),
        ParserInnerType::StructWithGenerics {
            identifier,
            generic_types,
        } => Type::TApp(
            TypeApp::StructWithGenerics(identifier.clone()),
            generic_types
                .iter()
                .map(|g| Arc::new(from_parser_data_type(g, tg)))
                .collect(),
        ),
        ParserInnerType::NativeFunction(x) => {
            let inner = from_parser_data_type(x, tg);
            Type::TApp(TypeApp::NativeFn, vec![Arc::new(inner)])
        }
        ParserInnerType::Scope(vals) => Type::TApp(
            TypeApp::Scope,
            vals.iter()
                .map(|v| Arc::new(from_parser_data_type(v, tg)))
                .collect(),
        ),
        ParserInnerType::DollarIdentifier(_) => tg.fresh(),
        ParserInnerType::Range => Type::TCon(TypeCon::Range),
    }
}

pub fn to_parser_data_type(
    ty: &Type,
    cache: &mut FxHashMap<Type, ParserDataType>,
) -> ParserDataType {
    if let Some(pd) = cache.get(ty) {
        return pd.clone();
    }

    let res = match ty {
        Type::TVar(_) => ParserDataType::new(Span::default(), ParserInnerType::Auto(None)),
        Type::TCon(tc) => match tc {
            TypeCon::Int => ParserDataType::new(Span::default(), ParserInnerType::Int),
            TypeCon::UInt => ParserDataType::new(Span::default(), ParserInnerType::UInt),
            TypeCon::Float => ParserDataType::new(Span::default(), ParserInnerType::Float),
            TypeCon::Bool => ParserDataType::new(Span::default(), ParserInnerType::Bool),
            TypeCon::Str => ParserDataType::new(Span::default(), ParserInnerType::Str),
            TypeCon::Char => ParserDataType::new(Span::default(), ParserInnerType::Char),
            TypeCon::Null => ParserDataType::new(Span::default(), ParserInnerType::Null),
            TypeCon::Dyn => ParserDataType::new(Span::default(), ParserInnerType::Dynamic),
            TypeCon::Range => ParserDataType::new(Span::default(), ParserInnerType::Range),
            TypeCon::Struct(s) => {
                ParserDataType::new(Span::default(), ParserInnerType::Struct(s.clone()))
            }
        },
        Type::TList(inner) => ParserDataType::new(
            Span::default(),
            ParserInnerType::List(Box::new(to_parser_data_type(inner.as_ref(), cache))),
        ),
        Type::TApp(name, args) => match name {
            TypeApp::Tuple => ParserDataType::new(
                Span::default(),
                ParserInnerType::Tuple(
                    args.iter()
                        .map(|a| to_parser_data_type(a.as_ref(), cache))
                        .collect(),
                ),
            ),
            TypeApp::Option => ParserDataType::new(
                Span::default(),
                ParserInnerType::Option(Box::new(to_parser_data_type(args[0].as_ref(), cache))),
            ),
            TypeApp::Ptr => ParserDataType::new(
                Span::default(),
                ParserInnerType::Ptr(Box::new(to_parser_data_type(args[0].as_ref(), cache))),
            ),
            TypeApp::Result => ParserDataType::new(
                Span::default(),
                ParserInnerType::Result {
                    ok: Box::new(to_parser_data_type(args[0].as_ref(), cache)),
                    err: Box::new(to_parser_data_type(args[1].as_ref(), cache)),
                },
            ),
            TypeApp::NativeFn => ParserDataType::new(
                Span::default(),
                ParserInnerType::NativeFunction(Box::new(to_parser_data_type(
                    args[0].as_ref(),
                    cache,
                ))),
            ),
            TypeApp::Scope => ParserDataType::new(
                Span::default(),
                ParserInnerType::Scope(
                    args.iter()
                        .map(|a| to_parser_data_type(a.as_ref(), cache))
                        .collect(),
                ),
            ),
            TypeApp::StructWithGenerics(id) => ParserDataType::new(
                Span::default(),
                ParserInnerType::StructWithGenerics {
                    identifier: id.clone(),
                    generic_types: args
                        .iter()
                        .map(|a| to_parser_data_type(a.as_ref(), cache))
                        .collect(),
                },
            ),
        },
        Type::TArrow(_, _) => {
            let mut params = Vec::new();
            let mut cur = ty;
            let mut ret = None;
            while let Type::TArrow(l, r) = cur {
                params.push(to_parser_data_type(l.as_ref(), cache));
                ret = Some(to_parser_data_type(r.as_ref(), cache));
                cur = r.as_ref();
            }
            ParserDataType::new(
                Span::default(),
                ParserInnerType::Function {
                    return_type: Box::new(
                        ret.unwrap_or(ParserDataType::new(Span::default(), ParserInnerType::Null)),
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
