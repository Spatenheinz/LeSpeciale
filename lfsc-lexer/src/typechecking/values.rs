use core::fmt;
use std::{rc::Rc, borrow::Borrow};
use std::cell::RefCell;

use lfsc_syntax::ast::{AlphaTerm, AlphaTermSC, Ident, BuiltIn};

use super::{EnvWrapper, context::{LocalContext, GlobalContext}};

pub type Closure<'term, T> =
    Box<dyn Fn(RT<'term, T>, &GlobalContext<'term, T>, u32) -> ResRT<'term, T> + 'term>;

pub fn const_closure<T>(cons: RT<T>) -> Closure<T>
where T: PartialEq + std::fmt::Debug + Copy + BuiltIn
{
    Box::new(move |_,_,_| { Ok(cons.clone()) })
}

pub fn mk_closure<'term, T>(body: &'term AlphaTerm<T>,
                         lctx: super::context::Rlctx<'term, T>,
                        ) -> Closure<'term, T>
where T: PartialEq + std::fmt::Debug + Copy + BuiltIn
{
    Box::new(move |v, gctx, allow_dbi|{
             let lctx = LocalContext::insert(v, lctx.clone());
             let env = EnvWrapper::new(lctx, gctx, allow_dbi);
             env.eval(body)})
}

pub type TResult<T, K> = Result<T, TypecheckingErrors<K>>;
pub type Type<'term, T> = Value<'term, T>;
pub type RT<'term, T> = Rc<Type<'term, T>>;
pub type ResRT<'term, T> = TResult<RT<'term, T>, T>;

// #[derive(Clone)]
pub enum Value<'term, T: Copy + PartialEq> {
    Pi(RT<'term, T>, Closure<'term, T>),
    Lam(Closure<'term, T>),
    Box, // Universe
    Star,
    ZT,
    Z(i32), // TODO: should in fact be unbounded
    QT,
    Q(i32, i32), // TODO: should in fact be unbounded
    Neutral(RT<'term, T>, Rc<Neutral<'term, T>>),
    Run(&'term AlphaTermSC<T>, RT<'term, T>, Rc<LocalContext<'term, T>>),
    Prog(Vec<RT<'term, T>>, &'term AlphaTermSC<T>),
}

impl<'term, T: Copy + fmt::Debug + PartialEq> fmt::Debug for Value<'term, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Pi(a, _) => write!(f, "∏ {:?}", a),
            Value::Lam(_) => write!(f, "Lam"),
            Value::Box => write!(f, "Box"),
            Value::Star => write!(f, "Star"),
            Value::ZT => write!(f, "ZT"),
            Value::Z(i) => write!(f, "Z: {}", i),
            Value::QT => write!(f, "QT"),
            Value::Q(i, j) => write!(f, "Q: {}/{}", i, j),
            Value::Neutral(_ty, n) => write!(f, "{:?} : {:?}", _ty, n),
            // Value::Neutral(ty, n) => write!(f, "Neutral:\n\tty:  {:?}\n\tvar: {:?}", ty, n),
            // Value::Hole(hole) => {
            //     let hole = hole.borrow();
            //     match &*hole {
            //         Some(t) => fmt::Debug::fmt(t, f),
            //         None => write!(f, "_"),
            //     }
            // }
            Value::Run(ty, t, _) => write!(f, "^ {:?} {:?}", ty, t),
            Value::Prog(vars, t) => write!(f, "Prog: {:?}\n\t{:?}\n\t", vars, t),
        }
    }
}

impl<'term, T: Copy + PartialEq> PartialEq for Value<'term, T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // TODO incomplete
            // (Value::Pi(a, _), Value::Pi(b, _)) => a == b,
            // (Value::Lam(_), Value::Lam(_)) => true,
            (Value::Box, Value::Box) => true,
            (Value::Star, Value::Star) => true,
            (Value::ZT, Value::ZT) => true,
            (Value::QT, Value::QT) => true,
            (Value::Z(i), Value::Z(j)) => i == j,
            (Value::Q(i, j), Value::Q(k, l)) => i == k && j == l,
            (Value::Neutral(a, n), Value::Neutral(b, m)) => {
                // if let Value::Hole(hol) = b {
                //     let hol = hol.borrow();
                //     if let Some(t) = hol {
                //         return
                //     }
                // }
                **n == **m && a == b
            }
            (Value::Run(a, t,_), Value::Run(b, u,_)) => a == b && t == u,
            _ => false,
        }
    }
}

pub fn ref_compare<'term, T>(a: RT<'term, T>, b: RT<'term, T>) -> bool
where T: Copy + PartialEq
{
    match (&*a, &*b) {
        (Value::Box, Value::Box) => true,
        (Value::Star, Value::Star) => true,
        (Value::ZT, Value::ZT) => true,
        (Value::QT, Value::QT) => true,
        (Value::Z(i), Value::Z(j)) => i == j,
        (Value::Q(i, j), Value::Q(k, l)) => i == k && j == l,
        (Value::Neutral(a, n), Value::Neutral(b, m)) => {
            ref_compare_neutral(n.clone(), m.clone()) && a == b
        },
        _ => false,
    }
}

fn ref_compare_neutral<'term, T>(n: Rc<Neutral<'term, T>>, m: Rc<Neutral<'term, T>>) -> bool
where T: Copy + PartialEq
{
    match (&*n, &*m) {
        (Neutral::Var(i), Neutral::Var(j)) => i == j,
        (Neutral::DBI(i), Neutral::DBI(j)) => i == j,
        (Neutral::App(a, b), Neutral::App(c, d)) => {
            ref_compare_neutral(a.clone(), c.clone()) && ref_compare_normal(b, d)
        },
        //TODO what about a hole hole combination?
        (Neutral::Hole(h1), Neutral::Hole(h2)) => {
            if let Some(t1) = &*h1.borrow() {
                if let Some(t2) = &*h2.borrow() {
                    // TODO: eq check vs ref compare
                    return t1 == t2
                    // return ref_compare_neutral(t1.clone(), t2.clone())
                }
                h2.replace(Some(t1.clone()));
                return true
            }
            if let Some(t2) = &*h2.borrow() {
                if let Some(t1) = &*h1.borrow() {
                    // TODO: eq check vs ref compare
                    return t1 == t2
                    // return ref_compare_neutral(t1.clone(), t2.clone())
                }
                h1.replace(Some(t2.clone()));
                return true
            }
            false
        },
        (Neutral::Hole(hol), a) => {
            if let Some(t) = &*hol.borrow() {
                return a == &**t
            }
                hol.replace(Some(m));
                true
        },
        (a, Neutral::Hole(hol)) => {
            if let Some(t) = &*hol.borrow() {
                return a == &**t
            }
                hol.replace(Some(n));
                true
        },
        _ => false,
    }
}
fn ref_compare_normal<'term, T>(n: &Normal<'term, T>, m: &Normal<'term, T>) -> bool
where T: Copy + PartialEq
{
    ref_compare(n.0.clone(), m.0.clone()) && ref_compare(n.1.clone(), m.1.clone())
}

#[inline(always)]
pub fn mk_neutral_var_with_type<T>(typ: RT<T>) -> RT<T>
where T: Copy + PartialEq
{
    Rc::new(Value::Neutral(typ, Rc::new(Neutral::DBI(0))))
}

pub fn is_type_or_datatype<T>(v: &Value<T>) -> TResult<(),T>
where T: Copy + PartialEq
{
    if Type::Star == *v {
        return Ok(());
    }
    is_datatype(v)
}

pub fn is_datatype<T>(v: &Value<T>) -> TResult<(), T>
where T: Copy + PartialEq
{
    if let Ok(..) = as_Z(v) {
        return Ok(());
    }
    if let Ok(..) = as_Q(v) {
        return Ok(());
    }
    if let Ok(..) = as_symbolic(v) {
        return Ok(());
    }
    Err(TypecheckingErrors::NotDatatype)
}

pub fn as_symbolic<T>(v: &Value<T>)
                          -> TResult<Ident<T>, T>
where T: Copy + PartialEq
{
    match v {
        Value::Neutral(_, n) => match &**n {
            Neutral::Var(x) => Ok(Ident::Symbol(*x)),
            Neutral::DBI(x) => Ok(Ident::DBI(*x)),
            _ => Err(TypecheckingErrors::NotSymbolic),
        },
        _ => Err(TypecheckingErrors::NotSymbolic),
    }
}

#[allow(non_snake_case)]
pub fn as_Z<T>(v: &Value<T>) -> TResult<(), T>
where T: Copy + PartialEq
{
    match v {
        Value::ZT => Ok(()),
        _ => Err(TypecheckingErrors::NotZ),
    }
}

#[allow(non_snake_case)]
pub fn as_Q<T>(v: & Value<T>) -> TResult<(), T>
where T: Copy + PartialEq
{
    match v {
        Value::QT => Ok(()),
        _ => Err(TypecheckingErrors::NotZ),
    }
}
pub fn as_neutral<'term, T>(v: &Value<'term, T>)
                          -> TResult<Rc<Neutral<'term, T>>, T>
where T: Copy + PartialEq
{
    match v {
        Value::Neutral(_, n) => Ok(n.clone()),
        _ => Err(TypecheckingErrors::NotSymbolic),
    }
}

pub fn is_neutral<T>(v: &Value<T>) -> bool
where T: Copy + PartialEq
{
    matches!(v, Value::Neutral(_, _))
}

#[derive(Debug)]
pub enum TypecheckingErrors<T>
where T: Copy
{
    //Readback errors
    ValueUsedAsType,
    ReadBackMismatch,
    // ReadBackMismatch(RT<'ctx, T>, RT<'ctx, T>),
    WrongNumberOfArguments,

    SymbolAlreadyDefined(T),

    NotPi,
    NotZ,
    NotQ,
    LookupFailed(super::context::LookupErr),
    CannotInferLambda,
    CannotInferHole,

    UnexpectedSC,
    KindLevelDefinition,

    NotFullyApplied,
    DependentTypeNotAllowed,

    NotDatatype,
    ExpectedSort,
    // ExpectedSort(Value<T>),
    // we have these errors in Side-conditions.
    ExpectedNum,
    ExpectedSameNum,
    Mismatch(AlphaTerm<T>, AlphaTerm<T>),
    DivByZero,
    NaN,
    ReachedFail,
    NotSymbolic,
    NoMatch,
    Mark, // tried to get mark for non-symbolic variable.
    // For hacking holes
    ReadHole,

    ValAsType,
}


#[derive(Debug, Clone, PartialEq)]
pub enum Neutral<'term, T: Copy + PartialEq>
{
    Var(T),
    DBI(u32),
    App(Rc<Neutral<'term, T>>, Normal<'term, T>),
    Hole(RefCell<Option<Rc<Neutral<'term, T>>>>),
    // SC
}

pub fn flatten<'term, T: Copy + PartialEq>(neu: &Neutral<'term, T>) -> TResult<(Ident<T>, Vec<RT<'term, T>>), T> {
    use Neutral::*;
    match neu {
        Var(c) => Ok((Ident::Symbol(*c), vec![])),
        DBI(i) => Ok((Ident::DBI(*i), vec![])),
        App(f, a) => {
            let (f, mut args) = flatten(f)?;
            args.push(a.1.clone());
            Ok((f, args))
        },
        Hole(inner) => {
            if let Some(n) = &*inner.borrow() {
                flatten(n)
            } else {
                Err(TypecheckingErrors::NotSymbolic)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Normal<'term, T: Copy + PartialEq>(pub Rc<Type<'term, T>>, pub Rc<Value<'term, T>>);
