use core::fmt;
use std::rc::Rc;
use std::cell::RefCell;

use lfsc_syntax::ast::{AlphaTerm, AlphaTermSC, Ident};

use super::context::{LocalContext, RGCTX};

pub type Closure<'a,T> = Box<dyn Fn(RT<'a, T>) -> ResRT<'a, T> + 'a>;

pub fn mkClosure<'a, T>(body: &'a AlphaTerm<T>,
                        env: super::context::RLCTX<'a, T>,
                        gctx: RGCTX<'a, T>)
                       -> Closure<'a, T>
where T: Clone + PartialEq + fmt::Debug
{
    Box::new(move |v|
             super::nbe::eval(body,
                              LocalContext::insert(v, env.clone()),
                              gctx))
}

pub type TResult<T, K> = Result<T, TypecheckingErrors<K>>;
pub type Type<'a, T> = Value<'a, T>;
pub type RT<'a, T> = Rc<Type<'a, T>>;
// pub type RT<'a, T> = &'a Type<'a, T>;
// pub type Eval<'a, T> = TResult<(RT<'a, T>, LocalContext<'a, T>)>;
pub type ResRT<'a, T> = TResult<RT<'a, T>, T>;

pub enum Value<'a, T: Clone> {
    Pi(RT<'a, T>, Closure<'a, T>),
    Lam(Closure<'a, T>),
    Box, // Universe
    Star,
    ZT,
    Z(i32), // TODO: should in fact be unbounded
    QT,
    Q(i32, i32), // TODO: should in fact be unbounded
    Neutral(RT<'a, T>, Rc<Neutral<'a, T>>),
    Run(&'a AlphaTermSC<T>, RT<'a, T>),
}

impl<'a, T: Clone> fmt::Debug for Value<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Pi(a, _) => write!(f, "Pi: {:?}", a),
            Value::Lam(_) => write!(f, "Lam"),
            Value::Box => write!(f, "Box"),
            Value::Star => write!(f, "Star"),
            Value::ZT => write!(f, "ZT"),
            Value::Z(i) => write!(f, "Z: {}", i),
            Value::QT => write!(f, "QT"),
            Value::Q(i, j) => write!(f, "Q: {}/{}", i, j),
            Value::Neutral(_, n) => write!(f, "Neutral: "),
            Value::Run(_, t) => write!(f, "Run: {:?}", t),
        }
    }
}

impl<'a, T: Clone> PartialEq for Value<'a, T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // TODO incomplete
            // (Value::Pi(a, _), Value::Pi(b, _)) => a == b,
            // (Value::Lam(_), Value::Lam(_)) => true,
            (Value::Box, Value::Box) => true,
            (Value::Star, Value::Star) => true,
            (Value::ZT, Value::ZT) => true,
            (Value::Z(i), Value::Z(j)) => i == j,
            (Value::QT, Value::QT) => true,
            (Value::Q(i, j), Value::Q(k, l)) => i == k && j == l,
            // (Value::Neutral(_, n), Value::Neutral(_, m)) => n == m,
            // (Value::Run(_, t), Value::Run(_, u)) => t == u,
            _ => false,
        }
    }
}
pub fn as_symbolic<'a, T>(v: &Value<'a, T>)
                          -> TResult<Ident<T>, T>
where T: Clone
{
    match v {
        Value::Neutral(_, n) => match &**n {
            Neutral::Var(x) => Ok(Ident::Symbol(x.clone())),
            Neutral::DBI(x) => Ok(Ident::DBI(*x)),
            _ => Err(TypecheckingErrors::NotSymbolic),
        },
        _ => Err(TypecheckingErrors::NotSymbolic),
    }
}

#[allow(non_snake_case)]
pub fn as_Z<'a,T>(v: &Value<'a, T>) -> TResult<(), T>
where T: Clone
{
    match v {
        Value::ZT => Ok(()),
        _ => Err(TypecheckingErrors::NotZ),
    }
}

#[allow(non_snake_case)]
pub fn as_Q<'a,T>(v: & Value<'a, T>) -> TResult<(), T>
where T: Clone
{
    match v {
        Value::QT => Ok(()),
        _ => Err(TypecheckingErrors::NotZ),
    }
}

pub fn is_neutral<'a, T>(v: &Value<'a, T>) -> bool
where T: Clone
{
    match v {
        Value::Neutral(_, _) => true,
        _ => false,
    }
}

#[derive(Debug)]
pub enum TypecheckingErrors<T> {
    TypeInReadBack,
    ValueInReadBack,
    LamUsedAsType,
    HoleUsedAsType,
    NumberUsedAsType,
    NeutralUsedAsType,
    NotPi,
    NotZ,
    NotQ,
    LookupFailed(super::context::LookupErr),
    CannotInferLambda,
    CannotInferHole,

    NotFullyApplied,
    DependentTypeNotAllowed,

    ExpectedSort,
    // ExpectedSort(Value<T>),
    // we have these errors in Side-conditions.
    ExpectedNum,
    ExpectedSameNum,
    ExpectedSameInBranch,
    Mismatch(AlphaTerm<T>, AlphaTerm<T>),
    DivByZero,
    NaN,
    ReachedFail,
    NotSymbolic,
    Mark, // tried to get mark for non-symbolic variable.
}


#[derive(Debug, PartialEq)]
pub enum Neutral<'a, T>
where T: Clone
{
    Var(T),
    DBI(u32),
    Hole(RefCell<Option<RT<'a, T>>>),
    App(Rc<Neutral<'a, T>>, Normal<'a, T>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Normal<'a, T: Clone>(pub Rc<Type<'a, T>>, pub Rc<Value<'a, T>>);
