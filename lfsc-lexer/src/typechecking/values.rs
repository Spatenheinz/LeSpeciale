use core::fmt;
use std::rc::Rc;
use std::cell::RefCell;

use lfsc_syntax::ast::{AlphaTerm, AlphaTermSC, Ident, BuiltIn};

use super::{EnvWrapper, context::LocalContext};

pub type Closure<'a,T> =
    Box<dyn Fn(RT<'a, T>, &EnvWrapper<'a, T>) -> ResRT<'a, T> + 'a>;

pub fn mk_closure<'a, T>(body: &'a AlphaTerm<T>,
                         lctx: super::context::Rlctx<'a, T>,
                        ) -> Closure<'a, T>
where T: PartialEq + std::fmt::Debug + Copy + BuiltIn
{
    Box::new(move |v, env|{
             let lctx = LocalContext::insert(v, lctx.clone());
             let env = EnvWrapper::new(lctx, env.gctx.clone(), env.allow_dependent);
             env.eval(body)})
}

pub type TResult<T, K> = Result<T, TypecheckingErrors<K>>;
pub type Type<'a, T> = Value<'a, T>;
pub type RT<'a, T> = Rc<Type<'a, T>>;
pub type ResRT<'a, T> = TResult<RT<'a, T>, T>;

// #[derive(Clone)]
pub enum Value<'a, T: Copy> {
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

// fn level_print(val: &Value, f: &mut fmt::Formatter, lvl: u32) {
//     match self {
//         Value::Pi(a, b) => write!(f, "(∏ x{} : {:?}. {})", lvl, a, level_print(b, f, lvl + 1)),
//           Value::Lam(m) => write!(f, "Lam"),
//           Value::Box => write!(f, "Box"),
//           Value::Star => write!(f, "Star"),
//           Value::ZT => write!(f, "ZT"),
//           Value::Z(i) => write!(f, "Z: {}", i),
//           Value::QT => write!(f, "QT"),
//           Value::Q(i, j) => write!(f, "Q: {}/{}", i, j),
//           Value::Neutral(_, n) => write!(f, "Neutral: "),
//           Value::Run(_, t) => write!(f, "Run: {:?}", t),
//     }
// }



impl<'a, T: Copy> fmt::Debug for Value<'a, T> {
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
            Value::Neutral(_, n) => write!(f, "Neutral: "),
            Value::Run(_, t) => write!(f, "Run: {:?}", t),
        }
    }
}

impl<'a, T: Copy> PartialEq for Value<'a, T> {
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

#[inline(always)]
pub fn mk_neutral_var_with_type<T>(typ: RT<T>) -> RT<T>
where T: Copy
{
    Rc::new(Value::Neutral(typ, Rc::new(Neutral::DBI(0))))
}

pub fn as_symbolic<T>(v: &Value<T>)
                          -> TResult<Ident<T>, T>
where T: Copy
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
where T: Copy
{
    match v {
        Value::ZT => Ok(()),
        _ => Err(TypecheckingErrors::NotZ),
    }
}

#[allow(non_snake_case)]
pub fn as_Q<T>(v: & Value<T>) -> TResult<(), T>
where T: Copy
{
    match v {
        Value::QT => Ok(()),
        _ => Err(TypecheckingErrors::NotZ),
    }
}

pub fn is_neutral<T>(v: &Value<T>) -> bool
where T: Copy
{
    matches!(v, Value::Neutral(_, _))
    // matches! v {
    //     Value::Neutral(_, _) => true,
    //     _ => false,
    // }
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


#[derive(Debug, Clone)]
pub enum Neutral<'a, T: Copy>
{
    Var(T),
    DBI(u32),
    Hole(RefCell<Option<RT<'a, T>>>),
    App(Rc<Neutral<'a, T>>, Normal<'a, T>),
}

#[derive(Debug, Clone)]
pub struct Normal<'a, T: Copy>(pub Rc<Type<'a, T>>, pub Rc<Value<'a, T>>);
