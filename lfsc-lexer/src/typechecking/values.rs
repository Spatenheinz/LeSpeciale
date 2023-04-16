use std::rc::Rc;
use std::cell::RefCell;
use std::borrow::Cow;

use lfsc_syntax::ast::AlphaTerm;

use super::context::{LResult, LocalContext};

#[derive(Debug, Clone)]
pub struct Closure<'a, T: Clone> {
    #[cfg(feature = "conslist")]
    pub env: super::context::RLCTX<'a, T>,
    // #[cfg(not(feature = "conslist"))]
    // pub env: &'a LocalContext<'a, T>,
    pub body: &'a AlphaTerm<T>,
}
//
pub type Type<'a, T> = Value<'a, T>;
pub type RT<'a, T> = Rc<Type<'a, T>>;
// pub type RT<'a, T> = &'a Type<'a, T>;
// pub type Eval<'a, T> = TResult<(RT<'a, T>, LocalContext<'a, T>)>;
pub type ResRT<'a, T> = TResult<RT<'a, T>, T>;

#[derive(Debug, Clone)]
pub enum Value<'a, T: Clone> {
    Pi(RT<'a, T>, Closure<'a, T>),
    Lam(Closure<'a, T>),
    Kind, // Universe
    Type,
    ZT,
    Z(u32),
    QT,
    Q(u32, u32),
    Neutral(RT<'a, T>, Rc<Neutral<'a, T>>),
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

pub fn as_pi<'a, T>(v: & Value<'a, T>)
                    -> TResult<(RT<'a,T>, Closure<'a, T>), T>
where T: Clone
{
    match v {
        Value::Pi(a, b) => Ok((a.clone(), b.clone())),
        _ => Err(TypecheckingErrors::NotPi),
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
    Mismatch(AlphaTerm<T>, AlphaTerm<T>),
}

pub type TResult<T, K> = Result<T, TypecheckingErrors<K>>;

#[derive(Debug, Clone)]
pub enum Neutral<'a, T>
where T: Clone
{
    Var(T),
    DBI(u32),
    Hole(RefCell<Option<RT<'a, T>>>),
    App(Rc<Neutral<'a, T>>, Normal<'a, T>),
}

#[derive(Debug, Clone)]
pub struct Normal<'a, T: Clone>(pub Rc<Type<'a, T>>, pub Rc<Value<'a, T>>);
