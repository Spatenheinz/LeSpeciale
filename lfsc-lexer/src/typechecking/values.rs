use std::rc::Rc;
use std::cell::RefCell;

use lfsc_syntax::ast::AlphaTerm;

use super::context::LResult;
use std::borrow::Borrow;

#[derive(Debug)]
pub struct Closure<'a, T> {
    // pub env: &'a mut super::context::LocalContext<'a, T>,
    pub env: Rc<RefCell<super::context::LocalContext<'a, T>>>,
    pub body: &'a AlphaTerm<T>,
}
//
pub type Type<'a, T> = Value<'a, T>;
pub type RT<'a, T> = Rc<Type<'a, T>>;

#[derive(Debug, Clone)]
pub enum Value<'a, T> {
    Pi(RT<'a, T>, Rc<Closure<'a, T>>),
    Lam(Rc<Closure<'a, T>>),
    Kind, // Universe
    ZT,
    Z(u32),
    QT,
    Q(u32, u32),
    Neutral(RT<'a, T>, Rc<Neutral<'a, T>>),
}

#[allow(non_snake_case)]
pub fn as_Z<'a,T>(v: RT<'a, T>) -> TResult<()> {
    match v.borrow() {
        Value::ZT => Ok(()),
        _ => Err(TypecheckingErrors::NotZ),
    }
}

#[allow(non_snake_case)]
pub fn as_Q<'a,T>(v: RT<'a, T>) -> TResult<()> {
    match v.borrow() {
        Value::QT => Ok(()),
        _ => Err(TypecheckingErrors::NotZ),
    }
}

pub fn as_pi<'a, T>(v: RT<'a, T>)
                    -> TResult<(RT<'a,T>, Rc<Closure<'a, T>>)> {
    match v.borrow() {
        Value::Pi(a, b) => Ok((a.clone(), b.clone())),
        _ => Err(TypecheckingErrors::NotPi),
    }
}

pub fn is_neutral<'a, T>(v: &Value<'a, T>) -> bool {
    match v {
        Value::Neutral(_, _) => true,
        _ => false,
    }
}

#[derive(Debug)]
pub enum TypecheckingErrors {
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
}

pub type TResult<T> = Result<T, TypecheckingErrors>;

#[derive(Debug, Clone)]
pub enum Neutral<'a, T> {
    Var(T),
    DBI(u32),
    Hole(Option<RT<'a, T>>),
    App(Rc<Neutral<'a, T>>, Normal<'a, T>),
}

#[derive(Debug, Clone)]
pub struct Normal<'a, T>(pub Rc<Type<'a, T>>, pub Rc<Value<'a, T>>);
