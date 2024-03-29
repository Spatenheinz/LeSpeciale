use core::fmt;
use std::rc::Rc;
use std::cell::{RefCell, Cell};

use lfsc_syntax::ast::{AlphaTerm, AlphaTermSC, Ident, BuiltIn};

use super::errors::TypecheckingErrors;
use super::{EnvWrapper, context::{LocalContext, GlobalContext}};

pub type Closure<'term, T> =
    Box<dyn Fn(RT<'term, T>, &GlobalContext<'term, T>) -> ResRT<'term, T> + 'term>;

pub fn const_closure<T>(cons: RT<T>) -> Closure<T>
where T: BuiltIn
{
    Box::new(move |_,_| { Ok(cons.clone()) })
}

pub fn mk_closure<'term, T>(body: &'term AlphaTerm<T>,
                         lctx: super::context::Rlctx<'term, T>,
                        ) -> Closure<'term, T>
where T: BuiltIn
{
    Box::new(move |v, gctx| {
             let lctx = LocalContext::insert(v, lctx.clone());
             let env = EnvWrapper::new(lctx, gctx);
             env.eval(body)})
}

pub type TResult<T, K> = Result<T, TypecheckingErrors<K>>;
pub type Type<'term, T> = Value<'term, T>;
pub type RT<'term, T> = Rc<Type<'term, T>>;
pub type ResRT<'term, T> = TResult<RT<'term, T>, T>;

pub enum Value<'term, T: BuiltIn> {
    Pi(bool, RT<'term, T>, Closure<'term, T>),
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

impl<'term, T: BuiltIn> fmt::Debug for Value<'term, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Pi(_, a, _) => write!(f, "∏ {:?}", a),
            Value::Lam(_) => write!(f, "Lam"),
            Value::Box => write!(f, "Box"),
            Value::Star => write!(f, "Star"),
            Value::ZT => write!(f, "ZT"),
            Value::Z(i) => write!(f, "Z: {}", i),
            Value::QT => write!(f, "QT"),
            Value::Q(i, j) => write!(f, "Q: {}/{}", i, j),
            Value::Neutral(_ty, n) => write!(f, "{:?}", n),
            Value::Run(ty, t, _) => write!(f, "^ {:?} {:?}", ty, t),
            Value::Prog(vars, t) => write!(f, "Prog: {:?}\n\t{:?}\n\t", vars, t),
        }
    }
}



impl<'term, T: BuiltIn> PartialEq for Value<'term, T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // TODO incomplete
            (Value::Box, Value::Box) => true,
            (Value::Star, Value::Star) => true,
            (Value::ZT, Value::ZT) => true,
            (Value::QT, Value::QT) => true,
            (Value::Z(i), Value::Z(j)) => i == j,
            (Value::Q(i, j), Value::Q(k, l)) => i == k && j == l,
            (Value::Neutral(a, n), Value::Neutral(b, m)) => {
                **n == **m && a == b
            }
            (Value::Run(a, t,_), Value::Run(b, u,_)) => a == b && t == u,
            _ => false
        }
    }
}

pub fn ref_compare<'term, T>(a: RT<'term, T>, b: RT<'term, T>) -> bool
where T: BuiltIn
{
    match (&*a, &*b) {
        (Value::Box, Value::Box) => true,
        (Value::Star, Value::Star) => true,
        (Value::ZT, Value::ZT) => true,
        (Value::QT, Value::QT) => true,
        (Value::Z(i), Value::Z(j)) => i == j,
        (Value::Q(i, j), Value::Q(k, l)) => i == k && j == l,
        (Value::Neutral(a, n), Value::Neutral(b, m)) => {
            ref_compare_neutral(n.clone(), m.clone()) && ref_compare(a.clone(), b.clone())
        },
        _ => false,
    }
}

fn ref_compare_neutral<'term, T>(n: Rc<Neutral<'term, T>>, m: Rc<Neutral<'term, T>>) -> bool
where T: BuiltIn
{
    match (&*n, &*m) {
        (Neutral::Var(i), Neutral::Var(j)) => i == j,
        (Neutral::DBI(i), Neutral::DBI(j)) => i == j,
        (Neutral::App(a, b), Neutral::App(c, d)) => {
            ref_compare_neutral(a.clone(), c.clone()) && ref_compare_normal(b, d)
        },
        (Neutral::Hole(hol), a) => {
            if let Some(t) = &*hol.borrow() {
                return ref_compare_neutral(t.clone(), m)
            }
                hol.replace(Some(m));
                true
        },
        (a, Neutral::Hole(hol)) => {
            if let Some(t) = &*hol.borrow() {
                return ref_compare_neutral(t.clone(), n)
            }
                hol.replace(Some(n));
                true
        },
        _ => false,
    }
}
fn ref_compare_normal<'term, T>(n: &Normal<'term, T>, m: &Normal<'term, T>) -> bool
where T: BuiltIn
{
    ref_compare(n.0.clone(), m.0.clone()) && ref_compare(n.1.clone(), m.1.clone())
}

#[inline(always)]
pub fn mk_neutral_var_with_type<T>(typ: RT<T>) -> RT<T>
where T: BuiltIn
{
    Rc::new(Value::Neutral(typ, Rc::new(Neutral::DBI(0))))
}

pub fn as_type<T>(v: &Value<T>) -> TResult<(), T>
where T: BuiltIn
{
    match v {
        Value::Star => Ok(()),
        _ => Err(TypecheckingErrors::ExpectedType),
    }
}

pub fn is_type_or_datatype<T>(v: &Value<T>) -> TResult<(),T>
where T: BuiltIn
{
    if Type::Star == *v {
        return Ok(());
    }
    is_datatype(v)
}

pub fn is_Z_or_Q<T>(v: &Value<T>) -> TResult<(), T>
where T: BuiltIn
{
    if let Ok(..) = as_Z(v) {
        return Ok(());
    }
    if let Ok(..) = as_Q(v) {
        return Ok(());
    }
    Err(TypecheckingErrors::ExpectedNum)
}

pub fn is_datatype<T>(v: &Value<T>) -> TResult<(), T>
where T: BuiltIn
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
where T: BuiltIn
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
where T: BuiltIn
{
    match v {
        Value::ZT => Ok(()),
        _ => Err(TypecheckingErrors::NotZ),
    }
}

#[allow(non_snake_case)]
pub fn as_Q<T>(v: & Value<T>) -> TResult<(), T>
where T: BuiltIn
{
    match v {
        Value::QT => Ok(()),
        _ => Err(TypecheckingErrors::NotZ),
    }
}
pub fn as_neutral<'term, T>(v: &Value<'term, T>)
                          -> TResult<Rc<Neutral<'term, T>>, T>
where T: BuiltIn
{
    match v {
        Value::Neutral(_, n) => Ok(n.clone()),
        _ => Err(TypecheckingErrors::NotSymbolic),
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Neutral<'term, T: BuiltIn>
{
    Var(T),
    DBI(u32),
    App(Rc<Neutral<'term, T>>, Normal<'term, T>),
    Hole(RefCell<Option<Rc<Neutral<'term, T>>>>),
}

pub fn flatten<'term, T: BuiltIn>(neu: &Neutral<'term, T>) -> TResult<(Ident<T>, Vec<RT<'term, T>>), T> {
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

#[derive(Clone, PartialEq)]
pub struct Normal<'term, T: BuiltIn>(pub Rc<Type<'term, T>>, pub Rc<Value<'term, T>>);

impl<'term, T: BuiltIn> fmt::Debug for Normal<'term, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.1)
    }
}
