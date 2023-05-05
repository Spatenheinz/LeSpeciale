use core::fmt;
use std::rc::Rc;
use std::cell::RefCell;

use lfsc_syntax::ast::{AlphaTerm, AlphaTermSC, Ident, BuiltIn};

use super::{EnvWrapper, context::LocalContext};

pub type Closure<'a,T> =
    Box<dyn Fn(RT<'a, T>, &EnvWrapper<'a, T>) -> ResRT<'a, T> + 'a>;

pub fn const_closure<T>(cons: RT<T>) -> Closure<T>
where T: PartialEq + std::fmt::Debug + Copy + BuiltIn
{
    Box::new(move |_, _| { Ok(cons.clone()) })
}

pub fn mk_closure<'a, T>(body: &'a AlphaTerm<T>,
                         lctx: super::context::Rlctx<'a, T>,
                        ) -> Closure<'a, T>
where T: PartialEq + std::fmt::Debug + Copy + BuiltIn
{
    Box::new(move |v, env|{
             let lctx = LocalContext::insert(v, lctx.clone());
             let env = EnvWrapper::new(lctx, env.gctx.clone(), env.allow_dbi);
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
    Prog(Vec<RT<'a, T>>, &'a AlphaTermSC<T>),
}

// fn unfold<'a, T>(f: &Closure<'a, T>) -> RT<'a, T>
// where T: Copy + fmt::Debug + BuiltIn + PartialEq
// {
//     let v = mk_neutral_var_with_type(Type::Box.into());
//     f(v, &EnvWrapper::empty()).unwrap()
// }

// fn level_print<T>(val: &Value<T>, f: &mut fmt::Formatter, lvl: u32) -> fmt::Result
// where T: Copy + fmt::Debug + BuiltIn + PartialEq
// {
//     let plunge = |fun| {
//            level_print(&unfold(fun), f.clone(), lvl+1)
//         };
//     match val {
//         Value::Pi(a, b) => { write!(f, "(∏ x{} : {:?}. )", lvl, a); plunge(&b); Ok(()) },
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



impl<'a, T: Copy + fmt::Debug> fmt::Debug for Value<'a, T> {
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
            Value::Neutral(ty, n) => write!(f, "Neutral:\n\tty:  {:?}\n\tvar: {:?}", ty, n),
            Value::Run(_, t) => write!(f, "Run: {:?}", t),
            Value::Prog(vars, t) => write!(f, "Prog: {:?}\n\t{:?}\n\t", vars, t),
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
            // (Value::Neutral(a, n), Value::Neutral(b, m)) => a == b && n == m,
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

pub fn is_type_or_datatype<T>(v: &Value<T>) -> TResult<(),T>
where T: Copy
{
    if Type::Star == *v {
        return Ok(());
    }
    is_datatype(v)
}

pub fn is_datatype<T>(v: &Value<T>) -> TResult<(), T>
where T: Copy
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
    // SC
}

#[derive(Debug, Clone)]
pub struct Normal<'a, T: Copy>(pub Rc<Type<'a, T>>, pub Rc<Value<'a, T>>);
