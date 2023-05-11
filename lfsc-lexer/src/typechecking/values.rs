use core::fmt;
use std::rc::Rc;
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
pub enum Value<'term, T: Copy> {
    Pi(RT<'term, T>, Closure<'term, T>),
    Lam(Closure<'term, T>),
    Box, // Universe
    Star,
    ZT,
    Z(i32), // TODO: should in fact be unbounded
    QT,
    Q(i32, i32), // TODO: should in fact be unbounded
    Neutral(RT<'term, T>, Rc<Neutral<'term, T>>),
    Run(&'term AlphaTermSC<T>, RT<'term, T>),
    Prog(Vec<RT<'term, T>>, &'term AlphaTermSC<T>),
}

// fn unfold<'term, T>(f: &Closure<'term, T>) -> RT<'term, T>
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



impl<'term, T: Copy + fmt::Debug> fmt::Debug for Value<'term, T> {
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

impl<'term, T: Copy> PartialEq for Value<'term, T> {
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
            // (Value::Neutral(a, n), Value::Neutral(b, m)) => a == b && n == m,
            // (Value::Run(_, t), Value::Run(_, u)) => t == u,
            _ => false,
        }
    }
    fn ne(&self, other: &Self) -> bool {
        println!("wtf");
        match (self, other) {
            (Value::Box, Value::Box) => false,
            (Value::Star, Value::Star) => false,
            (Value::ZT, Value::ZT) => false,
            (Value::QT, Value::QT) => false,
            (Value::Z(i), Value::Z(j)) => i != j,
            (Value::Q(i, j), Value::Q(k, l)) => i == k && j == l,
            // TODO: extremely dangerous
            _ => true,
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
pub enum Neutral<'term, T: Copy>
{
    Var(T),
    DBI(u32),
    Hole(RefCell<Option<RT<'term, T>>>),
    App(Rc<Neutral<'term, T>>, Normal<'term, T>),
    // SC
}

#[derive(Debug, Clone)]
pub struct Normal<'term, T: Copy>(pub Rc<Type<'term, T>>, pub Rc<Value<'term, T>>);
