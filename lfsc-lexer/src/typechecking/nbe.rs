use super::context::{RGCTX, RLCTX, LocalContext};
use super::values::{Closure, Neutral, Normal, Value, RT, ResRT};
use lfsc_syntax::ast::{AlphaTerm, Num};
use lfsc_syntax::ast::Ident::*;
use lfsc_syntax::ast::AlphaTerm::*;

use std::rc::Rc;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::borrow::Cow;

#[cfg(feature = "conslist")]
pub fn eval<'a, T>(term: &'a AlphaTerm<T>,
                    lctx: RLCTX<'a, T>,
                    gctx: RGCTX<'a, T>) -> ResRT<'a, T>
    where T: Clone + PartialEq + std::fmt::Debug
{
    use super::values::mk_closure;

        match term {
            Number(Num::Z(p)) => Ok(Rc::new(Value::Z(*p))),
            Number(Num::Q(p,q)) => Ok(Rc::new(Value::Q(*p, *q))),
            Hole => {
                // Notice that hole does not have type kind, however
                // we will never get to use the type of a hole anyways.
                let refcell = RefCell::new(None);
                let neu = Rc::new(Neutral::Hole(refcell.clone()));
                Ok(Rc::new(Value::Neutral(gctx.kind.clone(), neu)))
            },
            Ident(Symbol(name)) => gctx.get_value(name),
            Ident(DBI(i)) => lctx.get_value(*i),
            App(fun, arg) => {
                let e1 = eval(fun, lctx.clone(), gctx.clone())?;
                let e2 = eval(arg, lctx, gctx.clone())?;
                do_app(e1, e2, gctx)
            },
            Pi(ty, body) => {
                let dom = eval(ty, lctx.clone(), gctx.clone())?;
                let ran = mk_closure(body, lctx);
                return Ok(Rc::new(Value::Pi(dom, ran)));
            },
            Lam(body) | AnnLam(_, body) => {
                let closure = mk_closure(body, lctx);
                Ok(Rc::new(Value::Lam(closure)))
            },
            Asc(_, val) => eval(val, lctx, gctx),
            SC(..) => todo!("eval SC"),
        }
}
// #[cfg(feature = "conslist")]
// pub fn eval_closure<'a, T>(closure: Closure<'a, T>,
//                             arg: RT<'a, T>,
//                             gctx: RGCTX<'a, T>) -> ResRT<'a, T>
// where T: Clone + PartialEq + std::fmt::Debug
// {
//        eval(closure.body, LocalContext::insert(arg, closure.env.clone()), gctx)
// }

#[cfg(feature = "conslist")]
pub fn do_app<'a, T>(func: RT<'a, T>, arg: RT<'a, T>,
                     gctx: RGCTX<'a,T>) -> ResRT<'a, T>
where T: Clone + PartialEq + std::fmt::Debug
{
   match func.borrow() {
       Value::Lam(closure) => closure(arg, gctx),
       Value::Neutral(f, neu) => {
           if let Value::Pi(dom, ran) = f.borrow() {
               Ok(Rc::new(Value::Neutral(
                   ran(arg.clone(), gctx.clone())?,
                   Rc::new(Neutral::App(neu.clone(), Normal(dom.clone(), arg))))))
           } else {
               todo!("This should be an error")
           }
       }
       _ => todo!("This should be an error"),
   }
}
