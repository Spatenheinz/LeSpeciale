use lfsc_syntax::ast::{AlphaTerm, BuiltIn};
use lfsc_syntax::ast::AlphaTerm::*;
use super::readback::readback;
use super::infer::infer;
use super::values::{Value, TResult, RT, Neutral, TypecheckingErrors};
use super::context::{RLCTX, LocalContext, RGCTX};

use std::rc::Rc;
use std::borrow::Borrow;

pub fn check<'a, T>(term: &'a AlphaTerm<T>, tau: RT<'a, T>,
                    lctx: RLCTX<'a, T>,
                    gctx: RGCTX<'a, T>) -> TResult<(), T>
where
    T: PartialEq + Clone + BuiltIn + std::fmt::Debug,
{
    match term {
        AlphaTerm::Lam(body) => {
            if let Value::Pi(a,b) = tau.borrow() {
                let val = b(Rc::new(Value::Neutral(a.clone(),
                                        Rc::new(Neutral::DBI(0)))))?;
                let ctx1 = LocalContext::insert(a.clone(), lctx.clone());
                return check(body, val, ctx1, gctx)
            }
            Err(TypecheckingErrors::NotPi)
        },
        SC(t1, t2) => {
            todo!()
        },
        // Fv, Bv, Ascription, PI, Annotated, etc.
        _ => {
            let t = infer(term, lctx.clone(), gctx)?;
            convert(t, tau, gctx.kind.clone(),  lctx, gctx)
        }
    }
}

fn convert<'a, T>(t1: RT<'a, T>, t2: RT<'a, T>, tau: RT<'a, T>,
                  lctx: RLCTX<'a, T>,
                  gctx: RGCTX<'a, T>) -> TResult<(), T>
where
    T: PartialEq + Clone + BuiltIn + std::fmt::Debug,
{
    let e1 = readback(tau.clone(), t1, lctx.clone(), gctx)?;
    let e2 = readback(tau.clone(), t2, lctx, gctx)?;
    if e1 == e2 {
        Ok(())
    } else {
        Err(TypecheckingErrors::Mismatch(e1, e2))
    }

}
