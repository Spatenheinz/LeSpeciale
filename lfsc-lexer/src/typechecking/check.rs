use lfsc_syntax::ast::{AlphaTerm, Num, BuiltIn};
use lfsc_syntax::ast::AlphaTerm::*;
use super::nbe::eval_closure;
use super::readback::readback;
use super::synth::synth;
use super::values::{Value, TResult, RT, as_pi, Neutral, as_Z, as_Q, TypecheckingErrors};
use super::context::{RLCTX, LocalContext, RGCTX};

use std::rc::Rc;
use std::borrow::{BorrowMut, Borrow};


pub fn check<'a, T>(term: &'a AlphaTerm<T>, tau: RT<'a, T>,
                    lctx: RLCTX<'a, T>,
                    gctx: RGCTX<'a, T>) -> TResult<(), T>
where
    T: PartialEq + Clone + BuiltIn + std::fmt::Debug,
{
    match term {
        Number(Num::Z(_)) => as_Z(tau.borrow()),
        Number(Num::Q(_,_)) => as_Q(tau.borrow()),
        AlphaTerm::Lam(body) => {
            let (a,b) = as_pi(tau.as_ref())?;
            let val = eval_closure(b.clone(),
                Rc::new(Value::Neutral(a.clone(),
                                       Rc::new(Neutral::DBI(0)))),
                gctx)?;
            let ctx1 = LocalContext::insert(a, lctx.clone());
            check(body, val, ctx1, gctx)
        },
        AnnLam(..) => todo!(),
        SC(t1, t2) => {
            todo!()
        },
        // Fv, Bv, Ascription, PI,
        _ => {
            let t = synth(term, lctx.clone(), gctx)?;
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
    let e2 = readback(tau.clone(), t2, lctx.clone(), gctx)?;
    if e1 == e2 {
        Ok(())
    } else {
        Err(TypecheckingErrors::Mismatch(e1, e2))
    }

}
