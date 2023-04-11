use lfsc_syntax::ast::{AlphaTerm, TermLiteral, FromLit, Num};
use lfsc_syntax::ast::AlphaTerm::*;
use super::nbe::eval_closure;
use super::synth::synth;
use super::values::{Value, TResult, RT, as_pi, Neutral, as_Z, as_Q};
use super::context::RLCTX;

use std::rc::Rc;
use std::borrow::{BorrowMut, Borrow};


pub fn check<'a, T>(term: &'a AlphaTerm<T>, tau: RT<'a, T>,
                    ctx: RLCTX<'a, T>) -> TResult<()>
where
    T: PartialEq + Clone + FromLit + std::fmt::Debug,
{
    match term {
        Literal(TermLiteral::Number(Num::Z(_))) => as_Z(tau.borrow()),
        Literal(TermLiteral::Number(Num::Q(_,_))) => as_Q(tau.borrow()),
        Var(x) => todo!(),
        DBI(i) => todo!(),
        AlphaTerm::Pi(t1, t2) => {
            todo!()
        },
        AlphaTerm::Lam(body) => {
            let (a,b) = as_pi(tau.borrow())?;
            let val = eval_closure(b,
                Rc::new(Value::Neutral(a.clone(),
                                       Rc::new(Neutral::DBI(0)))))?;
            super::context::LocalContext::insert(ctx.borrow_mut(), a);
            check(body, val, ctx)
        },
        AnnLam(..) => todo!(),
        Asc(t1, t2) => {
            todo!()
        },
        SC(t1, t2) => {
            todo!()
        },
        _ => {
            let t = synth(term, ctx.clone())?;
            convert(Rc::new(Value::Kind), t, tau, ctx)
        }
    }
}

fn convert<'a, T>(t1: RT<'a, T>, t2: RT<'a, T>, tau: RT<'a, T>,
                  ctx: RLCTX<'a, T>) -> TResult<()>
where
    T: PartialEq + Clone + FromLit + std::fmt::Debug,
{
    // if let Value::Neutral(_,x) = t1.borrow() {
    //     if let f @ Neutral::Hole(None) = x.borrow() {
    //         f.borrow_mut().0 = Some(t2.clone());
    //     }
    // };
    Ok(())

}
