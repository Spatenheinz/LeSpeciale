use lfsc_syntax::ast::{AlphaTerm, Num, BuiltIn};
use lfsc_syntax::ast::AlphaTerm::*;
use super::nbe::{eval, eval_closure};
use super::values::{as_pi, TypecheckingErrors, ResRT};
use super::context::{RLCTX, LocalContext, RGCTX};
use super::check::check;

use std::rc::Rc;
use std::borrow::Borrow;

pub fn synth<'a, T>(term: &'a AlphaTerm<T>,
                    lctx: RLCTX<'a,T>,
                    gctx: RGCTX<'a, T>) -> ResRT<'a,T>
where   T: PartialEq + Clone + BuiltIn + std::fmt::Debug,
{
    match term {
        Number(Num::Z(_)) => gctx.get_value(&T::_mpz()),
        Number(Num::Q(..)) => gctx.get_value(&T::_mpq()),
        Var(x)            => gctx.get_type(x),
        DBI(i)            => lctx.get_type(*i),
        AlphaTerm::Pi(t1, t2) => {
            check(t1, gctx.kind.clone(), lctx.clone(), gctx)?;
            let val = eval(t1, lctx.clone(), gctx)?;
            check(t2, gctx.kind.clone(),
                  LocalContext::insert(val.clone(), lctx.clone()), gctx)?;
            Ok(gctx.kind.clone())
        },
        AnnLam(dom, range) => {
            // to make a type for a lambda, we start by checking that the domain is type
            // it cannot be Kind.
            // then we check that the range is a function -- im not entirely sure this is correct.
            // check(dom, Rc::new(Type::Type), lctx.clone(), gctx.clone())?;
            // let range = synth(range, lctx.clone(), gctx.clone())?;
            // Ok(Rc::new(Type::Pi(dom.clone(), range)));
            todo!("cannot synth a lambda");
        }
        App(t1, t2) => {
            let f_ty = synth(t1, lctx.clone(), gctx)?;
            let (a,b) = as_pi(f_ty.borrow())?;
            check(t2, a.clone(), lctx.clone(), gctx)?;
            let res_clo = eval_closure(b, eval(t2, lctx.clone(), gctx)?, gctx)?;
            Ok(res_clo)
        }
        Asc(t1, t2) => {
            let ty = synth(t1, lctx.clone(), gctx)?;
            check(t2, ty.clone(), lctx, gctx)?;
            Ok(ty)
        }
        AlphaTerm::Lam(_) => Err(TypecheckingErrors::CannotInferLambda),
        Hole => Err(TypecheckingErrors::CannotInferHole),
        SC(t1, t2) => {
            todo!()
        }
    }
}
