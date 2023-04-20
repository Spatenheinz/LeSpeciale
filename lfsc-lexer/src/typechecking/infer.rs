use lfsc_syntax::ast::{AlphaTerm, Num, BuiltIn, AlphaTermSC, NumericSC,
                       CompoundSC, SideEffectSC};
use lfsc_syntax::ast::AlphaTerm::*;
use lfsc_syntax::ast::NumericSC::*;
use lfsc_syntax::ast::CompoundSC::*;
use lfsc_syntax::ast::SideEffectSC::*;
use super::nbe::{eval, eval_closure};
use super::values::{as_pi, TypecheckingErrors, ResRT, Type, Closure, Value};
use super::context::{RLCTX, LocalContext, RGCTX};
use super::check::check;

use std::rc::Rc;
use std::borrow::Borrow;

pub fn infer<'a, T>(term: &'a AlphaTerm<T>,
                    lctx: RLCTX<'a,T>,
                    gctx: RGCTX<'a, T>) -> ResRT<'a,T>
where   T: PartialEq + Clone + BuiltIn + std::fmt::Debug,
{
    match term {
        Number(Num::Z(_)) => gctx.get_value(&T::_mpz()),
        Number(Num::Q(..)) => gctx.get_value(&T::_mpq()),
        Var(x)            => gctx.get_type(x),
        DBI(i)            => lctx.get_type(*i),
        AlphaTerm::Pi(a, b) => {
            // may this only be star or also box?
            infer_sort(a, lctx.clone(), gctx)?;
            let val = eval(a, lctx.clone(), gctx)?;
            infer_sort(b, LocalContext::insert(val.clone(), lctx), gctx)
        },
        AnnLam(a, m) => {
            let val = infer(a, lctx.clone(), gctx)?;
            infer(m, LocalContext::insert(val.clone(), lctx.clone()), gctx)?;
            let closure = Closure{ env: lctx, body: m };
            Ok(Rc::new(Type::Pi(val, closure)))
        }
        App(m, n) => {
            let f_ty = infer(m, lctx.clone(), gctx)?;
            let (a,b) = as_pi(f_ty.borrow())?;
            check(n, a.clone(), lctx.clone(), gctx)?;
            let res_clo = eval_closure(b, eval(n, lctx.clone(), gctx)?, gctx)?;
            // if we use HOAS we can just use f(n)
            Ok(res_clo)
        }
        Asc(a, m) => {
            infer_sort(a, lctx.clone(), gctx)?;
            let ty = infer(a, lctx.clone(), gctx)?;
            check(m, ty.clone(), lctx, gctx)?;
            Ok(ty)
        }
        AlphaTerm::Lam(_) => Err(TypecheckingErrors::CannotInferLambda),
        Hole => Err(TypecheckingErrors::CannotInferHole),
        // only allowed when infering and only inside of a pi or annotated lambda.
        SC(t1, t2) => {
            let t1_ = infer_sc(t1, lctx.clone(), gctx)?;
            let t2 = infer_sc(t2, lctx, gctx)?;
            Ok(Rc::new(Type::Run(t1, t2)))
        }
    }
}

fn infer_sort<'a, T>(term: &'a AlphaTerm<T>,
                  lctx: RLCTX<'a, T>,
                  gctx: RGCTX<'a, T>) -> ResRT<'a, T>
where   T: PartialEq + Clone + BuiltIn + std::fmt::Debug,
{
    let x = infer(term, lctx, gctx)?;
    match x.borrow() {
        Type::Box | Type::Star => Ok(x),
        _ => Err(TypecheckingErrors::ExpectedSort)
    }
}

fn infer_sc<'a, T>(sc: &'a AlphaTermSC<T>,
                 lctx: RLCTX<'a, T>,
                 gctx: RGCTX<'a, T>) -> ResRT<'a, T>
    where T: Clone + PartialEq + std::fmt::Debug
{
    match sc {
    AlphaTermSC::Number(Num::Z(p)) => Ok(Rc::new(Type::ZT)),
    AlphaTermSC::Number(Num::Q(p,q)) => Ok(Rc::new(Type::Q(*p, *q))),
    // Little unsure about value vs type.
    AlphaTermSC::DBI(i) => lctx.get_type(*i),
    AlphaTermSC::Var(x) => gctx.get_type(x),
    AlphaTermSC::Let(m, n) => {
        let m_ty = infer_sc(m, lctx.clone(), gctx.clone())?;
        let lctx = LocalContext::insert(m, lctx);
        infer_sc(n, lctx, gctx)
    },
    AlphaTermSC::App(m, n) => todo!(),
    AlphaTermSC::Numeric(num) => infer_num(num, lctx, gctx),
    AlphaTermSC::Compound(compound) => infer_compound(compound, lctx, gctx),
    AlphaTermSC::SideEffect(se) => infer_sideeffect(se, lctx, gctx)
    }
}

fn infer_sideeffect<'a, T>(sc: &'a SideEffectSC<AlphaTermSC<T>>,
                 lctx: RLCTX<'a, T>,
                 gctx: RGCTX<'a, T>) -> ResRT<'a, T>
    where T: Clone + PartialEq + std::fmt::Debug
{
    match sc {
        Do(a, b) => {
            todo!()
        },
        MarkVar(n, var) => {
            todo!()
        },
        IfMarked{ n, c, tbranch, fbranch} => {
            todo!()
        }
    }
}

fn infer_compound<'a, T>(sc: &'a CompoundSC<AlphaTermSC<T>, T>,
                 lctx: RLCTX<'a, T>,
                 gctx: RGCTX<'a, T>) -> ResRT<'a, T>
    where T: Clone + PartialEq + std::fmt::Debug
{
    match sc {
        Fail(x) => {
            let x_ty = infer_sc(x, lctx, gctx)?;
            if *x_ty.borrow() != Type::Star {
                // why not kind here??
                return Err(TypecheckingErrors::ExpectedSort)
            }
            // TODO: what should we return here?
            Ok(x_ty)

        }
        IfEq { a, b, tbranch, fbranch } => {
            todo!("figure out how to compare")
        }
        Compare { a, b, tbranch, fbranch } => {
            todo!() // somehow a little tricky
        }
        Match(scrut, cases) => {
            todo!("maybe easier after apply..")
        }
    }
}

fn infer_num<'a, T>(sc: &'a NumericSC<AlphaTermSC<T>>,
                 lctx: RLCTX<'a, T>,
                 gctx: RGCTX<'a, T>) -> ResRT<'a, T>
    where T: Clone + PartialEq + std::fmt::Debug
{
    match sc {
        Sum(x, y) | Prod(x, y) | Div(x, y) => {
            let x_ty = infer_sc(x, lctx.clone(), gctx)?;
            let y_ty = infer_sc(y, lctx, gctx)?;
            let x_bor = *x_ty.borrow();
            if Type::ZT != x_bor || Type::QT != x_bor {
                return Err(TypecheckingErrors::ExpectedNum)
            };
            if x_bor != *y_ty.borrow() {
                return Err(TypecheckingErrors::ExpectedSameNum)
            };
            Ok(x_ty)
        }
        Neg(x) => {
            let x_ty = infer_sc(x, lctx.clone(), gctx)?;
            let x_bor = *x_ty.borrow();
            if Type::ZT != x_bor || Type::QT != x_bor {
                return Err(TypecheckingErrors::ExpectedNum)
            };
            Ok(x_ty)
        },
        ZtoQ(z) => {
            let z_ty = infer_sc(z, lctx.clone(), gctx)?;
            if Type::ZT != *z_ty.borrow() {
                return Err(TypecheckingErrors::NotZ)
            }
            Ok(z_ty)
        },
        ZBranch { n, tbranch, fbranch } | NegBranch { n, tbranch, fbranch } => {
            let n_ty = infer_sc(n, lctx.clone(), gctx)?;
            let n_ty = *n_ty.borrow();
            if Type::ZT != n_ty || Type::QT != n_ty {
                return Err(TypecheckingErrors::ExpectedNum)
            };
            let tbranch_ty = infer_sc(tbranch, lctx.clone(), gctx)?;
            let fbranch_ty = infer_sc(fbranch, lctx.clone(), gctx)?;
            if *tbranch_ty.borrow() != *fbranch_ty.borrow() {
                return Err(TypecheckingErrors::ExpectedSameInBranch)
            };
            Ok(tbranch_ty)
        }
    }
}
