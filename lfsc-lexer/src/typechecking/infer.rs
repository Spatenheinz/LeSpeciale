use lfsc_syntax::ast::{AlphaTerm, Num, BuiltIn, AlphaTermSC, NumericSC,
                       CompoundSC, SideEffectSC, Pattern};
use lfsc_syntax::ast::Ident::*;
use lfsc_syntax::ast::AlphaTerm::*;
use lfsc_syntax::ast::NumericSC::*;
use lfsc_syntax::ast::CompoundSC::*;
use lfsc_syntax::ast::SideEffectSC::*;
use super::nbe::{eval, do_app};
use super::values::{TypecheckingErrors, ResRT, Type, mk_closure, TResult, RT};
use super::context::{RLCTX, LocalContext, RGCTX, get_type};
use super::check::check;

use std::rc::Rc;
use std::borrow::Borrow;

pub fn infer<'a, T>(term: &'a AlphaTerm<T>,
                    lctx: RLCTX<'a,T>,
                    gctx: RGCTX<'a, T>) -> ResRT<'a,T>
where   T: PartialEq + Clone + BuiltIn + std::fmt::Debug,
{
    match term {
        Number(Num::Z(_))  => gctx.get_value(&T::_mpz()),
        Number(Num::Q(..)) => gctx.get_value(&T::_mpq()),
        Ident(x)   => get_type(x, lctx, gctx),
        AlphaTerm::Pi(a, b) => {
            // may this only be star or also box?
            infer_sort(a, lctx.clone(), gctx.clone())?;
            let val = eval(a, lctx.clone(), gctx.clone())?;
            infer_sort(b, LocalContext::insert(val.clone(), lctx), gctx)
        },
        AnnLam(a, m) => {
            let val = infer(a, lctx.clone(), gctx.clone())?;
            infer(m, LocalContext::insert(val.clone(), lctx.clone()), gctx.clone())?;
            let closure = mk_closure(m, lctx);
            Ok(Rc::new(Type::Pi(val, closure)))
        }
        App(m, n) => {
            let f_ty = infer(m, lctx.clone(), gctx.clone())?;
            if let Type::Pi(a,b) = f_ty.borrow() {
                check(n, a.clone(), lctx.clone(), gctx.clone())?;
                return b(eval(n, lctx, gctx.clone())?, gctx)
            };
            Err(TypecheckingErrors::NotPi)
        }
        Asc(a, m) => {
            infer_sort(a, lctx.clone(), gctx.clone())?;
            let ty = infer(a, lctx.clone(), gctx.clone())?;
            check(m, ty.clone(), lctx, gctx)?;
            Ok(ty)
        }
        AlphaTerm::Lam(_) => Err(TypecheckingErrors::CannotInferLambda),
        Hole => Err(TypecheckingErrors::CannotInferHole),
        // only allowed when infering and only inside of a pi or annotated lambda.
        SC(t1, t2) => {
            todo!()
            // let t1_ = infer_sc(t1, lctx.clone(), gctx)?;
            // let t2 = infer_sc(t2, lctx, gctx)?;
            // Ok(Rc::new(Type::Run(t1, t2)))
        }
    }
}

pub fn infer_sort<'a, T>(term: &'a AlphaTerm<T>,
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

// fn infer_sc<'a, T>(sc: &'a AlphaTermSC<T>,
//                  lctx: RLCTX<'a, T>,
//                  gctx: RGCTX<'a, T>) -> ResRT<'a, T>
//     where T: Clone + PartialEq + std::fmt::Debug + BuiltIn
// {
//     match sc {
//     AlphaTermSC::Number(Num::Z(_)) => gctx.get_value(&T::_mpz()),
//     AlphaTermSC::Number(Num::Q(..)) => gctx.get_value(&T::_mpq()),
//     AlphaTermSC::Ident(x) => get_type(x, lctx, gctx),
//     AlphaTermSC::Let(m, n) => {
//         let m_ty = infer_sc(m, lctx.clone(), gctx.clone())?;
//         let lctx = LocalContext::insert(m_ty, lctx);
//         infer_sc(n, lctx, gctx)
//     },
//     AlphaTermSC::App(m, n) => {
//         // TODO: check if a occurs free in b,
//         // TODO: check if fully applied??
//         todo!()
//         // let m_ty = infer_sc(m, lctx.clone(), gctx.clone())?;
//         //     if let Type::Pi(a,b) = m_ty.borrow() {
//         //         let n_ty = infer_sc(n, lctx, gctx)?;
//         //         if

//         //     }
//         // }
//     },
//     AlphaTermSC::Numeric(num) => infer_num(num, lctx, gctx),
//     AlphaTermSC::Compound(compound) => infer_compound(compound, lctx, gctx),
//     AlphaTermSC::SideEffect(se) => infer_sideeffect(se, lctx, gctx)
//     }
// }

// fn infer_sideeffect<'a, T>(sc: &'a SideEffectSC<AlphaTermSC<T>>,
//                  lctx: RLCTX<'a, T>,
//                  gctx: RGCTX<'a, T>) -> ResRT<'a, T>
//     where T: Clone + PartialEq + std::fmt::Debug + BuiltIn
// {
//     match sc {
//         Do(a, b) => {
//             infer_sc(a, lctx.clone(), gctx)?;
//             infer_sc(b, lctx, gctx)
//         },
//         MarkVar(_, var) => {
//             let var_ty = infer_sc(var, lctx.clone(), gctx)?;
//             // TODO: make check about lambda bound variable.
//             Ok(var_ty)
//         },
//         IfMarked{n: _n, c, tbranch, fbranch} => {
//             let _var_ty    = infer_sc(c, lctx.clone(), gctx)?;
//             let tbranch_ty = infer_sc(tbranch, lctx.clone(), gctx)?;
//             let fbranch_ty = infer_sc(fbranch, lctx, gctx)?;
//             // TODO: make check about lambda bound variable.
//             if *&tbranch_ty != *&fbranch_ty {
//                 return Err(TypecheckingErrors::ExpectedSameInBranch)
//             }
//             Ok(tbranch_ty)
//         }
//     }
// }

// fn infer_compound<'a, T>(sc: &'a CompoundSC<AlphaTermSC<T>, T>,
//                  lctx: RLCTX<'a, T>,
//                  gctx: RGCTX<'a, T>) -> ResRT<'a, T>
//     where T: Clone + PartialEq + std::fmt::Debug + BuiltIn
// {
//     match sc {
//         Fail(x) => {
//             let x_ty = infer_sc(x, lctx, gctx)?;
//             if Type::Star != *x_ty.borrow() {
//                 return Err(TypecheckingErrors::ExpectedSort)
//             }
//             Ok(x_ty)

//         }
//         IfEq { a, b, tbranch, fbranch } => {
//             let a_ty = infer_sc(a, lctx.clone(), gctx.clone())?;
//             let b_ty = infer_sc(b, lctx.clone(), gctx.clone())?;
//             if *&a_ty != *&b_ty {
//                 // TODO: misleading
//                 return Err(TypecheckingErrors::ExpectedSameInBranch)
//             }
//             let tbranch_ty = infer_sc(tbranch, lctx.clone(), gctx.clone())?;
//             let fbranch_ty = infer_sc(fbranch, lctx.clone(), gctx.clone())?;
//             if *&tbranch_ty != *&fbranch_ty {
//                 return Err(TypecheckingErrors::ExpectedSameInBranch)
//             }
//             Ok(tbranch_ty)
//         }
//         Match(scrut, cases) => {
//             let scrut_ty = infer_sc(scrut, lctx.clone(), gctx)?;
//             // Terrible structure
//             let mut t_ty = None;
//             for i in cases.iter() {
//                 let (p, t) = i;
//                 let p_ty = infer_pattern(p, lctx.clone(), gctx)?;
//                 let lctx =
//                     match p_ty {
//                         (Some(p_ty), lctx) => {
//                             if &*p_ty != &*scrut_ty {
//                                 return Err(TypecheckingErrors::ExpectedSameInBranch)
//                             }
//                             lctx.clone()
//                         },
//                         (None, lctx) => lctx.clone()
//                     };
//                 if t_ty.is_none() {
//                     t_ty = Some(infer_sc(t, lctx.clone(), gctx.clone())?);
//                 } else {
//                     if t_ty != Some(infer_sc(t, lctx.clone(), gctx.clone())?) {
//                         return Err(TypecheckingErrors::ExpectedSameInBranch)
//                     }
//                 }
//             }
//             // safe to unwrap since there is always atleast 1 case by construction
//             Ok(t_ty.unwrap())
//         }
//     }
// }

// fn infer_pattern<'a, T>(p: &'a Pattern<T>,
//                  lctx: RLCTX<'a, T>,
//                  gctx: RGCTX<'a, T>) -> TResult<(Option<RT<'a, T>>, RLCTX<'a,T>), T>
//     where T: Clone + PartialEq + std::fmt::Debug + BuiltIn
// {
//     match p {
//         Pattern::Default => Ok((None, lctx)),
//         Pattern::Symbol(x) => Ok((Some(get_type(x, lctx.clone(), gctx)?), lctx)),
//         Pattern::App(id, args) => {
//             let mut lctx = lctx;
//             let mut head = get_type(id, lctx.clone(), gctx.clone())?;
//             for _i in 0..args.len() {
//                 (head, lctx) = force_pi(head, lctx)?;
//             }
//             if let Type::Pi(..) = head.borrow() {
//                 return Err(TypecheckingErrors::NotPi)
//             }
//             Ok((Some(head), lctx))
//         }
//     }
// }
// fn force_pi<'a, T>(ty: RT<'a, T>, lctx: RLCTX<'a, T>)
//                    -> TResult<(RT<'a,T>, RLCTX<'a, T>), T>
// where T: Clone + PartialEq + std::fmt::Debug + BuiltIn {
//     match ty.borrow() {
//         Type::Pi(dom, ran) =>
//             Ok((ran(dom.clone())?, LocalContext::insert(dom.clone(), lctx))),
//         _ => Err(TypecheckingErrors::NotPi)
//     }
// }

// fn infer_num<'a, T>(sc: &'a NumericSC<AlphaTermSC<T>>,
//                  lctx: RLCTX<'a, T>,
//                  gctx: RGCTX<'a, T>) -> ResRT<'a, T>
//     where T: Clone + PartialEq + std::fmt::Debug + BuiltIn
// {
//     match sc {
//         Sum(x, y) | Prod(x, y) | Div(x, y) => {
//             let x_ty = infer_sc(x, lctx.clone(), gctx)?;
//             let y_ty = infer_sc(y, lctx, gctx)?;
//             let x_bor = x_ty.borrow();
//             if Type::ZT != *x_bor || Type::QT != *x_bor {
//                 return Err(TypecheckingErrors::ExpectedNum)
//             };
//             if *x_bor != *y_ty.borrow() {
//                 return Err(TypecheckingErrors::ExpectedSameNum)
//             };
//             Ok(x_ty)
//         }
//         Neg(x) => {
//             let x_ty = infer_sc(x, lctx.clone(), gctx)?;
//             let x_bor = x_ty.borrow();
//             if Type::ZT != *x_bor || Type::QT != *x_bor {
//                 return Err(TypecheckingErrors::ExpectedNum)
//             };
//             Ok(x_ty)
//         },
//         ZtoQ(z) => {
//             let z_ty = infer_sc(z, lctx.clone(), gctx)?;
//             if Type::ZT != *z_ty.borrow() {
//                 return Err(TypecheckingErrors::NotZ)
//             }
//             Ok(z_ty)
//         },
//         ZBranch { n, tbranch, fbranch } | NegBranch { n, tbranch, fbranch } => {
//             let n_ty = infer_sc(n, lctx.clone(), gctx)?;
//             let n_ty = n_ty.borrow();
//             if Type::ZT != *n_ty || Type::QT != *n_ty {
//                 return Err(TypecheckingErrors::ExpectedNum)
//             };
//             let tbranch_ty = infer_sc(tbranch, lctx.clone(), gctx)?;
//             let fbranch_ty = infer_sc(fbranch, lctx.clone(), gctx)?;
//             if *&tbranch_ty != *&fbranch_ty {
//                 return Err(TypecheckingErrors::ExpectedSameInBranch)
//             };
//             Ok(tbranch_ty)
//         }
//     }
// }
