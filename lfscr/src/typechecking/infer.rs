use lfsc_syntax::ast::{AlphaTerm, Num, BuiltIn, AlphaTermSC, AlphaNumericSC,
                       AlphaCompoundSC, AlphaSideEffectSC, AlphaPattern, Ident};
use lfsc_syntax::ast::AlphaTerm::*;
use lfsc_syntax::ast::NumericSC::*;
use lfsc_syntax::ast::CompoundSC::*;
use lfsc_syntax::ast::SideEffectSC::*;
use lfsc_syntax::free::FreeVar;
use crate::typechecking::values::{const_closure, Neutral};

use super::EnvWrapper;
use super::errors::TypecheckingErrors;
use super::values::{ResRT, Type, RT, TResult, is_Z_or_Q, as_type};

use std::cell::RefCell;
use std::rc::Rc;
use std::borrow::Borrow;

use std::hash::Hash;

impl<'global, 'term, T> EnvWrapper<'global, 'term, T>
where T: BuiltIn
{
    pub fn infer(&self, term: &'term AlphaTerm<T>) -> ResRT<'term, T> {
        match term {
            Number(Num::Z(_))  => self.gctx.get_value(&T::_mpz()),
            Number(Num::Q(..)) => self.gctx.get_value(&T::_mpq()),
            Ident(x)   => self.get_type(x),
            AlphaTerm::Pi(a, b) => {
                let val =
                    if let SC(t1, t2) = &**a {
                        let t1_ty = self.infer_sc(t1)?;
                        self.check(t2, t1_ty.clone())?;
                        Rc::new(Type::Run(t1, t1_ty, self.lctx.clone()))
                    } else {
                        self.infer_as_type(a)?;
                        self.eval(a)?
                    };
                let env = self.update_local(val);
                env.infer_sort(b)
            },
            Let(m, n) => {
                self.define_local(self.infer(m)?, self.eval(m)?).infer(n)
            },
            AnnLam(a, m) => {
                self.infer_as_type(a)?;
                let val = self.eval(a)?;
                let closure = const_closure(self.update_local(val.clone()).infer(m)?);
                Ok(Rc::new(Type::Pi(m.free_in(0), val, closure)))
            }
            App(f, args) => {
                let mut f_ty = self.infer(f)?;
                for n in args {
                    f_ty = if let Type::Pi(free,a,b) = f_ty.borrow() {
                     if Hole == *n {
                         let hole = Rc::new(Neutral::Hole(RefCell::new(None)));
                         b(Rc::new(Type::Neutral(a.clone(), hole)), self.gctx)?
                     } else {
                        self.check(n, a.clone())?;
                         let x = if *free { self.eval(n)? } else { a.clone() };
                        b(x, self.gctx)?
                     }
                    } else {
                        return Err(TypecheckingErrors::NotPi)
                    }
                };
                Ok(f_ty)
            },
            Asc(a, m) => {
                self.infer_sort(a)?;
                let ty = self.eval(a)?;
                self.check(m, ty.clone())?;
                Ok(ty)
            }
            AlphaTerm::Lam(_) => Err(TypecheckingErrors::CannotInferLambda),
            Hole => Err(TypecheckingErrors::CannotInferHole),
            SC(..) => Err(TypecheckingErrors::UnexpectedSC)
        }
    }

    pub fn infer_as_type(&self, term: &'term AlphaTerm<T>) -> ResRT<'term, T> {
        let x = self.infer(term)?;
        match x.borrow() {
            Type::Star => Ok(x),
            _ => Err(TypecheckingErrors::ExpectedSort)
        }
    }
    pub fn infer_sort(&self, term: &'term AlphaTerm<T>) -> ResRT<'term, T> {
        let x = self.infer(term)?;
        match x.borrow() {
            Type::Box | Type::Star => Ok(x),
            _ => Err(TypecheckingErrors::ExpectedSort)
        }
    }

    pub fn infer_sc(&self, sc: &'term AlphaTermSC<T>) -> ResRT<'term, T> {
        match sc {
        AlphaTermSC::Number(Num::Z(_)) => self.gctx.get_value(&T::_mpz()),
        AlphaTermSC::Number(Num::Q(..)) => self.gctx.get_value(&T::_mpq()),
        AlphaTermSC::Ident(x) => self.get_type(x),
        AlphaTermSC::Let(m, n) => {
            self.update_local(self.infer_sc(m)?).infer_sc(n)
        },
        AlphaTermSC::App(f, args) => {
            let mut f_ty = self.get_type(f)?;
            if let Type::Prog(params, _) = self.get_value(f)?.borrow() {
                if args.len() != args.len() {
                    return Err(TypecheckingErrors::WrongNumberOfArguments);
                }
                for (arg,param) in args.iter().zip(params.iter()) {
                    self.check_sc(arg, param.clone())?;
                }
                return Ok(f_ty)
            }
            // The case for PI
            for arg in args.iter() {
                 if let Type::Pi(_,a,b) = f_ty.borrow() {
                    self.check_sc(arg, a.clone())?;
                    f_ty = b(self.gctx.kind.clone(), self.gctx)?;
                 } else {
                    return Err(TypecheckingErrors::NotPi);
                 }
            }
            Ok(f_ty)
        },
        AlphaTermSC::Numeric(num) => self.infer_num(num),
        AlphaTermSC::Compound(com) => self.infer_compound(com),
        AlphaTermSC::SideEffect(se) => self.infer_sideeffect(se)
        }
    }

    fn infer_sideeffect(&self, sc: &'term AlphaSideEffectSC<T>) -> ResRT<'term, T>
    {
        match sc {
            Do(a, b) => { self.infer_sc(a)?; self.infer_sc(b) },
            MarkVar(_, var) => self.infer_sc(var),
            IfMarked{n: _n, c, tbranch, fbranch} => {
                self.infer_sc(c)?;
                self.check_sc(fbranch, self.infer_sc(tbranch)?)
            }
        }
    }

    fn infer_compound(&self, sc: &'term AlphaCompoundSC<T>) -> ResRT<'term, T>
    {
        match sc {
            Fail(x) => { self.infer_as_type(x)?; Ok(self.eval(x)?) },
            IfEq { a, b, tbranch, fbranch } => {
                self.check_sc(b, self.infer_sc(a)?)?;
                self.check_sc(fbranch, self.infer_sc(tbranch)?)
            }
            Match(scrut, cases) => {
                let scrut_ty = self.infer_sc(scrut)?;
                let mut t_ty : Option<RT<_>> = None;
                for i in cases.iter() {
                    let (p, t) = i;
                    let (pat_ty, local_env) = self.infer_pattern(p)?;
                    if let Some(p_ty) = pat_ty {
                        self.same(p_ty, scrut_ty.clone())?;
                    };
                    let cur_ty = local_env.infer_sc(t)?;
                    if let Some(t) = t_ty {
                        self.same(t.clone(), cur_ty.clone())?;
                        t_ty = Some(t);
                    } else {
                        t_ty = Some(cur_ty);
                    }
                }
                // // safe to unwrap since there is always atleast 1 case by construction
                Ok(t_ty.unwrap())
            }
        }
    }

    fn infer_pattern(&self,
                     p: &'term AlphaPattern<T>,
                    ) -> TResult<(Option<RT<'term, T>>, Self), T>
    {
        match p {
            AlphaPattern::Default => Ok((None, self.clone())),
            AlphaPattern::Symbol(x) => Ok((Some(self.get_type(x)?), self.clone())),
            AlphaPattern::App(id, args) => {
                let mut head = self.get_type(id)?;
                let mut env = self.clone();
                for _i in 0..*args {
                    (head, env) = env.force_pi(head)?;
                }
                if let Type::Pi(..) = head.borrow() {
                    return Err(TypecheckingErrors::NotFullyApplied)
                }
                Ok((Some(head), env))
            }
        }
    }

    // indirection to since we might now assign to a borrow...
    fn force_pi(&self, ty: RT<'term, T>) -> TResult<(RT<'term, T>, Self), T> {
        if let Type::Pi(_, dom, ran) = ty.borrow() {
            Ok((ran(dom.clone(), self.gctx)?, self.update_local(dom.clone())))
        } else {
            Err(TypecheckingErrors::NotPi)
        }
    }

    fn infer_num(&self, sc: &'term AlphaNumericSC<T>) -> ResRT<'term, T>
        where T: Clone + Eq + Ord + Hash + std::fmt::Debug + BuiltIn
    {
        match sc {
            Sum(x, y) | Prod(x, y) | Div(x, y) => {
                let x_ty = self.infer_sc(x)?;
                is_Z_or_Q(x_ty.borrow())?;
                self.check_sc(y, x_ty.clone())?;
                Ok(x_ty)
            }
            Neg(x) => {
                let x_ty = self.infer_sc(x)?;
                is_Z_or_Q(x_ty.borrow())?;
                Ok(x_ty)
            },
            ZtoQ(z) => {
                let z_ty = self.infer_sc(z)?;
                if Type::ZT != *z_ty.borrow() {
                    return Err(TypecheckingErrors::NotZ)
                }
                Ok(z_ty)
            },
            ZBranch { n, tbranch, fbranch }
            | NegBranch { n, tbranch, fbranch } => {
                let n_ty = self.infer_sc(n)?;
                is_Z_or_Q(n_ty.borrow())?;
                self.check_sc(fbranch, self.infer_sc(tbranch)?)
            }
        }
    }
}




