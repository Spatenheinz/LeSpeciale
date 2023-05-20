use lfsc_syntax::ast::{AlphaTerm, Num, BuiltIn, AlphaTermSC, AlphaNumericSC,
                       AlphaCompoundSC, AlphaSideEffectSC, AlphaPattern, Ident};
use lfsc_syntax::ast::AlphaTerm::*;
use lfsc_syntax::ast::NumericSC::*;
use lfsc_syntax::ast::CompoundSC::*;
use lfsc_syntax::ast::SideEffectSC::*;
use crate::typechecking::values::{const_closure, Neutral};

use super::EnvWrapper;
use super::errors::TypecheckingErrors;
use super::values::{ResRT, Type, RT, TResult, is_Z_or_Q};

use std::cell::RefCell;
use std::rc::Rc;
use std::borrow::Borrow;

use std::hash::Hash;

impl<'global, 'term, T> EnvWrapper<'global, 'term, T>
where T: Eq + Ord + Hash + std::fmt::Debug + Copy + BuiltIn
{
    pub fn infer(&mut self, term: &'term AlphaTerm<T>) -> ResRT<'term, T> {
        match term {
            Number(Num::Z(_))  => self.gctx.get_value(&T::_mpz()),
            Number(Num::Q(..)) => self.gctx.get_value(&T::_mpq()),
            Ident(x)   => self.get_type(x),
            AlphaTerm::Pi(a, b) => {
                let cut = self.lctx.len();
                let val =
                    if let SC(t1, t2) = &**a {
                        let t1_ty = self.infer_sc(t1)?;
                        match &**t2 {
                            Ident(Ident::Symbol(_name)) => {
                                Rc::new(Type::Run(t1, t1_ty, self.lctx.clone()))
                            },
                            _ => {
                            self.check(t2, t1_ty.clone())?;
                            Rc::new(Type::Run(t1, t1_ty, self.lctx.clone()))
                            }
                        }
                    } else {
                        self.infer_as_type(a)?;
                        self.eval(a)?
                    };
                self.lctx.truncate(cut); // domain can mess up the environment
                self.update_local(val);
                self.infer_sort(b)
            },
            Let(m, n) => {
                let m_ty = self.infer(m)?;
                // again we can insert because it is straight
                let m = self.eval(m)?;
                self.define_local(m_ty, m);
                self.infer(n)
            },
            AnnLam(a, m) => {
                self.infer_as_type(a)?;
                let val = self.eval(a)?;
                self.update_local(val.clone());
                let closure = const_closure(self.infer(m)?);
                Ok(Rc::new(Type::Pi(val, closure)))
            }
            App(f, args) => {
                // pi types have their own environment so we dont need to worry about
                // truncating
                let mut f_ty = self.infer(f)?;
                for n in args {
                    f_ty = if let Type::Pi(a,b) = f_ty.borrow() {
                     if Hole == *n {
                         let c = self.hole_count.get();
                         self.hole_count.set(c + 1);
                         let hole = Rc::new(Neutral::Hole(RefCell::new(None), c));
                         b(Rc::new(Type::Neutral(a.clone(), hole)),
                           self.gctx, self.allow_dbi, self.hole_count.clone())?
                     } else {
                        self.check(n, a.clone())?;
                        b(self.eval(n)?, self.gctx, self.allow_dbi, self.hole_count.clone())?
                     }
                    } else {
                        return Err(TypecheckingErrors::NotPi)
                    }
                };
                if let Type::Pi(a, b) = f_ty.borrow() {
                    if let Type::Run(sc, t, lctx) = a.borrow() {
                        // not the best
                        let mut env = EnvWrapper::new(lctx.clone(), self.gctx, self.allow_dbi, self.hole_count.clone());
                        let sc = env.run_sc(sc)?;
                        env.same(sc, t.clone())?;
                        return b(t.clone(), env.gctx, env.allow_dbi, env.hole_count.clone());
                    }
                }
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

    pub fn infer_as_type(&mut self, term: &'term AlphaTerm<T>) -> ResRT<'term, T> {
        let x = self.infer(term)?;
        match x.borrow() {
            Type::Star => Ok(x),
            _ => Err(TypecheckingErrors::ExpectedSort)
        }
    }
    pub fn infer_sort(&mut self, term: &'term AlphaTerm<T>) -> ResRT<'term, T> {
        let x = self.infer(term)?;
        match x.borrow() {
            Type::Box | Type::Star => Ok(x),
            _ => Err(TypecheckingErrors::ExpectedSort)
        }
    }

    pub fn infer_sc(&mut self, sc: &'term AlphaTermSC<T>) -> ResRT<'term, T> {
        match sc {
        AlphaTermSC::Number(Num::Z(_)) => self.gctx.get_value(&T::_mpz()),
        AlphaTermSC::Number(Num::Q(..)) => self.gctx.get_value(&T::_mpq()),
        AlphaTermSC::Ident(x) => self.get_type(x),
        AlphaTermSC::Let(m, n) => {
            let m_ty = self.infer_sc(m)?;
            self.update_local(m_ty);
            self.infer_sc(n)
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
            let dbi = self.allow_dbi;
            for arg in args.iter() {
                 if let Type::Pi(a,b) = f_ty.borrow() {
                     // 1. check arg matches type of function
                    // let arg_ty = env.infer_sc(arg)?;
                    self.check_sc(arg, a.clone())?;
                     // 2. We dont allow x in the
                    self.allow_dbi += 1;
                     // hacky way to force evaluation
                    f_ty = b(self.gctx.kind.clone(), self.gctx, self.allow_dbi, self.hole_count.clone())?;
                 } else {
                    return Err(TypecheckingErrors::NotPi);
                 }
            }
            self.allow_dbi = dbi;
            Ok(f_ty)
        },
        AlphaTermSC::Numeric(num) => self.infer_num(num),
        AlphaTermSC::Compound(com) => self.infer_compound(com),
        AlphaTermSC::SideEffect(se) => self.infer_sideeffect(se)
        }
    }

    fn infer_sideeffect(&mut self, sc: &'term AlphaSideEffectSC<T>) -> ResRT<'term, T>
    {
        match sc {
            Do(a, b) => { self.infer_sc(a)?; self.infer_sc(b) },
            MarkVar(_, var) => self.infer_sc(var),
            IfMarked{n: _n, c, tbranch, fbranch} => {
                self.infer_sc(c)?;
                let tbranch = self.infer_sc(tbranch)?;
                self.check_sc(fbranch, tbranch)
            }
        }
    }

    fn infer_compound(&mut self, sc: &'term AlphaCompoundSC<T>) -> ResRT<'term, T>
    {
        match sc {
            Fail(x) => { self.infer_as_type(x)?; Ok(self.eval(x)?) },
            IfEq { a, b, tbranch, fbranch } => {
                let a = self.infer_sc(a)?;
                self.check_sc(b, a)?;
                let tbranch = self.infer_sc(tbranch)?;
                self.check_sc(fbranch, tbranch)
            }
            Match(scrut, cases) => {
                let scrut_ty = self.infer_sc(scrut)?;
                let mut t_ty : Option<RT<_>> = None;
                let lctx_cut = self.lctx.len();
                for i in cases.iter() {
                    let (p, t) = i;
                    // local scope
                    let pat_ty = self.infer_pattern(p)?;
                    if let Some(p_ty) = pat_ty {
                        self.same(p_ty, scrut_ty.clone())?;
                    };
                    let cur_ty = self.infer_sc(t)?;
                    if let Some(t) = t_ty {
                        self.same(t.clone(), cur_ty.clone())?;
                        t_ty = Some(t);
                    } else {
                        t_ty = Some(cur_ty);
                    }
                    //reset local scope
                    self.lctx.truncate(lctx_cut);
                }
                // // safe to unwrap since there is always atleast 1 case by construction
                Ok(t_ty.unwrap())
            }
        }
    }

    fn infer_pattern(&mut self,
                     p: &'term AlphaPattern<T>,
                    ) -> TResult<Option<RT<'term, T>>, T>
    {
        match p {
            AlphaPattern::Default => Ok(None),
            AlphaPattern::Symbol(x) => Ok(Some(self.get_type(x)?)),
            AlphaPattern::App(id, args) => {
                let mut head = self.get_type(id)?;
                for _i in 0..*args {
                    head = self.force_pi(head)?;
                }
                if let Type::Pi(..) = head.borrow() {
                    return Err(TypecheckingErrors::NotFullyApplied)
                }
                Ok(Some(head))
            }
        }
    }

    // indirection to since we might now assign to a borrow...
    fn force_pi(&mut self, ty: RT<'term, T>) -> TResult<RT<'term, T>, T> {
        if let Type::Pi(dom, ran) = ty.borrow() {
            self.update_local(dom.clone());
            Ok(ran(dom.clone(), self.gctx, self.allow_dbi, self.hole_count.clone())?)
        } else {
            Err(TypecheckingErrors::NotPi)
        }
    }

    fn infer_num(&mut self, sc: &'term AlphaNumericSC<T>) -> ResRT<'term, T>
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
                let tb_ty = self.infer_sc(tbranch)?;
                self.check_sc(fbranch, tb_ty)
            }
        }
    }
}




