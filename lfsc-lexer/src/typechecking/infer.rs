use lfsc_syntax::ast::{AlphaTerm, Num, BuiltIn, AlphaTermSC, AlphaNumericSC,
                       AlphaCompoundSC, AlphaSideEffectSC, AlphaPattern, Ident};
use lfsc_syntax::ast::AlphaTerm::*;
use lfsc_syntax::ast::NumericSC::*;
use lfsc_syntax::ast::CompoundSC::*;
use lfsc_syntax::ast::SideEffectSC::*;
use crate::typechecking::values::{const_closure, Neutral};

use super::EnvWrapper;
use super::values::{TypecheckingErrors, ResRT, Type, RT, TResult};

use std::cell::RefCell;
use std::rc::Rc;
use std::borrow::Borrow;


impl<'global, 'ctx, T> EnvWrapper<'global, 'ctx, T>
where T: PartialEq + std::fmt::Debug + Copy + BuiltIn
{
    pub fn infer(&self, term: &'ctx AlphaTerm<T>) -> ResRT<'ctx, T> {
        println!("infer: {:?}", term);
        match term {
            Number(Num::Z(_))  => self.gctx.get_value(&T::_mpz()),
            Number(Num::Q(..)) => self.gctx.get_value(&T::_mpq()),
            Ident(x)   => self.get_type(x),
            AlphaTerm::Pi(a, b) => {
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
                let env = self.update_local(val);
                env.infer_sort(b)
            },
            AnnLam(a, m) => {
                self.infer_as_type(a)?;
                let val = self.eval(a)?;
                let env = self.update_local(val.clone());
                let t2 = env.infer(m)?;
                let closure = const_closure(t2);
                Ok(Rc::new(Type::Pi(val, closure)))
            }
            App(f, args) => {
                let mut f_ty = self.infer(f)?;
                for n in args {
                    f_ty = if let Type::Pi(a,b) = f_ty.borrow() {
                     if Hole == *n {
                         let hole = Rc::new(Neutral::Hole(RefCell::new(None)));
                         println!("holes: {:?}", hole);
                         b(Rc::new(Type::Neutral(a.clone(), hole)), self.gctx, self.allow_dbi)?
                     } else {
                        self.check(n, a.clone())?;
                        println!("checked {:?} against {:?}", n, self.readback(self.gctx.kind.clone(), a.clone()));
                        b(self.eval(n)?, self.gctx, self.allow_dbi)?
                     }
                    } else {
                        return Err(TypecheckingErrors::NotPi)
                    }
                };
                if let Type::Pi(a, b) = f_ty.borrow() {
                    if let Type::Run(sc, t, lctx) = a.borrow() {
                        let env = EnvWrapper::new(lctx.clone(), self.gctx, self.allow_dbi);
                        let sc = env.run_sc(sc)?;
                        env.same(sc, t.clone())?;
                        println!("sc");
                        return b(t.clone(), self.gctx, self.allow_dbi);
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

    pub fn infer_as_type(&self, term: &'ctx AlphaTerm<T>) -> ResRT<'ctx, T> {
        let x = self.infer(term)?;
        match x.borrow() {
            Type::Star => Ok(x),
            _ => Err(TypecheckingErrors::ExpectedSort)
        }
    }
    pub fn infer_sort(&self, term: &'ctx AlphaTerm<T>) -> ResRT<'ctx, T> {
        let x = self.infer(term)?;
        match x.borrow() {
            Type::Box | Type::Star => Ok(x),
            _ => Err(TypecheckingErrors::ExpectedSort)
        }
    }

    pub fn infer_sc(&self, sc: &'ctx AlphaTermSC<T>) -> ResRT<'ctx, T> {
        match sc {
        AlphaTermSC::Number(Num::Z(_)) => self.gctx.get_value(&T::_mpz()),
        AlphaTermSC::Number(Num::Q(..)) => self.gctx.get_value(&T::_mpq()),
        AlphaTermSC::Ident(x) => self.get_type(x),
        AlphaTermSC::Let(m, n) => {
            let m_ty = self.infer_sc(m)?;
            let env = self.update_local(m_ty);
            env.infer_sc(n)
        },
        AlphaTermSC::App(f, args) => {
            let mut f_ty = self.get_type(f)?;
            let mut env = self.clone();
            // Since im too stupid to make a program a Pi type, we do the following:
            if let Type::Prog(params, _) = self.get_value(f)?.borrow() {
                if args.len() != args.len() {
                    return Err(TypecheckingErrors::WrongNumberOfArguments);
                }
                for (arg,param) in args.iter().zip(params.iter()) {
                    let arg = self.infer_sc(arg)?;
                    self.same(arg, param.clone())?;
                }
                // TODO: do we really need to check the body again?
                return Ok(f_ty)
            }
            // The case for PI
            for arg in args.iter() {
                 if let Type::Pi(a,b) = f_ty.borrow() {
                     // 1. check arg matches type of function
                    let arg_ty = env.infer_sc(arg)?;
                    env.same(arg_ty, a.clone())?;
                     // 2. We dont allow x in the
                    env.allow_dbi += 1;
                     // hacky way to force evaluation
                    f_ty = b(env.gctx.kind.clone(), env.gctx, env.allow_dbi)?;
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

    fn infer_sideeffect(&self, sc: &'ctx AlphaSideEffectSC<T>) -> ResRT<'ctx, T>
    {
        match sc {
            Do(a, b) => {
                self.infer_sc(a)?;
                self.infer_sc(b)
            },
            MarkVar(_, var) => {
                let var_ty = self.infer_sc(var)?;
                Ok(var_ty)
            },
            IfMarked{n: _n, c, tbranch, fbranch} => {
                let _var_ty    = self.infer_sc(c)?;
                let tbranch_ty = self.infer_sc(tbranch)?;
                let fbranch_ty = self.infer_sc(fbranch)?;
                self.same(tbranch_ty.clone(), fbranch_ty)?;
                Ok(tbranch_ty)
            }
        }
    }

    fn infer_compound(&self, sc: &'ctx AlphaCompoundSC<T>) -> ResRT<'ctx, T>
    {
        match sc {
            Fail(x) => { self.infer_as_type(x)?; Ok(self.eval(x)?) },
            IfEq { a, b, tbranch, fbranch } => {
                let a_ty = self.infer_sc(a)?;
                let b_ty = self.infer_sc(b)?;
                self.same(a_ty, b_ty)?;
                let tbranch_ty = self.infer_sc(tbranch)?;
                let fbranch_ty = self.infer_sc(fbranch)?;
                self.same(tbranch_ty.clone(), fbranch_ty)?;
                Ok(tbranch_ty)
            }
            Match(scrut, cases) => {
                println!("match: {:?}", cases);
                let scrut_ty = self.infer_sc(scrut)?;
                let mut t_ty : Option<RT<_>> = None;
                for i in cases.iter() {
                    let env = self.clone();
                    let (p, t) = i;
                    let (pat_ty, local_env) = env.infer_pattern(p)?;
                    if let Some(p_ty) = pat_ty {
                        env.same(p_ty, scrut_ty.clone())?;
                    };
                    println!("BOEFORE:\n\n");
                    let cur_ty = local_env.infer_sc(t)?;
                    env.same(cur_ty.clone(), scrut_ty.clone())?;
                    if t_ty.is_none() {
                        t_ty = Some(cur_ty);
                    }
                }
                // // safe to unwrap since there is always atleast 1 case by construction
                Ok(t_ty.unwrap())
            }
        }
    }

    fn infer_pattern(&self,
                     p: &'ctx AlphaPattern<T>,
                    ) -> TResult<(Option<RT<'ctx, T>>, Self), T>
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
    fn force_pi(&self, ty: RT<'ctx, T>) -> TResult<(RT<'ctx, T>, Self), T> {
        if let Type::Pi(dom, ran) = ty.borrow() {
            println!("dom: {:?}\n\n", dom);
            Ok((ran(dom.clone(), self.gctx, self.allow_dbi)?, self.update_local(dom.clone())))
        } else {
            Err(TypecheckingErrors::NotPi)
        }
    }

    fn infer_num(&self, sc: &'ctx AlphaNumericSC<T>) -> ResRT<'ctx, T>
        where T: Clone + PartialEq + std::fmt::Debug + BuiltIn
    {
        match sc {
            Sum(x, y) | Prod(x, y) | Div(x, y) => {
                let x_ty = self.infer_sc(x)?;
                let y_ty = self.infer_sc(y)?;
                let x_bor = x_ty.borrow();
                if Type::ZT != *x_bor && Type::QT != *x_bor {
                    return Err(TypecheckingErrors::ExpectedNum)
                };
                if *x_bor != *y_ty.borrow() {
                    return Err(TypecheckingErrors::ExpectedSameNum)
                };
                Ok(x_ty)
            }
            Neg(x) => {
                let x_ty = self.infer_sc(x)?;
                let x_bor = x_ty.borrow();
                if Type::ZT != *x_bor && Type::QT != *x_bor {
                    return Err(TypecheckingErrors::ExpectedNum)
                };
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
                println!("n: {:?}\n\n", n);
                let n_ty = self.infer_sc(n)?;
                println!("n_ty: {:?}\n\n", n_ty);
                let n_ty = n_ty.borrow();
                if Type::ZT != *n_ty && Type::QT != *n_ty {
                // if Type::ZT != *n_ty || Type::QT != *n_ty {
                    return Err(TypecheckingErrors::ExpectedNum)
                };
                let tbranch_ty = self.infer_sc(tbranch)?;
                let fbranch_ty = self.infer_sc(fbranch)?;
                self.same(tbranch_ty.clone(), fbranch_ty)?;
                Ok(tbranch_ty)
            }
        }
    }
}




