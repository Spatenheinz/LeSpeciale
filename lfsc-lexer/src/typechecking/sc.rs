use std::borrow::Borrow;
use std::rc::Rc;

use lfsc_syntax::ast::{AlphaTermSC, Num, AlphaNumericSC, AlphaCompoundSC,
                      AlphaSideEffectSC, BuiltIn};
use lfsc_syntax::ast::Ident::*;
use lfsc_syntax::ast::AlphaTermSC::*;
use lfsc_syntax::ast::NumericSC::*;
use lfsc_syntax::ast::CompoundSC::*;
use lfsc_syntax::ast::SideEffectSC::*;
use super::EnvWrapper;
use super::values::TypecheckingErrors::{NaN, DivByZero, ReachedFail};
// use super::context::{RLCTX, RGCTX, set_mark, get_mark, LocalContext};
use super::values::{ResRT, Value, as_symbolic};


impl<'global, 'term, T> EnvWrapper<'global, 'term, T>
where T: PartialEq + std::fmt::Debug + Copy + BuiltIn
{
    pub fn run_sc(&self, sc: &'term AlphaTermSC<T>) -> ResRT<'term, T>
    {
        match sc {
            Number(Num::Z(p)) => Ok(Rc::new(Value::Z(*p))),
            Number(Num::Q(p,q)) => Ok(Rc::new(Value::Q(*p, *q))),
            // Little unsure about value vs type.
            Ident(DBI(i)) => self.lctx.get_value(*i),
            Ident(Symbol(x)) => self.gctx.get_value(x),
            Let(m, n) => {
                let m = self.run_sc(m)?;
                self.update_local(m).run_sc(n)
            },
            App(m, n) => todo!(),
            Numeric(num) => self.run_num(num),
            Compound(compound) => self.run_compound(compound),
            SideEffect(se) => self.run_sideeffect(se)
        }
    }

    fn run_sideeffect(&self, sc: &'term AlphaSideEffectSC<T>) -> ResRT<'term, T>
    {
        match sc {
        Do(a, b) => { self.run_sc(a)?; self.run_sc(b) },
        MarkVar(n, var) => {
            // TODO
            // by construction Markvar will be a variable.
            let var = self.run_sc(var)?;
            let v = as_symbolic(&var)?;
            // set_mark(v, *n, lctx, gctx);
            Ok(var)
            //var must be symbolic (meaning it is neutral?)
        },
        IfMarked{ n, c, tbranch, fbranch} => {
            // TODO
            let c = self.run_sc(c)?;
            let v = as_symbolic(&c)?;
            // let mark = self.get_mark(v, *n)?;
            // if mark == 1 {
                // self.run_sc(tbranch)
            // } else {
                self.run_sc(fbranch)
            // }
        }
        }
    }

    fn run_compound(&self, sc: &'term AlphaCompoundSC<T>) -> ResRT<'term, T>
    {
        match sc {
        Fail(_) => Err(ReachedFail),
        IfEq { a, b, tbranch, fbranch } => {
            todo!("figure out how to compare")
            // let a = run_sc(a, lctx, gctx)?;
            // let b = run_sc(b, lctx, gctx)?;
            // // TODO check if alpha equivalent is enough or should we check specifically with holes?
            // match (a.borrow(), b.borrow()) {
            //     (a,b) if *a == *b => run_sc(tbranch, lctx, gctx),
            //     (a,b) if *a != *b => run_sc(fbranch, lctx, gctx),
            //     (Value::ZT,_) => unreachable!() // only to dominate the typechecker
            // }
        }
        Match(scrut, cases) => {
            todo!("maybe easier after apply..")
        }
        }
    }

    fn run_num(&self, sc: &'term AlphaNumericSC<T>) -> ResRT<'term, T>
    {
        match sc {
            Sum(x, y) | Prod(x, y) | Div(x, y) => self.binop(sc),
            Neg(x) => {
                match self.run_sc(x)?.borrow() {
                    Value::Z(x) => Ok(Rc::new(Value::Z(-x))),
                    Value::Q(x, y) => Ok(Rc::new(Value::Q(-x, *y))),
                    _ => Err(NaN)
                }
            },
            ZtoQ(z) => todo!(),
            ZBranch { n, tbranch, fbranch } | NegBranch { n, tbranch, fbranch } => {
                let n = self.run_sc(n)?;
                let val = match n.borrow() {
                        Value::Z(x) => x,
                        Value::Q(q,_) => q,
                        _ => return Err(NaN)
                    };
                let cond = match sc {
                        ZBranch { .. } => *val == 0,
                        NegBranch { .. } => *val < 0,
                        _ => unreachable!(),
                        };
                self.run_sc(if cond { tbranch  } else { fbranch })
            }
        }
    }

    fn binop(&self, sc: &'term AlphaNumericSC<T>) -> ResRT<'term, T>
    {
        match sc {
        Sum(x, y) | Prod(x, y) | Div(x, y) => {
            let x = self.run_sc(x)?;
            let y = self.run_sc(y)?;
            if let Div(_,_) = sc {
                match y.borrow() {
                    Value::Z(0) | Value::Q(0,_) => return Err(DivByZero),
                    _ => ()
                }
            }
            match (x.borrow(), y.borrow()) {
            (Value::Z(x), Value::Z(y)) => {
                match sc {
                    Sum(_, _) => Ok(Rc::new(Value::Z(x + y))),
                    Prod(_, _) => Ok(Rc::new(Value::Z(x * y))),
                    Div(_, _) =>Ok(Rc::new(Value::Z(x / y))),
                    _ => unreachable!()
                    }
                },
            (Value::Q(x, y), Value::Q(a, b)) => {
                match sc {
                    Sum(_, _) => Ok(Rc::new(Value::Q(x * b + a * y, y * b))),
                    Prod(_, _) => Ok(Rc::new(Value::Q(x * a, y * b))),
                    Div(_, _) => Ok(Rc::new(Value::Q(x * b, y * a))),
                    _ => unreachable!()
                    }
                },
                _ => unreachable!()
            }
        }
        _ => unreachable!()
        }
    }
}
// // NOTE: SymS is bound in PI, ANNLAM, BigLam, Lam, Let and in toplevel.
// // Seams like it is simply a variable.
