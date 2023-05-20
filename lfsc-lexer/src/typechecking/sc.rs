use std::borrow::Borrow;
use std::rc::Rc;

use lfsc_syntax::ast::{AlphaTermSC, Num, AlphaNumericSC, AlphaCompoundSC,
                      AlphaSideEffectSC, BuiltIn, AlphaPattern, Ident};
use lfsc_syntax::ast::Ident::*;
use lfsc_syntax::ast::AlphaTermSC::*;
use lfsc_syntax::ast::NumericSC::*;
use lfsc_syntax::ast::CompoundSC::*;
use lfsc_syntax::ast::SideEffectSC::*;
use crate::typechecking::values::{as_neutral, flatten, ref_compare};

use super::EnvWrapper;
use super::errors::TypecheckingErrors::{NaN, DivByZero, ReachedFail, NoMatch};
// use super::context::{RLCTX, RGCTX, set_mark, get_mark, LocalContext};
use super::values::{ResRT, Value, as_symbolic, TResult};

use std::hash::Hash;

impl<'global, 'term, T> EnvWrapper<'global, 'term, T>
where T: Eq + Ord + Hash + std::fmt::Debug + Copy + BuiltIn
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
            App(m, n) => {
              let mut fun = self.get_value(m)?;
              let mut args = Vec::with_capacity(n.len());
              for e in n {
                args.push(self.run_sc(e)?);
              }
              if let Value::Prog(_, body) = fun.borrow() {
                  let mut env = self.clone();
                  for e in args {
                    env = env.insert_local(e);
                  }
                  let res = env.run_sc(body)?;
                return Ok(res);
              };
              for e in args {
                fun = self.do_app(fun, e)?
              };
              Ok(fun)
            }
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
            let a = self.run_sc(a)?;
            let b = self.run_sc(b)?;
          // TODO not good to use ref_compare. In case where holes are not filled we dont want to fill them.
            self.run_sc( if ref_compare(a, b) { tbranch } else { fbranch })
        }
        Match(scrut, cases) => {
            let scrut = self.run_sc(scrut)?;
            let scrut1 = as_neutral(&scrut)?;
            let (c,xs) = flatten(&scrut1)?;
            for (p,s) in cases {
                let mut env = self.clone();
                let (ci, si) = env.match_pattern(p)?;
                if c == ci && xs.len() == (si as usize) {
                   for x in xs.iter() {
                       env = env.insert_local(x.clone());
                   }
                   return env.run_sc(s);
                }
                if Symbol(T::_default()) == ci {
                    return env.run_sc(s);
                }
            };
          Err(NoMatch)
        }
        }
    }

    #[inline(always)]
    fn match_pattern(&self, p: &'term AlphaPattern<T>)
                     -> TResult<(Ident<T>, u32), T>
    {
      match p {
        AlphaPattern::Default => Ok((Ident::Symbol(T::_default()), 0)),
        AlphaPattern::Symbol(c) => Ok((c.clone(), 0)),
        AlphaPattern::App(c, xs) => Ok((c.clone(), *xs)),
      }
    }

    fn run_num(&self, sc: &'term AlphaNumericSC<T>) -> ResRT<'term, T>
    {
        match sc {
            Sum(..) | Prod(..) | Div(..) => self.binop(sc),
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
                        Value::Q(q,_) => todo!("error on q"),
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
