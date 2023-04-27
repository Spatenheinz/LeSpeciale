use lfsc_syntax::ast::{AlphaTerm, BuiltIn};
use lfsc_syntax::ast::AlphaTerm::*;
use super::EnvWrapper;
use super::values::{Value, TResult, RT, Neutral, TypecheckingErrors};

use std::rc::Rc;
use std::borrow::Borrow;


impl<'ctx, T> EnvWrapper<'ctx, T>
where T: PartialEq + std::fmt::Debug + Copy + BuiltIn
{
    pub fn check(&self,
                 term: &'ctx AlphaTerm<T>,
                 tau: RT<'ctx, T>) -> TResult<(), T>
    {
        match term {
            AlphaTerm::Lam(body) => {
                if let Value::Pi(a,b) = tau.borrow() {
                    let val = b(Rc::new(Value::Neutral(a.clone(),
                                        Rc::new(Neutral::DBI(0)))),
                                self)?;
                    let env = self.update_local(val.clone());
                    return env.check(body, val)
                }
                Err(TypecheckingErrors::NotPi)
            },
            SC(t1, t2) => {
                todo!()
            },
            // Fv, Bv, Ascription, PI, Annotated, etc.
            _ => {
                let t = self.infer(term)?;
                self.convert(t, tau, self.gctx.kind.clone())
            }
        }
    }
    fn convert(&self,
               t1: RT<'ctx, T>,
               t2: RT<'ctx, T>,
               tau: RT<'ctx, T>) -> TResult<(), T>
    {
        let e1 = self.readback(tau.clone(), t1)?;
        let e2 = self.readback(tau.clone(), t2)?;
        if e1 == e2 {
            Ok(())
        } else {
            Err(TypecheckingErrors::Mismatch(e1, e2))
        }

    }

}
// pub fn check<'a, T>(term: &'a AlphaTerm<T>, tau: RT<'a, T>,
//                     lctx: Rlctx<'a, T>,
//                     gctx: Rgctx<'a, T>) -> TResult<(), T>
// where
//     T: PartialEq + Clone + BuiltIn + std::fmt::Debug,
// {
//     match term {
//         AlphaTerm::Lam(body) => {
//             if let Value::Pi(a,b) = tau.borrow() {
//                 let val = b(Rc::new(Value::Neutral(a.clone(),
//                                         Rc::new(Neutral::DBI(0)))), gctx.clone())?;
//                 let ctx1 = LocalContext::insert(a.clone(), lctx.clone());
//                 return check(body, val, ctx1, gctx)
//             }
//             Err(TypecheckingErrors::NotPi)
//         },
//         SC(t1, t2) => {
//             todo!()
//         },
//         // Fv, Bv, Ascription, PI, Annotated, etc.
//         _ => {
//             let t = infer(term, lctx.clone(), gctx.clone())?;
//             convert(t, tau, gctx.kind.clone(),  lctx, gctx)
//         }
//     }
// }

