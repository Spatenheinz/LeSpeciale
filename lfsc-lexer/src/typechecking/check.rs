use lfsc_syntax::ast::{AlphaTerm, BuiltIn, AlphaTermSC};
use lfsc_syntax::ast::AlphaTerm::*;
use super::EnvWrapper;
use super::values::{Value, TResult, RT, Neutral, TypecheckingErrors};

use std::rc::Rc;
use std::borrow::Borrow;

// #[macro_export]
// macro_rules! check {
//     ($node:tt, $term:ident, $tau:ident) => {
//             let t = self.infer_$node($term)?;
//             self.convert(t, $tau.clone(), self.gctx.kind.clone())
//     }
// };

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
            // Fv, Bv, Ascription, PI, Annotated, etc.
            _ => {
                let t = self.infer(term)?;
                println!("term: {:?} -- type in check: {:?} must be {:?}", term, t, tau);
                // self.convert(t, tau, Rc::new(Value::Star))
                self.convert(t, tau, self.gctx.kind.clone())
            }
        }
    }
}
