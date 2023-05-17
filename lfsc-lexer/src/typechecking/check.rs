use lfsc_syntax::ast::{AlphaTerm, BuiltIn, AlphaTermSC};
use super::EnvWrapper;
use super::errors::TypecheckingErrors;
use super::values::{Value, TResult, RT, mk_neutral_var_with_type, ResRT};

use std::borrow::Borrow;

impl<'global, 'ctx, T> EnvWrapper<'global, 'ctx, T>
where T: PartialEq + std::fmt::Debug + Copy + BuiltIn
{
    pub fn check(&self, term: &'ctx AlphaTerm<T>, tau: RT<'ctx, T>) -> TResult<(), T>
    {
        match term {
            AlphaTerm::Lam(body) => {
                if let Value::Pi(a,b) = tau.borrow() {
                    let val = b(mk_neutral_var_with_type(a.clone()),
                                self.gctx, self.allow_dbi, self.hole_count.clone())?;
                    let env = self.update_local(a.clone());
                    // println!("val: {:?}", val);
                    return env.check(body, val)
                }
                Err(TypecheckingErrors::NotPi)
            },
            _ => {
                let t = self.infer(term)?;
                self.same(t, tau)
            }
        }
    }

    pub fn check_sc(&self, sc: &'ctx AlphaTermSC<T>, tau: RT<'ctx, T>) -> ResRT<'ctx, T>
    {
        let t = self.infer_sc(sc)?;
        self.same(t.clone(), tau)?;
        Ok(t)
    }
}
