use lfsc_syntax::ast::{AlphaTerm, BuiltIn};
use super::EnvWrapper;
use super::values::{Value, TResult, RT, TypecheckingErrors, mk_neutral_var_with_type};

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
                                self.gctx, self.allow_dbi)?;
                    let env = self.update_local(val.clone());
                    return env.check(body, val)
                }
                Err(TypecheckingErrors::NotPi)
            },
            // Fv, Bv, Ascription, PI, Annotated, etc.
            _ => {
                let t = self.infer(term)?;
                println!("term: {:?} -- type in check: {:?} must be {:?}", term, t, tau);
                self.convert(t, tau, self.gctx.kind.clone())
            }
        }
    }
}
