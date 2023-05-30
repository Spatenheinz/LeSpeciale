use super::EnvWrapper;
use super::values::{Neutral, Normal, Value, RT, ResRT};
use lfsc_syntax::ast::{AlphaTerm, Num, BuiltIn};
use lfsc_syntax::ast::AlphaTerm::*;
use lfsc_syntax::free::FreeVar;
use crate::typechecking::errors::TypecheckingErrors::*;

use std::rc::Rc;
use std::borrow::Borrow;

impl<'global, 'ctx, T> EnvWrapper<'global, 'ctx, T>
where T: BuiltIn
{
    pub fn eval(&self, term: &'ctx AlphaTerm<T>) -> ResRT<'ctx, T>
    {
        use super::values::mk_closure;

        match term {
            Number(Num::Z(p)) => Ok(Rc::new(Value::Z(*p))),
            Number(Num::Q(p,q)) => Ok(Rc::new(Value::Q(*p, *q))),
            Ident(ident) => self.get_value(ident),
            App(fun, arg) => {
                let mut e1 = self.eval(fun)?;
                for a in arg.iter() {
                    let e2 = self.eval(a)?;
                    e1 = self.do_app(e1, e2)?;
                };
                Ok(e1)
            },
            Let(m,n) => {
                self.insert_local(self.eval(m)?).eval(n)
            }
            Pi(ty, body) => {
                let dom =
                    if let SC(sc, term) = &**ty {
                        let t_ty = self.eval(term)?;
                        let sc = self.run_sc(sc)?;
                        self.same(sc.clone(), t_ty.clone())?;
                        return self.insert_local(t_ty).eval(body)
                } else {
                    self.eval(ty)?
                };
                let ran = mk_closure(body, self.lctx.clone());
                return Ok(Rc::new(Value::Pi(body.free_in(0), dom, ran)));
            },
            Lam(body) | AnnLam(_, body) => {
                let closure = mk_closure(body, self.lctx.clone());
                Ok(Rc::new(Value::Lam(closure)))
            },
            Asc(_, val) => self.eval(val),
            SC(..) => Err(CannotEvalSC),
            Hole => Err(CannotEvalHole),
        }
    }

    pub fn do_app(&self, f: RT<'ctx, T>, arg: RT<'ctx, T>) -> ResRT<'ctx, T>
    {
    match f.borrow() {
        Value::Lam(closure) => closure(arg, self.gctx),
        Value::Neutral(f, neu) => {
            if let Value::Pi(_,dom, ran) = f.borrow() {
                Ok(Rc::new(Value::Neutral(
                    ran(arg.clone(), self.gctx)?,
                    Rc::new(Neutral::App(neu.clone(), Normal(dom.clone(), arg))))))
            } else {
                Err(NotPi)
            }
        }
        _ => Err(NotPi)
    }
    }
}

