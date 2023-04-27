use super::EnvWrapper;
use super::values::{Neutral, Normal, Value, RT, ResRT};
use lfsc_syntax::ast::{AlphaTerm, Num, BuiltIn};
use lfsc_syntax::ast::Ident::*;
use lfsc_syntax::ast::AlphaTerm::*;

use std::rc::Rc;
use std::borrow::Borrow;

impl<'ctx, T> EnvWrapper<'ctx, T>
where T: PartialEq + std::fmt::Debug + BuiltIn + Copy
{
    pub fn eval(&self, term: &'ctx AlphaTerm<T>) -> ResRT<'ctx, T>
    {
        use super::values::mk_closure;

            match term {
                Number(Num::Z(p)) => Ok(Rc::new(Value::Z(*p))),
                Number(Num::Q(p,q)) => Ok(Rc::new(Value::Q(*p, *q))),
                Hole => {
                    // Notice that hole does not have type kind, however
                    // we will never get to use the type of a hole anyways.
                    // let refcell = RefCell::new(None);
                    // let neu = Rc::new(Neutral::Hole(refcell.clone()));
                    todo!()
                    // Ok(Rc::new(Value::Neutral(gctx.kind.clone(), neu)))
                },
                Ident(Symbol(name)) => self.gctx.get_value(name),
                Ident(DBI(i)) => self.lctx.get_value(*i),
                App(fun, arg) => {
                    let e1 = self.eval(fun)?;
                    let e2 = self.eval(arg)?;
                    do_app(e1, e2, self)
                },
                Pi(ty, body) => {
                    let dom = self.eval(ty)?;
                    let ran = mk_closure(body, self.lctx.clone());
                    return Ok(Rc::new(Value::Pi(dom, ran)));
                },
                Lam(body) | AnnLam(_, body) => {
                    let closure = mk_closure(body, self.lctx.clone());
                    Ok(Rc::new(Value::Lam(closure)))
                },
                Asc(_, val) => self.eval(val),
                SC(..) => todo!("eval SC"),
            }
    }
}

pub fn do_app<'ctx, T>(func: RT<'ctx, T>,
                       arg: RT<'ctx, T>,
                       env: &EnvWrapper<'ctx, T>) -> ResRT<'ctx, T>
where T: PartialEq + std::fmt::Debug + Copy
{
   match func.borrow() {
       Value::Lam(closure) => closure(arg, env),
       Value::Neutral(f, neu) => {
           if let Value::Pi(dom, ran) = f.borrow() {
               Ok(Rc::new(Value::Neutral(
                   ran(arg.clone(), env)?,
                   Rc::new(Neutral::App(neu.clone(), Normal(dom.clone(), arg))))))
           } else {
               todo!("This should be an error")
           }
       }
       _ => todo!("This should be an error"),
   }
}
