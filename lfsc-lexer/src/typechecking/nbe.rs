use super::EnvWrapper;
use super::values::{Neutral, Normal, Value, RT, ResRT};
use lfsc_syntax::ast::{AlphaTerm, Num, BuiltIn};
use lfsc_syntax::ast::Ident::*;
use lfsc_syntax::ast::AlphaTerm::*;

use std::rc::Rc;
use std::borrow::Borrow;

impl<'global, 'ctx, T> EnvWrapper<'global, 'ctx, T>
where T: PartialEq + std::fmt::Debug + BuiltIn + Copy
{
    pub fn eval(&self, term: &'ctx AlphaTerm<T>) -> ResRT<'ctx, T>
    {
        use super::values::mk_closure;

            match term {
                Number(Num::Z(p)) => Ok(Rc::new(Value::Z(*p))),
                Number(Num::Q(p,q)) => Ok(Rc::new(Value::Q(*p, *q))),
                Hole => {
                    todo!()
                },
                Ident(Symbol(name)) => self.gctx.get_value(name),
                Ident(DBI(i)) => {
                    if self.allow_dbi <= *i {
                        return self.lctx.get_value(*i);
                    }
                    // TODO fix panic
                    panic!("dbi: {} is not allowed", i)
                }
                App(fun, arg) => {
                    let e1 = self.eval(fun)?;
                    let e2 = self.eval(arg)?;
                    self.do_app(e1, e2)
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
                SC(..) => panic!("eval SC"),
            }
    }

    pub fn do_app(&self, f: RT<'ctx, T>, arg: RT<'ctx, T>) -> ResRT<'ctx, T>
    {
    match f.borrow() {
        Value::Lam(closure) => closure(arg, self.gctx, self.allow_dbi),
        Value::Neutral(f, neu) => {
            if let Value::Pi(dom, ran) = f.borrow() {
                Ok(Rc::new(Value::Neutral(
                    ran(arg.clone(), self.gctx, self.allow_dbi)?,
                    Rc::new(Neutral::App(neu.clone(), Normal(dom.clone(), arg))))))
            } else {
                todo!("This should be an error")
            }
        }
        _ => todo!("This should be an error"),
    }
    }
}

