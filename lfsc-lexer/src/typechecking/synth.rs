use lfsc_syntax::ast::{AlphaTerm, FromLit};
use lfsc_syntax::ast::AlphaTerm::*;
use super::nbe::{eval, eval_closure};
use super::values::{TResult, Type, as_pi};
use super::context::{Context, Key, RLCTX};
use super::check::check;

use std::rc::Rc;

pub fn synth<'a, T>(term: &'a AlphaTerm<T>, ctx: RLCTX<'a,T>) -> TResult<Rc<Type<'a, T>>>
where   T: PartialEq + Clone + FromLit + std::fmt::Debug,
{
    match term {
        Literal(l) =>
            ctx.borrow().get_type(Key::Name(FromLit::from_lit(l.clone()))),
        Var(x) =>
            ctx.borrow().get_type(Key::Name(x.clone())),
        DBI(i) =>
            ctx.borrow().get_type(Key::DBI(*i)),
        AlphaTerm::Pi(t1, t2) => {
            let tmp = Rc::new(Type::Kind);
            check(t1, tmp.clone(), ctx.clone())?;
            let val = eval(t1, ctx.clone())?;
            ctx.borrow_mut().insert(val);
            check(t2, tmp, ctx)?;
            Ok(Rc::new(Type::Kind))
        }
        AlphaTerm::Lam(_) | AnnLam(..) => {
            todo!("cannot synth a lambda");
        }
        App(t1, t2) => {
            use std::borrow::Borrow;
            let f_ty = synth(t1, ctx.clone())?;
            let (a,b) = as_pi(f_ty.borrow())?;
            check(t2, a.clone(), ctx.clone())?;
            let res_clo = eval_closure(b.clone(), eval(t2, ctx.clone())?)?;
            Ok(res_clo)
        }
        Asc(t1, t2) => {
            let ty = synth(t1, ctx.clone())?;
            check(t2, ty.clone(), ctx)?;
            Ok(ty)
        }
        SC(t1, t2) => {
            todo!()
        }
    }
}
