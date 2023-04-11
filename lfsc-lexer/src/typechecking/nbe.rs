use super::context::{Context, Key, RLCTX};
use super::values::{Closure, Neutral, Normal, Value, TResult, RT};
use lfsc_syntax::ast::{AlphaTerm, Num, TermLiteral};
use lfsc_syntax::ast::AlphaTerm::*;
use lfsc_syntax::ast::TermLiteral::*;

use std::rc::Rc;
use std::borrow::Borrow;
use std::cell::RefCell;

pub fn eval<'a, T>(term: &'a AlphaTerm<T>, ctx: RLCTX<'a, T>)
                   -> TResult<Rc<Value<'a, T>>>
where
    T: PartialEq + Clone + std::fmt::Debug,
{
    match term {
        Literal(lit) => eval_literal(lit.clone()),
        Var(name) => ctx.borrow_mut().get_value(Key::Name(name.clone())),
        DBI(i) => ctx.borrow_mut().get_value(Key::DBI(*i)),
        App(fun, arg) => {
            let e1 = eval(fun,ctx.clone())?;
            let e2 = eval(arg,ctx.clone())?;
            do_app(e1, e2)
        },
        Pi(ty, body) => {
            let dom = eval(ty, ctx.clone())?;
            let ran = Closure {
                env: ctx.clone(),
                body
            };
            return Ok(Rc::new(Value::Pi(dom, Rc::new(ran))));
        },
        Lam(body) => {
            let closure = Closure { env: ctx.clone(), body };
            Ok(Rc::new(Value::Lam(Rc::new(closure))))
        },
        AnnLam(..) => todo!("eval AnnLam"),
        Asc(_, _) => unreachable!("should be removed by typechecker, RIGHT?"),
        SC(..) => todo!("eval SC"),
    }
}

fn eval_literal<'a, T>(lit: TermLiteral) -> TResult<RT<'a, T>>
where
    T: PartialEq + Clone + std::fmt::Debug,
{
    match lit {
        Number(Num::Z(p)) => Ok(Rc::new(Value::Z(p))),
        Number(Num::Q(p,q)) => Ok(Rc::new(Value::Q(p, q))),
        Hole => todo!("what to do about holes?, it should be neutral, but we dont know the type yet"),
        _ => todo!("eval_literal"),
    }
}

pub fn eval_closure<'a, T>(closure: Rc<Closure<'a, T>>, arg: RT<'a, T>)
                           -> TResult<RT<'a, T>>
where
    T: PartialEq + Clone + std::fmt::Debug
{
    let env = &closure.env;
    env.borrow_mut().insert(arg);
    eval(&closure.body, env.clone())
}

// // Eliminators

pub fn do_app<'a, T>(func: RT<'a, T>, arg: RT<'a, T>) -> TResult<RT<'a, T>>
where
    T: PartialEq + Clone + std::fmt::Debug,
{
    match func.borrow() {
        Value::Lam(closure) => eval_closure(closure.clone(), arg),
        Value::Neutral(f, neu) => {
            if let Value::Pi(dom, ran) = f.borrow() {
                Ok(Rc::new(Value::Neutral(
                    eval_closure(ran.clone(), arg.clone())?,
                    Rc::new(Neutral::App(neu.clone(), Normal(dom.clone(), arg))))))
            } else {
                todo!("This should be an error")
            }
        }
        _ => todo!("This should be an error"),
    }
}
