use std::{rc::Rc, cell::RefCell, borrow::Borrow};

use lfsc_syntax::{ast::AlphaTerm,  abinder};
use lfsc_syntax::ast::TermLiteral;
use lfsc_syntax::ast::AlphaTerm::*;

use super::{values::{RT, Normal, Value, Type, Neutral, TypecheckingErrors, TResult},
            context::RLCTX,
            nbe::{eval_closure, do_app}
};



pub fn readback_normal<'a, T>(normal: Normal<'a, T>, ctx: RLCTX<'a, T>)
                          -> TResult<AlphaTerm<T>>
where
    T: PartialEq + Clone + std::fmt::Debug
{
    readback(normal.0 , normal.1 , ctx)
}


pub fn readback<'a, T>(ty: RT<'a, T>,
                       val: RT<'a, T>,
                       ctx: RLCTX<'a, T>) -> TResult<AlphaTerm<T>>
where
    T: PartialEq + Clone + std::fmt::Debug,
{
    // neutral values can be readback without a type
    match val.borrow() {
        Value::Neutral(_, a) => return readback_neutral(ty, a.clone(), ctx),
        _ => (),
    }
    match ty.borrow() {
        Type::Pi(dom, ran) => {
            ctx.borrow_mut().insert(dom.clone());
            let tmp = Rc::new(Value::Neutral(dom.clone(), Rc::new(Neutral::DBI(0))));
            let ran_ = eval_closure(ran.clone(), tmp.clone())?;
            let app = do_app(val, tmp)?;
            Ok(abinder!(lam, readback(ran_, app, ctx)?))
        }
        Type::Kind => {
            match val.borrow() {
                Value::Pi(at, bt) => {
                    let dom = readback(ty.clone(), at.clone(), ctx.clone())?;
                    ctx.borrow_mut().insert(at.clone());
                    let cls_res =
                        eval_closure(bt.clone(),
                                    Rc::new(Value::Neutral(at.clone(),
                                                           Rc::new(Neutral::DBI(0)))))?;
                    let ran = readback(ty.clone(), cls_res, ctx.clone())?;
                    Ok(abinder!(pi, dom, ran))
                }
                Value::ZT => Ok(Literal(TermLiteral::Mpz)),
                Value::QT => Ok(Literal(TermLiteral::Mpq)),

                Value::Z(_) => todo!("readback z"),
                Value::Q(..) => todo!("readback q"),
                Value::Kind => todo!("readback kind"),
                Value::Lam(_) => todo!("readback lam"),
                Value::Neutral(_, _) => unreachable!("neutral should have been handled above"),
            }
        }
        Type::ZT => {
            todo!("readback zt")
        },
        Type::QT => {
            todo!("readback qt")
        },
        // what to do about numbers?
        Value::Neutral(..) => Err(TypecheckingErrors::NeutralUsedAsType),
        Value::Z(_) | Value::Q(..) => Err(TypecheckingErrors::NumberUsedAsType),
        Value::Lam(_) => Err(TypecheckingErrors::LamUsedAsType),
    }
}

fn readback_neutral<'a, T>(ty: RT<'a, T>,
                           neu: Rc<Neutral<'a, T>>,
                           ctx: RLCTX<'a,T>) -> TResult<AlphaTerm<T>>
where
    T: PartialEq + Clone + std::fmt::Debug,
{
    match neu.borrow() {
        Neutral::DBI(i) => Ok(DBI(*i)),
        Neutral::Var(name) => Ok(Var(name.clone())),
        Neutral::App(f, a) => {
            let f = readback_neutral(ty, f.clone(), ctx.clone())?;
            let a = readback_normal(a.clone(), ctx.clone())?;
            Ok(App(Box::new(f),Box::new(a)))
        },
        Neutral::Hole(None) => Ok(Literal(TermLiteral::Hole)),
        Neutral::Hole(Some(val)) => readback(ty, val.clone(), ctx)
    }
}
