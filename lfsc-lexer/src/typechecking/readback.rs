use std::{rc::Rc, borrow::Borrow};

use lfsc_syntax::{ast::AlphaTerm,  abinder, ast::BuiltIn};
use lfsc_syntax::ast::AlphaTerm::*;

use super::context::{LocalContext, RGCTX};
use super::{values::{RT, Normal, Value, Type, Neutral, TypecheckingErrors, TResult},
            context::RLCTX,
            nbe::{eval_closure, do_app}
};



pub fn readback_normal<'a, T>(normal: Normal<'a, T>,
                              lctx: RLCTX<'a, T>,
                              gctx: RGCTX<'a, T>) -> TResult<AlphaTerm<T>, T>
where
    T: PartialEq + Clone + BuiltIn + std::fmt::Debug
{
    readback(normal.0 , normal.1 , lctx, gctx)
}


pub fn readback<'a, T>(ty: RT<'a, T>,
                       val: RT<'a, T>,
                       lctx: RLCTX<'a, T>,
                       gctx: RGCTX<'a, T>) -> TResult<AlphaTerm<T>, T>
where
    T: PartialEq + Clone + BuiltIn + std::fmt::Debug,
{
    // neutral values can be readback without a type
    match val.borrow() {
        Value::Neutral(_, a) => return readback_neutral(ty, a.clone(), lctx, gctx),
        _ => (),
    }
    match ty.borrow() {
        Type::Pi(dom, ran) => {
            let ctx1 = LocalContext::insert(dom.clone(), lctx.clone());
            let tmp = Rc::new(Value::Neutral(dom.clone(), Rc::new(Neutral::DBI(0))));
            let ran_ = eval_closure(ran.clone(), tmp.clone(), gctx)?;
            let app = do_app(val, tmp, gctx)?;
            Ok(abinder!(lam, readback(ran_, app, lctx, gctx)?))
        }
        Type::Kind => {
            match val.borrow() {
                Value::Pi(at, bt) => {
                    let dom = readback(ty.clone(), at.clone(), lctx.clone(), gctx)?;
                    let ctx1 = LocalContext::insert(at.clone(), lctx.clone());
                    let cls_res =
                        eval_closure(bt.clone(),
                                    Rc::new(Value::Neutral(at.clone(),
                                                           Rc::new(Neutral::DBI(0)))),
                                    gctx)?;
                    let ran = readback(ty.clone(), cls_res, lctx.clone(), gctx)?;
                    Ok(abinder!(pi, dom, ran))
                }
                Value::ZT => Ok(Var(T::_mpz())),
                Value::QT => Ok(Var(T::_mpq())),

                Value::Type => todo!(),
                Value::Z(_) => todo!("readback z"),

                Value::Q(..) => todo!("readback q"),
                Value::Kind => todo!("readback kind"),
                Value::Lam(_) => todo!("readback lam"),
                Value::Neutral(_, _) => unreachable!("neutral should have been handled above"),
            }
        }
        Type::Type => todo!(),
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
                           lctx: RLCTX<'a,T>,
                           gctx: RGCTX<'a,T>) -> TResult<AlphaTerm<T>, T>
where
    T: PartialEq + Clone + BuiltIn + std::fmt::Debug,
{
    match neu.borrow() {
        Neutral::DBI(i) => Ok(DBI(*i)),
        Neutral::Var(name) => Ok(Var(name.clone())),
        Neutral::App(f, a) => {
            let f = readback_neutral(ty, f.clone(), lctx.clone(), gctx)?;
            let a = readback_normal(a.clone(), lctx.clone(), gctx)?;
            Ok(App(Box::new(f),Box::new(a)))
        },
        Neutral::Hole(_) => todo!("readback hole"),
    }
}
