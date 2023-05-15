use std::{rc::Rc, borrow::Borrow};

use lfsc_syntax::{ast::AlphaTerm,  abinder, ast::BuiltIn};
use lfsc_syntax::ast::{Ident::*, Num};
use lfsc_syntax::ast::AlphaTerm::*;

use super::values::mk_neutral_var_with_type;
use super::values::{RT, Normal, Value, Type, Neutral, TypecheckingErrors, TResult};

use super::EnvWrapper;

impl<'global, 'ctx, T> EnvWrapper<'global, 'ctx, T>
where T: PartialEq + std::fmt::Debug + BuiltIn + Copy
{
    pub fn readback_normal(&self, normal: Normal<'ctx, T>)
                           -> TResult<AlphaTerm<T>, T>
    {
        self.readback(normal.0 , normal.1)
    }

    pub fn readback(&self,
                    ty: RT<'ctx, T>,
                    val: RT<'ctx, T>) -> TResult<AlphaTerm<T>, T>
    {
        // neutral values can be readback without a type
        if let Value::Neutral(_, a) = val.borrow() {
            return self.readback_neutral(ty, a.clone())
        }
        match ty.borrow() {
            Type::Pi(dom, ran) => {
                let env = self.update_local(dom.clone());
                let var = mk_neutral_var_with_type(dom.clone());
                let ran_ = ran(var.clone(), self.gctx, self.allow_dbi)?;
                let app = self.do_app(val, var)?;
                Ok(abinder!(lam, env.readback(ran_, app)?))
            }
            Type::Box => {
                match val.borrow() {
                    Value::Pi(at, bt) => {
                        let dom = self.readback(ty.clone(), at.clone())?;
                        let env = self.update_local(at.clone());
                        let cls_res = bt(mk_neutral_var_with_type(at.clone()),
                                         self.gctx, self.allow_dbi)?;
                        let ran = env.readback(ty, cls_res)?;
                        Ok(abinder!(pi, dom, ran))
                    },
                    Value::Star => Ok(Ident(Symbol(T::_type()))),
                    Value::ZT => Ok(Ident(Symbol(T::_mpz()))),
                    Value::QT => Ok(Ident(Symbol(T::_mpq()))),
                    Value::Run(t1,t2,_) => self.readback(ty, t2.clone()),
                    // Value::Run(t1,t2) => Ok(SC((*t1).clone(), Box::new(self.readback(ty, t2.clone())?))),
                    _ => Err(TypecheckingErrors::ReadBackMismatch),
                }
            }
            Type::ZT => {
                match val.borrow() {
                    Value::Z(p) => Ok(Number(Num::Z(*p))),
                    _ => Err(TypecheckingErrors::ReadBackMismatch),
                }
            },
            Type::QT => {
                match val.borrow() {
                    Value::Q(p, q) => Ok(Number(Num::Q(*p, *q))),
                    _ => Err(TypecheckingErrors::ReadBackMismatch),
                }
            },
            _ => Err(TypecheckingErrors::ValueUsedAsType)
        }
    }

    fn readback_neutral(&self,
                        _ty: RT<'ctx, T>,
                        neu: Rc<Neutral<'ctx, T>>,
                       ) -> TResult<AlphaTerm<T>, T>
    {
        match neu.borrow() {
            Neutral::DBI(i) => {
                Ok(Ident(DBI(*i)))
            },
            Neutral::Hole(hol) => {
                if let Some(ty) = &*hol.borrow() {
                    // TODO: check the cost of this
                    self.readback_neutral(_ty, ty.clone())
                } else {
                    Ok(Hole)
                }
            },
            Neutral::Var(name) => Ok(Ident(Symbol(*name))),
            Neutral::App(f, a) => {
                let f = self.readback_neutral(_ty, f.clone())?;
                let a = self.readback_normal(a.clone())?;
                Ok(App(Box::new(f), vec![a]))
                //TODO fix this to make sense
                // Ok(App(Box::new(f), Box::new(a)))
            },
        }
    }
}
