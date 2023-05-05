use std::{rc::Rc, borrow::Borrow};

use lfsc_syntax::{ast::AlphaTerm,  abinder, ast::BuiltIn};
use lfsc_syntax::ast::{Ident::*, Num};
use lfsc_syntax::ast::AlphaTerm::*;

use super::values::mk_neutral_var_with_type;
use super::{values::{RT, Normal, Value, Type, Neutral, TypecheckingErrors, TResult},
};

use super::nbe::do_app;

use super::EnvWrapper;

impl<'ctx, T> EnvWrapper<'ctx, T>
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
                let ran_ = ran(var.clone(), self)?;
                let app = do_app(val, var, self)?;
                Ok(abinder!(lam, env.readback(ran_, app)?))
            }
            Type::Box => {
                println!("Box:: {:?}", val);
                match val.borrow() {
                    Value::Pi(at, bt) => {
                        let dom = self.readback(ty.clone(), at.clone())?;
                        let env = self.update_local(at.clone());
                        let cls_res = bt(mk_neutral_var_with_type(at.clone()),
                                         self)?;
                        let ran = env.readback(ty.clone(), cls_res)?;
                        Ok(abinder!(pi, dom, ran))
                    },
                    Value::Star => Ok(Ident(Symbol(T::_type()))),
                    Value::ZT => Ok(Ident(Symbol(T::_mpz()))),
                    Value::QT => Ok(Ident(Symbol(T::_mpq()))),
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
            // TODO:: should we have this both here and for box??
            Type::Star => {
                match val.borrow() {
                    Value::ZT => Ok(Ident(Symbol(T::_mpz()))),
                    Value::QT => Ok(Ident(Symbol(T::_mpq()))),
                    _ => Err(TypecheckingErrors::ReadBackMismatch),
                }
            },
            Value::Neutral(..) => unreachable!(),
            _ => Err(TypecheckingErrors::ValueUsedAsType)
        }
    }

    fn readback_neutral(&self,
                        _ty: RT<'ctx, T>,
                        neu: Rc<Neutral<'ctx, T>>,
                       ) -> TResult<AlphaTerm<T>, T>
    {
        match neu.borrow() {
            Neutral::DBI(i) => Ok(Ident(DBI(*i))),
            Neutral::Var(name) => Ok(Ident(Symbol(*name))),
            Neutral::App(f, a) => {
                let f = self.readback_neutral(_ty, f.clone())?;
                let a = self.readback_normal(a.clone())?;
                Ok(App(Box::new(f), Box::new(a)))
            },
            Neutral::Hole(_) => todo!("readback hole"),
        }
    }
}
