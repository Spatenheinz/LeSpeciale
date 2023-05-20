use std::{rc::Rc, borrow::Borrow};

use lfsc_syntax::{ast::AlphaTerm,  abinder, ast::BuiltIn};
use lfsc_syntax::ast::{Ident::*, Num};
use lfsc_syntax::ast::AlphaTerm::*;

use super::errors::TypecheckingErrors;
use super::values::mk_neutral_var_with_type;
use super::values::{RT, Normal, Value, Type, Neutral, TResult};

use super::EnvWrapper;

use std::hash::Hash;

impl<'global, 'term, T> EnvWrapper<'global, 'term, T>
where T: Eq + Ord + Hash + std::fmt::Debug + BuiltIn + Copy
{
    pub fn readback_normal(&mut self, normal: Normal<'term, T>)
                           -> TResult<AlphaTerm<T>, T>
    {
        self.readback(normal.0, normal.1)
    }

    pub fn readback(&mut self,
                    ty: RT<'term, T>,
                    val: RT<'term, T>) -> TResult<AlphaTerm<T>, T>
    {
        // neutral values can be readback without a type
        if let Value::Neutral(_, a) = val.borrow() {
            return self.readback_neutral(ty, a.clone())
        }
        match ty.borrow() {
            Type::Pi(a, b) => {
                let var = mk_neutral_var_with_type(a.clone());
                let ran_ = b(var.clone(), self.gctx, self.allow_dbi, self.hole_count.clone())?;
                let app = self.do_app(val, var)?;
                self.update_local(a.clone());
                Ok(abinder!(lam, self.readback(ran_, app)?))
            }
            Type::Box => {
                match val.borrow() {
                    Value::Pi(at, bt) => {
                        let a = self.readback(ty.clone(), at.clone())?;
                        let cls_res = bt(mk_neutral_var_with_type(at.clone()),
                                         self.gctx, self.allow_dbi, self.hole_count.clone())?;
                        self.update_local(at.clone());
                        let b = self.readback(ty, cls_res)?;
                        Ok(abinder!(pi, a, b))
                    },
                    Value::Star => Ok(Ident(Symbol(T::_type()))),
                    Value::ZT => Ok(Ident(Symbol(T::_mpz()))),
                    Value::QT => Ok(Ident(Symbol(T::_mpq()))),
                    Value::Run(t1,t2,_) => self.readback(ty, t2.clone()),
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

    fn readback_neutral(&mut self,
                        _ty: RT<'term, T>,
                        neu: Rc<Neutral<'term, T>>,
                       ) -> TResult<AlphaTerm<T>, T>
    {
        match neu.borrow() {
            Neutral::DBI(i) => {
                Ok(Ident(DBI(*i)))
            },
            Neutral::Hole(hol, _) => {
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
