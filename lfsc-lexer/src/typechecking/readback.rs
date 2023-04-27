use std::{rc::Rc, borrow::Borrow};

use lfsc_syntax::{ast::AlphaTerm,  abinder, ast::BuiltIn};
use lfsc_syntax::ast::Ident::*;
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
        if let Value::Neutral(_, a) =  val.borrow() {
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
                match val.borrow() {
                    Value::Pi(at, bt) => {
                        let dom = self.readback(ty.clone(), at.clone())?;
                        let env = self.update_local(at.clone());
                        let cls_res = bt(mk_neutral_var_with_type(at.clone()),
                                         self)?;
                        let ran = env.readback(ty.clone(), cls_res)?;
                        Ok(abinder!(pi, dom, ran))
                    }
                    Value::ZT => Ok(Ident(Symbol(T::_mpz()))),
                    Value::QT => Ok(Ident(Symbol(T::_mpq()))),

                    Value::Star => todo!(),
                    Value::Z(_) => todo!("readback z"),

                    Value::Q(..) => todo!("readback q"),
                    Value::Box => {dbg!(&self.gctx); todo!()},
                    Value::Lam(_) => todo!("readback lam"),
                    Value::Neutral(_, _) => unreachable!("neutral should have been handled above"),
                    Value::Run(..) => todo!()
                }
            }
            Type::Star => todo!(),
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
            Value::Run(..) => todo!()
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
