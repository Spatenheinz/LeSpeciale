pub mod context;
mod check;
mod values;
mod nbe;
mod readback;
mod infer;
mod sc;
mod tester;

use std::{rc::Rc, borrow::Borrow};

use lfsc_syntax::ast::{Command, StrAlphaCommand, Ident, BuiltIn};
// use nbe::eval;

use self::{context::{LocalContext, Rgctx, Rlctx, GlobalContext},
           values::{TResult, RT, is_type_or_datatype, Value, TypecheckingErrors, Type, ResRT}};

#[derive(Clone)]
struct EnvWrapper<'global, 'term, T: Copy> {
    pub lctx: Rlctx<'term, T>,
    pub gctx: Rgctx<'global, 'term, T>,
    pub allow_dbi: u32,
}

impl<'global, 'term, T> EnvWrapper<'global, 'term, T>
where T: PartialEq + std::fmt::Debug + Copy + BuiltIn
{
    pub fn new(lctx: Rlctx<'term, T>,
               gctx: Rgctx<'global, 'term, T>,
           allow_dbi: u32) -> Self {
        Self { lctx, gctx, allow_dbi }
    }

    pub fn update_local(&self, val: RT<'term, T>) -> Self {
        Self { lctx: LocalContext::insert(val, self.lctx.clone()),
               gctx: self.gctx,
               allow_dbi: self.allow_dbi }
    }
    pub fn get_value(&self, key: &Ident<T>) -> ResRT<'term, T> {
        match key {
            Ident::DBI(i) if self.allow_dbi >= *i => self.lctx.get_value(*i),
            Ident::DBI(_) => Err(TypecheckingErrors::DependentTypeNotAllowed),
            Ident::Symbol(name) => self.gctx.get_value(name),
        }
    }

    pub fn get_type(&self, key: &Ident<T>) -> ResRT<'term, T> {
        match key {
            Ident::DBI(i) => self.lctx.get_type(*i),
            Ident::Symbol(name) => self.gctx.get_type(name),
        }
    }
    pub fn same(&self,
                t1: RT<'term, T>,
                t2: RT<'term, T>) -> TResult<(), T>
    {
        self.convert(t1, t2, self.gctx.kind.clone())
    }

    pub fn convert(&self,
                t1: RT<'term, T>,
                t2: RT<'term, T>,
                tau: RT<'term, T>) -> TResult<(), T>
    {
        println!("t1: {:?}\nt2: {:?}\ntau: {:?}", t1, t2, tau);
        let e1 = self.readback(tau.clone(), t1)?;
        let e2 = self.readback(tau.clone(), t2)?;
        println!("e1: {:?}\ne2: {:?}", e1, e2);
        if e1 == e2 {
            Ok(())
        } else {
            Err(TypecheckingErrors::Mismatch(e1, e2))
        }
    }
}

pub fn handle_command<'a, 'b>(com: &'b StrAlphaCommand<'a>,
                             // lctx: RLCTX<'a,T>,
                             gctx: &mut GlobalContext<'b, &'a str>) -> TResult<(), &'a str>
{
    // let lctx = Rc::new(LocalContext::new());
    let env = EnvWrapper::new(Rc::new(LocalContext::new()), gctx, 0);
    match com {
      Command::Declare(id, ty) => {
          // actually doing this for pi will check that it is a sort already,
            if gctx.contains(id) {
                return Err(TypecheckingErrors::SymbolAlreadyDefined(id))
            }
            println!("Declare: {:?}\n\n\n", id);
            env.infer_sort(ty)?;
            let val = env.eval(ty)?;
            gctx.insert(id, val);
            Ok(())
      },
        Command::Define(id, term) => {
            if gctx.contains(id) {
                return Err(TypecheckingErrors::SymbolAlreadyDefined(id))
            }
            println!("Define: {:?}\n", id);
            let ty = env.infer(term)?;
            let val = env.eval(term)?;
            if Type::Box == *ty.borrow() {
                return Err(TypecheckingErrors::KindLevelDefinition)
            }
            println!("ty: {:?}\nval: {:?}\n\n\n", ty, val);
            gctx.define(id, ty, val);
            Ok(())
        },
        Command::Check(term) => {
            println!("CHECKING");
            env.infer(term)?;
            Ok(())
        },
        Command::Prog { cache: _chache, id, args, ty, body } => {
            if gctx.contains(id) {
                return Err(TypecheckingErrors::SymbolAlreadyDefined(id))
            }
            println!("Prog: {:?}\n\n\n", id);
            // check that type of the program is type or a datatype
            let res_kind = env.infer(ty)?;
            is_type_or_datatype(res_kind.borrow())?;
            let res_ty = env.eval(ty)?;
            // check all arguments, they must also be either type or datatypes
            let mut args_ty = Vec::new();
            let mut tmp_env = env.clone();
            for arg in args.iter() {
                let arg_ty = env.infer(arg)?;
                is_type_or_datatype(arg_ty.borrow())?;
                let ty = env.eval(arg)?;
                tmp_env = tmp_env.update_local(ty.clone());
                args_ty.push(ty);
            }
            let lctx = tmp_env.lctx.clone();
            drop(tmp_env);
            // order is important since it will allow us to introduce recursion for functions
            println!("args: {:?}\n\n\n", args_ty);
            let typ = Rc::new(Value::Prog(args_ty.clone(), body));
            gctx.define(id, res_ty.clone(), typ);

            let env = EnvWrapper::new(lctx, gctx, 0);
            let body_ty = env.infer_sc(body)?;
            println!("body type {:?}", body_ty);
            println!("result type {:?}", res_ty);
            env.same(body_ty, res_ty.clone())?;
            Ok(())
        }
        Command::Run(..) => todo!(),
    }
}
