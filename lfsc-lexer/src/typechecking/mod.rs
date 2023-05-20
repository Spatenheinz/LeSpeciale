pub mod context;
mod check;
mod values;
mod nbe;
mod readback;
mod infer;
mod sc;
pub mod errors;

use std::{rc::Rc, borrow::Borrow, cell::Cell};

use lfsc_syntax::ast::{Command, StrAlphaCommand, Ident, BuiltIn};

use self::{context::{LocalContext, Rgctx, Rlctx, GlobalContext},
           values::{TResult, RT, is_type_or_datatype, Value,  Type, ResRT, ref_compare},
           errors::TypecheckingErrors};
use std::hash::Hash;

// #[derive(Clone)]
struct EnvWrapper<'global, 'term, T: Copy + Eq + Ord + Hash + std::fmt::Debug> {
    pub lctx: LocalContext<'term, T>,
    pub gctx: Rgctx<'global, 'term, T>,
    pub allow_dbi: u32,
    pub hole_count: Cell<u32>
}

impl<'global, 'term, T> EnvWrapper<'global, 'term, T>
where T: Eq + Ord + Hash + std::fmt::Debug + Copy + BuiltIn
{
    pub fn new(lctx: LocalContext<'term, T>,
               gctx: Rgctx<'global, 'term, T>,
           allow_dbi: u32,
           hole_count: Cell<u32>) -> Self {
        Self { lctx, gctx, allow_dbi, hole_count }
    }

    pub fn insert_local(&mut self, val: RT<'term, T>) {
        self.lctx.insert(val)

    }
    pub fn define_local(&mut self, ty: RT<'term, T>, val: RT<'term, T>) {
        self.lctx.define(ty, val)
    }
    pub fn update_local(&mut self, val: RT<'term, T>) {
        self.lctx.decl(val)
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
    pub fn same(&mut self,
                t1: RT<'term, T>,
                t2: RT<'term, T>) -> TResult<(), T>
    {
        self.convert(t1, t2, self.gctx.kind.clone())
    }

    pub fn convert(&mut self,
                t1: RT<'term, T>,
                t2: RT<'term, T>,
                tau: RT<'term, T>) -> TResult<(), T>
    {
        if ref_compare(t1.clone(), t2.clone()) { return Ok(()) }
        let e1 = self.readback(tau.clone(), t1)?;
        let e2 = self.readback(tau, t2)?;
        // println!("convert:\n\t{:?}\n\t{:?}", e1, e2);
        if e1 == e2 {
            Ok(())
        } else {
            Err(TypecheckingErrors::Mismatch(e1, e2))
        }
    }
}

pub fn handle_command<'a, 'b>(com: &'b StrAlphaCommand<'a>,
                             gctx: &mut GlobalContext<'b, &'a str>) -> TResult<(), &'a str>
where 'a: 'b
{
    // let lctx = Rc::new(LocalContext::new());
    let mut env = EnvWrapper::new(LocalContext::new(), gctx, 0, Cell::new(1));
    match com {
      Command::Declare(id, ty) => {
          // actually doing this for pi will check that it is a sort already,
          // println!("declare: {}", id);
            if gctx.contains(id) {
                return Err(TypecheckingErrors::SymbolAlreadyDefined(id))
            }
            env.infer_sort(ty)?;
            gctx.insert(id,  env.eval(ty)?);
            Ok(())
      },
        Command::Define(id, term) => {
          // println!("define: {}", id);
            if gctx.contains(id) {
                return Err(TypecheckingErrors::SymbolAlreadyDefined(id))
            }
            let ty = env.infer(term)?;
            if Type::Box == *ty.borrow() {
                return Err(TypecheckingErrors::KindLevelDefinition)
            }
            gctx.define(id, ty, env.eval(term)?);
            Ok(())
        },
        Command::Check(term) => {
            // println!("check");
            env.infer(term)?;
            Ok(())
        },
        Command::Prog { cache: _chache, id, args, ty, body } => {
            // println!("prog: {}", id);
            if gctx.contains(id) {
                return Err(TypecheckingErrors::SymbolAlreadyDefined(id))
            }
            // check that type of the program is type or a datatype
            let res_kind = env.infer(ty)?;
            is_type_or_datatype(res_kind.borrow())?;
            let res_ty = env.eval(ty)?;
            // check all arguments, they must also be either type or datatypes
            let mut args_ty = Vec::new();
            // local context
            let mut tmp_env = LocalContext::new();
            for arg in args.iter() {
                let arg_ty = env.infer(arg)?;
                is_type_or_datatype(arg_ty.borrow())?;
                let ty = env.eval(arg)?;
                tmp_env.decl(ty.clone());
                args_ty.push(ty);
            }
            // order is important since it will allow us to introduce recursion for functions
            let typ = Rc::new(Value::Prog(args_ty.clone(), body));
            gctx.define(id, res_ty.clone(), typ);

            EnvWrapper::new(tmp_env, gctx, 0, Cell::new(0)).check_sc(body, res_ty)?;
            Ok(())
        }
        Command::Run(..) => todo!(),
    }
}
