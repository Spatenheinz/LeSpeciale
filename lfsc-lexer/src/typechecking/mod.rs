pub mod check;
pub mod context;
mod values;
mod nbe;
mod readback;
mod infer;
mod sc;
mod tester;

use std::rc::Rc;

use lfsc_syntax::ast::{Command, StrAlphaCommand};
// use nbe::eval;

use self::{context::{LocalContext, Rgctx, Rlctx}, values::{TResult, RT}};

pub struct EnvWrapper<'ctx, T: Copy> {
    pub lctx: Rlctx<'ctx, T>,
    pub gctx: Rgctx<'ctx, T>,
    pub allow_dependent: bool,
}

impl<'ctx, T> EnvWrapper<'ctx, T>
where T: PartialEq + std::fmt::Debug + Copy
{
    pub fn new(lctx: Rlctx<'ctx, T>,
           gctx: Rgctx<'ctx, T>,
           allow_dependent: bool) -> Self {
        Self { lctx, gctx, allow_dependent }
    }

    pub fn update_local(&self, val: RT<'ctx, T>) -> Self {
        Self { lctx: LocalContext::insert(val, self.lctx.clone()),
               gctx: self.gctx.clone(),
               allow_dependent: self.allow_dependent }
    }
}

pub fn handle_command<'a, 'b>(com: &'b StrAlphaCommand<'a>,
                             // lctx: RLCTX<'a,T>,
                             gctx: Rgctx<'b, &'a str>) -> TResult<(), &'a str>
{
    // let lctx = Rc::new(LocalContext::new());
    let env = EnvWrapper::new(Rc::new(LocalContext::new()), gctx.clone(), true);
    match com {
      Command::Declare(var, ty) => {
          // actually doing this for pi will check that it is a sort already,
            env.infer_sort(ty)?;
            let val = env.eval(ty)?;
            gctx.insert(var, val);
            Ok(())
      },
        Command::Define(var, term) => {
            todo!()
        },
        Command::Check(term) => {
            // synth(term, lctx, gctx)
            todo!()
        },
    _ => todo!(),
    }
}
