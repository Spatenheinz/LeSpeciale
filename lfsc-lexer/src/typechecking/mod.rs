pub mod check;
pub mod context;
pub mod values;
pub mod nbe;
pub mod readback;
pub mod infer;
mod sc;
pub mod tester;

use std::rc::Rc;

use lfsc_syntax::ast::{Command, StrAlphaCommand};
use nbe::eval;

use self::{context::{LocalContext, Rgctx}, values::TResult, infer::infer_sort};

pub fn handle_command<'a, 'b>(com: &'b StrAlphaCommand<'a>,
                             // lctx: RLCTX<'a,T>,
                             gctx: Rgctx<'b, &'a str>) -> TResult<(), &'a str>
{
    let lctx = Rc::new(LocalContext::new());
    match com {
      Command::Declare(var, ty) => {
          // actually doing this for pi will check that it is a sort already,
            infer_sort(ty, lctx.clone(), gctx.clone())?;
            let val = eval(ty, lctx, gctx.clone())?;
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

