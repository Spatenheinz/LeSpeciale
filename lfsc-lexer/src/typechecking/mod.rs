pub mod check;
pub mod context;
pub mod values;
pub mod nbe;
pub mod readback;
pub mod infer;
mod sc;
pub mod tester;
// mod occurs;


use std::rc::Rc;

use lfsc_syntax::ast::{Command, AlphaTerm, BuiltIn, AlphaTermSC};
use infer::infer;
use check::check;
use nbe::eval;

use self::{context::{GlobalContext, RLCTX, LocalContext, RGCTX}, values::TResult, infer::infer_sort};

pub fn handle_command<'a, T>(com: &'a Command<T, AlphaTerm<T>, AlphaTermSC<T>>,
                             // lctx: RLCTX<'a,T>,
                             gctx: RGCTX<'a, T>) -> TResult<(), T>
where T: Clone + BuiltIn + std::fmt::Debug + PartialEq,
{
    let lctx = Rc::new(LocalContext::new());
    match com {
      Command::Declare(var, ty) => {
          // actually doing this for pi will check that it is a sort already,
            infer_sort(&ty, lctx.clone(), gctx.clone())?;
            let val = eval(&ty, lctx, gctx.clone())?;
            gctx.insert(var.clone(), val);
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

