pub mod check;
pub mod context;
pub mod values;
pub mod nbe;
pub mod readback;
pub mod infer;
mod sc;
// mod occurs;

use lfsc_syntax::ast::{Command, AlphaTerm, BuiltIn};
use infer::infer;
use check::check;
use nbe::eval;

use self::{context::{GlobalContext, RLCTX}, values::TResult};

// pub fn handle_command<'a, T>(com: Command<T, AlphaTerm<T>>,
//                              lctx: RLCTX<'a,T>,
//                              gctx: &'a mut GlobalContext<'a, T>) -> TResult<(), T>
// where T: Clone + BuiltIn + std::fmt::Debug + PartialEq
// {
//     match com {
//       Command::Declare(var, ty) => {
//           todo!()
//             // let ty1 = synth(&ty, lctx, gctx)?;
//             // check(ty1, gctx.kind, lctx, gctx)?;
//             // let val = eval(&ty, lctx, gctx)?;
//             // gctx.insert(var, val);
//       },
//         Command::Define(var, term) => {
//             todo!()
//         },
//         Command::Check(term) => {
//             // synth(term, lctx, gctx)
//             todo!()
//         },
//     _ => todo!(),
//     }
// }
