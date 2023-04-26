// // Numbers: should give value back.
// // (LFSC) sym is error
// //
// // (LFSC) syms is error if it is missing type??
// //
// // Application:
// //   check each type of arguments, (maybe we should just go with a vector)
// //   if fun is a prog, then set tp to its 0th kid, (what exactly is this?)
// //   else if fun is a syms_expr then get the type of the symbol.
// //   after this the head of an application does not have a functional type.
// //
// //   functions must be fully applied. (argues for we could use vector)
// //   check each argument against the type of the function.
// //
// //
// //  Do A B, check code of A then B. return result of checking B.
// //  Let x T M, inserts a T to x in environment, then check M.
// //
// //  BinOP: check left, right - type must be Number type (ZT, or QT)
// //  Must have same type.
// //
// //  Negation: check argument, must be Number type.
// //
// //  Z2Q: check argument, must be ZT. then return QT.
// //
// //  Ifneg and IfZero: first argument must have num type.
// //  then check the types of branches, must be same.
// //  return the type.
// //
// //  Fail should have child which is a star.
// //
// // markvar n C
// //  Markvar: check child, converts it to a symS_expr.
// //  set tptp to the type of the s if class is SymS and there is no value.
// //  report error if type is not set or the type is star.
// //  This happens if the marked variable is not a var or is a lambda bound variable.
// //  return the type
// //
// // ifmarked n C T F
// //  IfMarked: same start as markvar.
// //  then check the two branches
// //  they must have same type.
// //  return the type.
// //
// //  compare a b T F,
// //  check a, which must not be a lambda bound variable.
// //  check b, which must not be a lambda bound variable.
// //  check T and F, which must have same type.
// //  return the type of T.
// //
// //  IfEq
// //  same as compare, however lambdas must be used here.

// //
// // We actually run code, in the application case.
// // So when typechecking an application, we need to check if the parameter is Pi,
// // and if it contains a run. We want a seperate function,
// // because we want to make it only allowed there.
// // We run the code as follows:
// //
// // Numbers, are returned as is.
// // Hole must be filled in. ??? I am unsure how we can even encounter this.
// // SymS -- TODO
// //
// // Fail gives Null, We will return a Fail error
// // Do will run A, then B, and return the result of B.
// //
// // Let x M in N -- run code for M, then save previous value of x if already bound.
// // then set x to the new of M', then run N, then restore x to previous value.
// // return result of N.
// // If we use debruijn indices there is no problem with shadowing.
// //
// // BinoOPs:
// // check left and right, must be numbers, then call, mpz and mpq functions respectively.
// // division must not be 0, WHERE ARE THE FUNCTIONS???
// //
// // Negation:
// // check argument which must be Z or Q, call function respectively.
// //
// // Z2Q: TODO
// //
// // IFNEG & IFZERO: PRETTY SIMPLE.
// //
// // IFMARKED: must not be used in method (useCache mode)
// // run code for child, must be Sym or SymS,
// // we then get the mark of (n) (n must be a number),
// // then chose T branch if mark is 1, F branch if mark is 0.
// //
// // Compare:
// // run code for a, must be Sym or SymS,
// // run code for b, must be Sym or SymS,
// // then branch based on < order. TODO what is the order defined like?
// //
// // IfEq:
// // same as compare but we compare on alpha equivalence. subterms must be anything
// //
// // MarkVar:
// // run code for child, must be a Sym or SymS,
// // then swap the mark.
// //
// // Match:
// // run the scutinee
// // We must then apply weak head reduction? to know the head.
// // That is if scrutinee is a CEXPR, we must do whr.
// //
// // we then collect all args of the scrutinee, we then iterate over all the cases.
// // if the current case is not a case, then run the code (i dont really think this makes much sense, maybe for default case)
// // otherwise we look at the case:
// // if the head of scrutinee is equivalent to the head of the pattern,
// // then we collect the vars of the pattern,
// // we then save the old values temporarily,
// // set the vars to the arguments,
// // run code of the case, then restore variables.

// // APP:

// use std::borrow::Borrow;
// use std::rc::Rc;

// use lfsc_syntax::ast::{AlphaTermSC, Num, NumericSC, CompoundSC, SideEffectSC};
// use lfsc_syntax::ast::Ident::*;
// use lfsc_syntax::ast::AlphaTermSC::*;
// use lfsc_syntax::ast::NumericSC::*;
// use lfsc_syntax::ast::CompoundSC::*;
// use lfsc_syntax::ast::SideEffectSC::*;
// use super::values::TypecheckingErrors::{NaN, DivByZero, ReachedFail};
// use super::context::{RLCTX, RGCTX, set_mark, get_mark, LocalContext};
// use super::values::{ResRT, Value, as_symbolic};


// pub fn run_sc<'a, T>(sc: &'a AlphaTermSC<T>,
//                  lctx: RLCTX<'a, T>,
//                  gctx: RGCTX<'a, T>) -> ResRT<'a, T>
//     where T: Clone + PartialEq + std::fmt::Debug
// {
//     match sc {
//         Number(Num::Z(p)) => Ok(Rc::new(Value::Z(*p))),
//         Number(Num::Q(p,q)) => Ok(Rc::new(Value::Q(*p, *q))),
//         // Little unsure about value vs type.
//         Ident(DBI(i)) => lctx.get_value(*i),
//         Ident(Symbol(x)) => gctx.get_value(x),
//         Let(m, n) => {
//             let m = run_sc(m, lctx.clone(), gctx.clone())?;
//             let lctx = LocalContext::insert(m, lctx);
//             run_sc(n, lctx, gctx)
//         },
//         App(m, n) => todo!(),
//         Numeric(num) => run_num(num, lctx, gctx),
//         Compound(compound) => run_compound(compound, lctx, gctx),
//         SideEffect(se) => run_sideeffect(se, lctx, gctx)
//     }
// }



// fn run_sideeffect<'a, T>(sc: &'a SideEffectSC<AlphaTermSC<T>>,
//                  lctx: RLCTX<'a, T>,
//                  gctx: RGCTX<'a, T>) -> ResRT<'a, T>
//     where T: Clone + PartialEq + std::fmt::Debug
// {
//     match sc {
//     Do(a, b) => {
//         let _ = run_sc(a, lctx.clone(), gctx)?;
//         run_sc(b, lctx, gctx)
//     },
//     MarkVar(n, var) => {
//         // by construction Markvar will be a variable.
//         let var = run_sc(var, lctx.clone(), gctx)?;
//         let v = as_symbolic(&var)?;
//         set_mark(v, *n, lctx, gctx);
//         Ok(var)
//         //var must be symbolic (meaning it is neutral?)
//     },
//     IfMarked{ n, c, tbranch, fbranch} => {
//         let c = run_sc(c, lctx.clone(), gctx)?;
//         let v = as_symbolic(&c)?;
//         let mark = get_mark(v, *n, lctx.clone(), gctx)?;
//         if mark == 1 {
//             run_sc(tbranch, lctx, gctx)
//         } else {
//             run_sc(fbranch, lctx, gctx)
//         }
//     }
//     }
// }

// fn run_compound<'a, T>(sc: &'a CompoundSC<AlphaTermSC<T>, T>,
//                  lctx: RLCTX<'a, T>,
//                  gctx: RGCTX<'a, T>) -> ResRT<'a, T>
//     where T: Clone + PartialEq + std::fmt::Debug
// {
//     match sc {
//     Fail(_) => Err(ReachedFail),
//     IfEq { a, b, tbranch, fbranch } => {
//         todo!("figure out how to compare")
//         // let a = run_sc(a, lctx, gctx)?;
//         // let b = run_sc(b, lctx, gctx)?;
//         // // TODO check if alpha equivalent is enough or should we check specifically with holes?
//         // match (a.borrow(), b.borrow()) {
//         //     (a,b) if *a == *b => run_sc(tbranch, lctx, gctx),
//         //     (a,b) if *a != *b => run_sc(fbranch, lctx, gctx),
//         //     (Value::ZT,_) => unreachable!() // only to dominate the typechecker
//         // }
//     }
//     // Compare { a, b, tbranch, fbranch } => {
//     //     todo!() // somehow a little tricky
//     // }
//     Match(scrut, cases) => {
//         todo!("maybe easier after apply..")
//     }
//     }
// }

// fn run_num<'a, T>(sc: &'a NumericSC<AlphaTermSC<T>>,
//                  lctx: RLCTX<'a, T>,
//                  gctx: RGCTX<'a, T>) -> ResRT<'a, T>
//     where T: Clone + PartialEq + std::fmt::Debug
// {
//     match sc {
//         Sum(x, y) | Prod(x, y) | Div(x, y) => binop(sc, lctx, gctx),
//         Neg(x) => {
//             match run_sc(x, lctx, gctx)?.borrow() {
//                 Value::Z(x) => Ok(Rc::new(Value::Z(-x))),
//                 Value::Q(x, y) => Ok(Rc::new(Value::Q(-x, *y))),
//                 _ => Err(NaN)
//             }
//         },
//         ZtoQ(z) => todo!(),
//         ZBranch { n, tbranch, fbranch } | NegBranch { n, tbranch, fbranch } => {
//             let n = run_sc(n, lctx.clone(), gctx)?;
//             let val = match n.borrow() {
//                     Value::Z(x) => x,
//                     Value::Q(q,_) => q,
//                     _ => return Err(NaN)
//                 };
//             let cond = match sc {
//                     ZBranch { .. } => *val == 0,
//                     NegBranch { .. } => *val < 0,
//                     _ => unreachable!(),
//                     };
//             let branch = if cond { tbranch  } else { fbranch  };
//             run_sc(branch, lctx, gctx)
//         }
//     }
// }

// fn binop<'a, T>(sc: &'a NumericSC<AlphaTermSC<T>>,
//                  lctx: RLCTX<'a, T>,
//                  gctx: RGCTX<'a, T>) -> ResRT<'a, T>
//     where T: Clone + PartialEq + std::fmt::Debug
// {
//     match sc {
//       Sum(x, y) | Prod(x, y) | Div(x, y) => {
//          let x = run_sc(x, lctx.clone(), gctx)?;
//          let y = run_sc(y, lctx, gctx)?;
//          match (x.borrow(), y.borrow()) {
//            (_, Value::Z(0)) => Err(DivByZero),
//            (_, Value::Q(0, _)) => Err(DivByZero),
//            (Value::Z(x), Value::Z(y)) => {
//               match sc {
//                 Sum(_, _) => Ok(Rc::new(Value::Z(x + y))),
//                 Prod(_, _) => Ok(Rc::new(Value::Z(x * y))),
//                 Div(_, _) =>Ok(Rc::new(Value::Z(x / y))),
//                 _ => unreachable!()
//                 }
//             },
//            (Value::Q(x, y), Value::Q(a, b)) => {
//                match sc {
//                  Sum(_, _) => Ok(Rc::new(Value::Q(x * b + a * y, y * b))),
//                  Prod(_, _) => Ok(Rc::new(Value::Q(x * a, y * b))),
//                  Div(_, _) => Ok(Rc::new(Value::Q(x * b, y * a))),
//                 _ => unreachable!()
//                 }
//             },
//             _ => Err(NaN)
//         }
//       }
//     _ => unreachable!()
//     }
// }
// // NOTE: SymS is bound in PI, ANNLAM, BigLam, Lam, Let and in toplevel.
// // Seams like it is simply a variable.
