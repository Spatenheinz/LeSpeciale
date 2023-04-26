// use lfsc_syntax::ast::Term;
// use lfsc_syntax::ast::Term::*;
// use lfsc_syntax::ast::Ident::*;

// use super::shift::shift;

// pub fn subst<'a, T>(term: &'a mut Term<T>, target: &'a Option<T>, mut sub_term: Term<T>)
// where T: PartialEq + Clone {
//     match term {
//         Hole | Number(_) | Ident(DBI(_)) => (),
//         Ident(Symbol(x)) => {
//             if target.is_some() && target.as_ref().unwrap() == x {
//                 *term = sub_term
//             }
//         }
//         // Substitution should only happen in the binding not in the body in case of
//         // substitution is same as binder. We alwaysuse the innermost binder.
//         // Let { var, val, body } => {
//         //     subst(val, target, sub_term.clone());
//         //     shift(&mut sub_term, 1, target, 0);
//         //     if var != target {
//         //         subst(body, target, sub_term.clone());
//         //     }
//         // }
//         Binder { kind: _kind, var, ty, body } => {
//             ty.as_deref_mut()
//                 .map(|x| subst(x, target, sub_term.clone()));
//             shift(&mut sub_term, 1, target, 0);
//             if var != target {
//                 subst(body, target, sub_term.clone());
//             }
//         }
//         // From here it is trivial
//         Ascription { ty, val } => {
//             subst(ty, target, sub_term.clone());
//             subst(val, target, sub_term);
//         }
//         App(fun, arg) => {
//             subst(fun, target, sub_term.clone());
//             subst(arg, target, sub_term);
//         }
//         SC(x, y) => todo!(),
//         Arrow { decls, result } => todo!(),
//     }
// }

// #[cfg(test)]
// mod tests {
//     use lfsc_syntax::{term, term_, var, binder};

//     use crate::typechecking::subst::subst;

//     #[test]
//     fn subst_simple_let() {
//         let mut term = term!((let x x x));
//         subst(&mut term, &var!(x).into(), term!(y));
//         assert_eq!(term!((let x y x)), term);
//     }
// }

//         // Dependent { var, ty, body } => {
//         //     subst(ty, target, sub_term.clone());
//         //     shift(&mut sub_term, 1, target, 0);
//         //     if var != target {
//         //         subst(body, target, sub_term.clone());
//         //     }
//         // }
//         // Lam { var, ty, body } => {
//         //     ty.as_deref_mut()
//         //         .map(|x| subst(x, target, sub_term.clone()));
//         //     shift(&mut sub_term, 1, target, 0);
//         //     if var != target {
//         //         subst(body, target, sub_term.clone());
//         //     }
//         // }
