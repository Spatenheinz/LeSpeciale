use lfsc_syntax::ast::{Term, AlphaTerm, BinderKind};
use lfsc_syntax::ast::AlphaTerm::*;

use super::shift::shift;
use super::subst::subst;


pub fn alpha_normalize<'a, T>(term: Term<T>) -> AlphaTerm<T>
where T: Clone + PartialEq
{
    match term {
        Term::Number(l) => Number(l),
        Term::Hole => Hole,
        Term::Var(v) => Var(v),
        Term::DBI(d) => DBI(d),
        Term::Binder {kind, var, ty, mut body } => {
            let ty = ty.map(|x| alpha_normalize(*x));
            if !var.is_none() {
                shift(&mut body, 1, &None, 0);
                subst(&mut body, &var, Term::DBI(0));
            }
            let body = alpha_normalize(*body);
            match kind {
                BinderKind::Pi => Pi(Box::new(ty.unwrap()), Box::new(body)),
                BinderKind::Lam => {
                    if ty.is_none() {
                        Lam(Box::new(body))
                    } else {
                        AnnLam(Box::new(ty.unwrap()), Box::new(body))
                    }
                },
                BinderKind::BigLam => todo!()
            }
        },
        Term::Ascription { ty, val } => {
            let ty = alpha_normalize(*ty);
            let val = alpha_normalize(*val);
            Asc(Box::new(ty), Box::new(val))
        },
        Term::App(fun, arg) => {
            let f = alpha_normalize(*fun);
            let a = alpha_normalize(*arg);
            App(Box::new(f), Box::new(a))
        }
        Term::SC(x, y) => todo!(),
        Term::Arrow { decls, result } => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use crate::typechecking::alpha::alpha_normalize;
    use lfsc_syntax::ast::BinderKind::*;
    use lfsc_syntax::ast::Term::*;
    use lfsc_syntax::{term, var, term_, binder, rec, app};

    #[test]
    fn alpha_normal_let() {
        let mut term = term!((let x (let x y x) (let y x x)));
        alpha_normalize(&mut term);
        assert_eq!(term,
            // (\x -> (let y x x)) (let x y x)
            // (\x -> (\y -> x) x) (let x y x)
            // (\x -> (\y -> x) x) ((\x -> x) y)
            // (\x -> (\y -> 1) 0) ((\x -> 0) y)
            // (\ -> (\ -> 1) 0) ((\ -> 0) y)
            App { fun: Binder { kind: Lam,
                                var: None,
                                ty: None,
                                body: App {
                                    fun: Binder { kind: Lam,
                                                  var: None,
                                                  ty: None,
                                                  body: DBI(1).into() }.into(),
                                    arg: DBI(0).into() }.into() }.into(),
                  arg: App { fun: Binder { kind: Lam,
                                           var: None,
                                           ty: None,
                                           body: DBI(0).into() }.into(),
                             arg: Var("y".to_owned()).into() }.into() })
    }
    #[test]
    fn alpha_normal_lambda() {
        let mut term = term!((λ x (λ y (x y z))));
        alpha_normalize(&mut term);
        assert_eq!(term,
            Binder { kind: Lam,
                     var: None,
                     ty: None,
                     body: Binder { kind: Lam,
                                    var: None,
                                    ty: None,
                                    body: App { fun: DBI(1).into(),
                                                arg: App { fun: DBI(0).into(),
                                                           arg: Var("z".to_owned()).into()
                                                }.into()
                                              }.into() }.into() })
    }
    #[test]
    fn alpha_equivalent() {
        let mut term = term!((λ x (λ y (x y z))));
        alpha_normalize(&mut term);
        let mut term2 = term!((λ a (λ b (a b z))));
        alpha_normalize(&mut term2);
        assert_eq!(term, term2);
    }
}
