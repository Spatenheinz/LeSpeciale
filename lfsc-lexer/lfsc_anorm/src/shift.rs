use lfsc_syntax::ast::Term::*;
use lfsc_syntax::ast::Term;

/// Shift is supposed to shift the debruijn indicies of terms.
/// `term` is the term to modify, depth is to what depth it should be changed.
/// var_to_shift is the variable to shift.
// TODO: make String a T
// TODO: make visitor pattern.
pub fn shift<'a, T>(term: &'a mut Term<T>, d: u32, target: &'a Option<T>, minimum: u32)
where T: PartialEq + Clone {
    match term {
        Number(_) | Hole => (),
        DBI(n) => {
            if target.is_none() && minimum <= *n {
                *term = DBI(*n + d)
            }
        }
        Var(x) => {
            if target.is_some() && target.as_ref().unwrap() == x {
                *term = DBI(0);
            }
        },
        Binder {kind, var, ty, body } => {
            ty.as_deref_mut().map(|x| shift(x, d, target, minimum));
            if var == target {
                shift(body, d, target, minimum + 1);
            } else {
                shift(body, d, target, minimum);
            }
        }
        // Rest of the cases are trivial and simply recurses down the AST
        Ascription { ty, val } => {
            shift(ty, d, target, minimum);
            shift(val, d, target, minimum);
        }
        App(fun, arg) => {
            shift(fun, d, target, minimum);
            shift(arg, d, target, minimum);
        }
        SC(x, y) => todo!(),
        Arrow { decls, result } => todo!(),
    }
}


#[cfg(test)]
mod tests {
    use lfsc_syntax::{term, var};
    use lfsc_syntax::ast::{Term, TermLiteral};
    use super::shift;

    #[test]
    fn shift_literal_does_nothing() {
        let mut lit = Term::Literal(TermLiteral::Hole);
        let res = lit.clone();
        shift(&mut lit, 1, &var!(x).into(), 0);
        assert_eq!(res, lit)
    }

    #[test]
    fn shift_var_match_makes_debruijn() {
        let mut var = term!(x);
        shift(&mut var, 1, &var!(x).into(), 0);
        assert_eq!(Term::DBI(0), var)
    }

    #[test]
    fn shift_var_non_match_does_nothing() {
        let mut var = term!(x);
        let res = var.clone();
        shift(&mut var, 1, &var!(y).into(), 0);
        assert_eq!(res, var)
    }

}

        // // exactly the same as let // names are different -- maybe we need unifying names.
        // Dependent { var, ty, body } => {
        //     shift(ty, d, target, minimum);
        //     if var == target {
        //         shift(body, d, target, minimum + 1);
        //     } else {
        //         shift(body, d, target, minimum);
        //     }
        // }
        // Lam { var, ty, body } => {
        //     ty.as_deref_mut().map(|x| shift(x, d, target, minimum));
        //     if var == target {
        //         shift(body, d, target, minimum + 1);
        //     } else {
        //         shift(body, d, target, minimum);
        //     }
        // }
