use lfsc_syntax::ast::{Term, BinderKind, Command, StrTerm, StrAlphaTerm,
                       StrCommand, StrAlphaCommand,  StrSC, NumericSC,
                       CompoundSC, Pattern, AlphaPattern, SideEffectSC, AlphaTerm};
use lfsc_syntax::ast::AlphaTerm::*;
use lfsc_syntax::ast::Ident::*;

use lfsc_syntax::ast::StrAlphaSC as ASC;

use crate::lookup::Lookup;


pub fn alpha_convert_command(command: StrCommand) -> StrAlphaCommand {
    let vars = &mut Vec::new();
    match command {
        Command::Declare(var, ty) =>
            Command::Declare(var, alpha_normalize(ty, vars)),
        Command::Define(var, val) =>
            Command::Define(var, alpha_normalize(val, vars)),
        Command::Check(ty) =>
            Command::Check(alpha_normalize(ty, vars)),
        Command::Prog{cache, id, args, ty, body} => {
            let mut types = Vec::new();
            let mut args_checked = Vec::new();
            for (i, ty) in args.into_iter() {
                for j in args_checked.iter() {
                    if &i == j {
                        // TODO: This should be an Result
                        panic!("Duplicate variable in pattern")
                    }
                }
                args_checked.push(i);
                types.push(alpha_normalize(ty, vars));
            }
            let ty = alpha_normalize(ty, vars);
            let len = args_checked.len();
            vars.append(&mut args_checked);
            let body = alpha_normalize_sc(body, vars);
            vars.truncate(len);
            Command::Prog{cache, id, args: types, ty, body}
        }
        Command::Run(sc) => Command::Run(alpha_normalize_sc(sc, vars)),
    }
}

fn local<'a, 'b, Input, Output>(fun: impl Fn(Input, &mut Vec<&'a str>)
                     -> Output + 'b,
             vars: &'b mut Vec<&'a str>) -> Box<dyn FnMut(Input) -> Output + 'b> {
    Box::new(move |term| {
    let len = vars.len();
    let aterm = fun(term, vars);
    vars.truncate(len);
    aterm
    })
}

pub fn alpha_normalize<'a>(term: StrTerm<'a>,
                           vars: &mut Vec<&'a str>) -> StrAlphaTerm<'a> {
    match term {
        Term::Number(l) => Number(l),
        Term::Hole => Hole,
        Term::Ident(Symbol(v)) => Lookup::lookup(vars, v),
        Term::Ident(DBI(d)) => Ident(DBI(d)),
        // Hack
        // Term::Binder {kind:BinderKind::Pi, var,
        //               ty:Some(term), body } => {
        //     if let Term::SC(sc, var) = *term {
        //         if let Term::Ident(Symbol(n)) = *var {
        //         let t1 = AlphaTerm::SC(alpha_normalize_sc(sc,vars),
        //                                     Box::new(AlphaTerm::Ident(Symbol(n))));
        //         vars.push(n);
        //         let body = alpha_normalize(*body, vars);
        //         Pi(Box::new(t1), Box::new(body))
        //         } else {
        //             panic!("should not happen");
        //         }

        //     } else {
        //         let t1 = alpha_normalize(*term, vars);
        //         vars.push(var);
        //         let body = alpha_normalize(*body, vars);
        //         Pi(Box::new(t1), Box::new(body))
        //     }
        // },
        Term::Binder {kind, var, ty, body } => {
            // only some binders contains type annotations.
            let ty = ty.map(|x| local(alpha_normalize, vars)(*x));
            vars.push(var);
            let body = alpha_normalize(*body, vars);
            match kind {
                BinderKind::Pi => Pi(Box::new(ty.unwrap()), Box::new(body)),
                BinderKind::Lam => {
                    if let Some(ty) = ty {
                        AnnLam(Box::new(ty), Box::new(body))
                    } else {
                        Lam(Box::new(body))
                    }
                },
                BinderKind::Let => {
                    Let(Box::new(ty.unwrap()), Box::new(body))
                },
                BinderKind::BigLam => todo!("Currently unsupported in the alpha normalized language")
            }
        },
        Term::Ascription { ty, val } => {
            let mut alpha_local = local(alpha_normalize, vars);
            let ty = alpha_local(*ty);
            let val = alpha_local(*val);
            Asc(Box::new(ty), Box::new(val))
        },
        Term::App(fun, arg) => {
            let mut alpha_local = local(alpha_normalize, vars);
            let f = alpha_local(*fun);
            let mut a = Vec::new();
            for e in arg.into_iter() {
                a.push(alpha_local(e));
            };
            App(Box::new(f), a)
        },
        Term::SC(x, y) => {
            let x = local(alpha_normalize_sc, vars)(x);
            let y = local(alpha_normalize, vars)(*y);
            SC(x, Box::new(y))
        },
        Term::Arrow { decls: _decls, result: _result } => todo!("Currently unsupported"),
    }
}


fn alpha_normalize_sc<'a>(term: StrSC<'a>, vars: &mut Vec<&'a str>) -> ASC<'a> {
    match term {
        StrSC::Number(l) =>
            ASC::Number(l),
        StrSC::Var(v) =>
            Lookup::lookup(vars, v),
        StrSC::Let(var, val, body) => {
            let val = alpha_normalize_sc(*val, vars);
            vars.push(var);
            let body = alpha_normalize_sc(*body, vars);
            ASC::Let(Box::new(val), Box::new(body))
        },
        StrSC::App(fun, args) => {
            if let ASC::Ident(x) = Lookup::lookup(vars, fun) {
                let mut alpha_local = local(alpha_normalize_sc, vars);
                let mut args_ = Vec::new();
                for arg in args.into_iter() {
                    args_.push(alpha_local(arg));
                }
                return ASC::App(x, args_)
            };
            unreachable!("Cannot be reached because parser only allows identifiers in function position")
        },
        StrSC::Numeric(nsc) =>
            ASC::Numeric(Box::new(alpha_numeric(*nsc, vars))),
        StrSC::Compound(comp) =>
            ASC::Compound(Box::new(alpha_compound(*comp, vars))),
        StrSC::SideEffect(side) =>
            ASC::SideEffect(Box::new(alpha_sideeffect(*side, vars))),
    }
}

fn alpha_numeric<'a>(term: NumericSC<StrSC<'a>>, vars: &mut Vec<&'a str>)
                     -> NumericSC<ASC<'a>> {
    let mut alpha_local = local(alpha_normalize_sc, vars);
    match term {
        NumericSC::Sum(x, y) => NumericSC::Sum(alpha_local(x), alpha_local(y)),
        NumericSC::Prod(x, y) => NumericSC::Prod(alpha_local(x), alpha_local(y)),
        NumericSC::Div(x, y) => NumericSC::Div(alpha_local(x), alpha_local(y)),
        NumericSC::Neg(x) => NumericSC::Neg(alpha_local(x)),
        NumericSC::ZtoQ(x) => NumericSC::ZtoQ(alpha_local(x)),
        NumericSC::ZBranch { n, tbranch, fbranch } => {
            let n = alpha_local(n);
            let tbranch = alpha_local(tbranch);
            let fbranch = alpha_local(fbranch);
            NumericSC::ZBranch { n, tbranch, fbranch }
        },
        NumericSC::NegBranch { n, tbranch, fbranch } => {
            let n = alpha_local(n);
            let tbranch = alpha_local(tbranch);
            let fbranch = alpha_local(fbranch);
            NumericSC::NegBranch { n, tbranch, fbranch }
        }
    }
}

fn alpha_compound<'a>(term: CompoundSC<StrTerm<'a>, StrSC<'a>, Pattern<&'a str>>,
                      vars: &mut Vec<&'a str>)
                     -> CompoundSC<StrAlphaTerm<'a>, ASC<'a>, AlphaPattern<&'a str>> {
    match term {
        CompoundSC::Fail(x) => CompoundSC::Fail(alpha_normalize(x, vars)),
        CompoundSC::IfEq { a, b, tbranch, fbranch } => {
            let mut alpha_local = local(alpha_normalize_sc, vars);
            let a = alpha_local(a);
            let b = alpha_local(b);
            let tbranch = alpha_local(tbranch);
            let fbranch = alpha_local(fbranch);
            CompoundSC::IfEq { a, b, tbranch, fbranch }
        },
        CompoundSC::Match(scrut, branches) => {
            let scrut = alpha_normalize_sc(scrut, vars);
            let mut branches_ = Vec::new();
            for (pat, body) in branches.into_iter() {
                let len = vars.len();
                let pat = alpha_pattern(pat, vars);
                let body = alpha_normalize_sc(body, vars);
                vars.truncate(len);
                branches_.push((pat, body))
            };
            CompoundSC::Match(scrut, branches_)
        }
    }
}

fn alpha_pattern<'a>(pat: Pattern<&'a str>, vars: &mut Vec<&'a str>)
    -> AlphaPattern<&'a str> {
    match pat {
        Pattern::Default => AlphaPattern::Default,
        Pattern::Symbol(Symbol(x)) => Lookup::lookup(vars, x),
        Pattern::Symbol(DBI(x)) => AlphaPattern::Symbol(DBI(x)),
        Pattern::App(f, args) => {
            let mut args_checked = Vec::new();
            for i in args.iter() {
                for j in args_checked.iter() {
                    if i == j {
                        // TODO: This should be an Result
                        panic!("Duplicate variable in pattern")
                    }
                }
                args_checked.push(*i)
            }
            let len = args_checked.len();
            vars.append(&mut args_checked);
            // not really safe but unlikely the vector is gonna be that big.
            AlphaPattern::App(f, len as u32)
        }
    }
}

fn alpha_sideeffect<'a>(term: SideEffectSC<StrSC<'a>>,
                      vars: &mut Vec<&'a str>)
                     -> SideEffectSC<ASC<'a>> {
    let mut alpha_local = local(alpha_normalize_sc, vars);
    match term {
        SideEffectSC::IfMarked { n, c, tbranch, fbranch } => {
            let c = alpha_local(c);
            let tbranch = alpha_local(tbranch);
            let fbranch = alpha_local(fbranch);
            SideEffectSC::IfMarked { n, c, tbranch, fbranch }
        },
        SideEffectSC::MarkVar(n, term) => {
            let term = alpha_local(term);
            SideEffectSC::MarkVar(n, term)
        },
        SideEffectSC::Do(x, y) => {
            let x = alpha_local(x);
            let y = alpha_local(y);
            SideEffectSC::Do(x, y)
        }
    }
}

#[cfg(test)]
mod tests {
    use lfsc_syntax::ast::Ident::*;
    use lfsc_syntax::ast::AlphaTerm::*;
    use lfsc_syntax::{term, var, term_, binder, rec, app};

    use crate::alpha::alpha_normalize;

    #[test]
    fn alpha_normal_let() {
        let term = term!((let x (let x y x) (let y x x)));
        let term = alpha_normalize(term, &mut Vec::new());
        assert_eq!(term,
            // (\x -> (let y x x)) (let x y x)
            // (\x -> (\y -> x) x) (let x y x)
            // (\x -> (\y -> x) x) ((\x -> x) y)
            // (\x -> (\y -> 1) 0) ((\x -> 0) y)
            // (\ -> (\ -> 1) 0) ((\ -> 0) y)
            App (
                Lam(App (
                    Lam(Ident(DBI(1)).into()).into(),
                    Ident(DBI(0)).into()
                ).into()).into(),
                App (
                    Lam(Ident(DBI(0)).into()).into(),
                    Ident(Symbol("y")).into()
                ).into()
            ))
    }
    // #[test]
    // fn alpha_normal_lambda() {
    //     let mut term = term!((λ x (λ y (x y z))));
    //     alpha_normalize(&mut term);
    //     assert_eq!(term,
    //         Binder { kind: Lam,
    //                  var: None,
    //                  ty: None,
    //                  body: Binder { kind: Lam,
    //                                 var: None,
    //                                 ty: None,
    //                                 body: App { fun: DBI(1).into(),
    //                                             arg: App { fun: DBI(0).into(),
    //                                                        arg: Var("z".to_owned()).into()
    //                                             }.into()
    //                                           }.into() }.into() })
    // }
    #[test]
    fn alpha_equivalent() {
        let term = term!((λ x (λ y (x y z))));
        let term = alpha_normalize(term, &mut Vec::new());
        let term2 = term!((λ a (λ b (a b z))));
        let term2 = alpha_normalize(term2, &mut Vec::new());
        assert_eq!(term, term2);
    }
}
