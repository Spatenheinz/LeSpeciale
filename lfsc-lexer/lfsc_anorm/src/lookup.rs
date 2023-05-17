use lfsc_syntax::ast::AlphaPattern;
use lfsc_syntax::ast::StrAlphaSC as ASC;
use lfsc_syntax::ast::StrAlphaTerm;
use lfsc_syntax::ast::AlphaTerm::*;
use lfsc_syntax::ast::Ident::*;

fn lookup_(vars: &[&str], var: &str) -> Option<u32> {
    vars.iter().rev()
               .position(|&x| x == var)
               .map(|x| (x as u32))
}

pub(crate) trait Lookup<'a> {
    fn lookup(vars: &[&'a str], var: &'a str) -> Self;
}

impl<'a> Lookup<'a> for StrAlphaTerm<'a> {
    fn lookup(vars: &[&'a str], var: &'a str) -> Self {
        lookup_(vars, var).map(|x| Ident(DBI(x)))
                          .unwrap_or(Ident(Symbol(var)))
    }
}

impl<'a> Lookup<'a> for ASC<'a> {
    fn lookup(vars: &[&'a str], var: &'a str) -> Self {
        lookup_(vars, var).map(|x| ASC::Ident(DBI(x)))
                          .unwrap_or(ASC::Ident(Symbol(var)))
    }
}
impl<'a> Lookup<'a> for AlphaPattern<&'a str> {
    fn lookup(vars: &[&'a str], var: &'a str) -> Self {
        lookup_(vars, var).map(|x| AlphaPattern::Symbol(DBI(x)))
                          .unwrap_or(AlphaPattern::Symbol(Symbol(var)))
    }
}
