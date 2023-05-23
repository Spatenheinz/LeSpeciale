use crate::ast::{Ident, AlphaTerm, AlphaTermSC, AlphaNumericSC, NumericSC, AlphaCompoundSC, CompoundSC, AlphaPattern, AlphaSideEffectSC, SideEffectSC};

pub trait FreeVar {
 fn free_in(&self, dbi: u32) -> bool;
}

impl<Id> FreeVar for Ident<Id> {
 fn free_in(&self, dbi: u32) -> bool {
  match self {
   Ident::Symbol(_) => false,
   Ident::DBI(x) => dbi == *x,
  }
 }
}

impl<Id> FreeVar for AlphaTerm<Id> {
 fn free_in(&self, dbi: u32) -> bool {
  match self {
    AlphaTerm::Ident(x) => x.free_in(dbi),
    AlphaTerm::Hole | AlphaTerm::Number(_) => false,
    AlphaTerm::App(x, y) => x.free_in(dbi) || y.iter().any(|x| x.free_in(dbi)),
    AlphaTerm::Lam(x) => x.free_in(dbi + 1),
    AlphaTerm::Let(x, y) | AlphaTerm::Pi(x, y) | AlphaTerm::AnnLam(x,y)
          => x.free_in(dbi) || y.free_in(dbi + 1),
    AlphaTerm::SC(x, y) => x.free_in(dbi) || y.free_in(dbi),
    AlphaTerm::Asc(x,y) => x.free_in(dbi) || y.free_in(dbi),
  }
 }
}

impl<Id> FreeVar for AlphaTermSC<Id> {
 fn free_in(&self, dbi: u32) -> bool {
  match self {
    AlphaTermSC::Number(_) => false,
    AlphaTermSC::Ident(x) => x.free_in(dbi),
    AlphaTermSC::Let(x, y) => x.free_in(dbi) || y.free_in(dbi + 1),
    AlphaTermSC::App(f, args) => f.free_in(dbi) || args.iter().any(|x| x.free_in(dbi)),
    AlphaTermSC::Numeric(x) => x.free_in(dbi),
    AlphaTermSC::Compound(x) => x.free_in(dbi),
    AlphaTermSC::SideEffect(x) => x.free_in(dbi),
  }
 }
}

impl<Id> FreeVar for AlphaNumericSC<Id> {
 fn free_in(&self, dbi: u32) -> bool {
  use NumericSC::*;
  match self {
    Sum(x, y) | Prod(x, y) | Div(x, y) => x.free_in(dbi) || y.free_in(dbi),
    Neg(x) | ZtoQ(x) => x.free_in(dbi),
    ZBranch { n, tbranch, fbranch }
      | NegBranch { n, tbranch, fbranch } => n.free_in(dbi) || tbranch.free_in(dbi) || fbranch.free_in(dbi),
  }
 }
}

impl<Id> FreeVar for AlphaCompoundSC<Id> {
 fn free_in(&self, dbi: u32) -> bool {
  use CompoundSC::*;
  match self {
    Match(scrut, branches) => {
        scrut.free_in(dbi) ||
            branches.iter()
                    .any(|(p,c)| match p {
                        AlphaPattern::Default => c.free_in(dbi),
                        AlphaPattern::Symbol(x) => x.free_in(dbi) || c.free_in(dbi),
                        AlphaPattern::App(x, args) => {
                            x.free_in(dbi) || c.free_in(dbi + *args)},
                        })
    },
    IfEq { a, b, tbranch, fbranch} => a.free_in(dbi) || b.free_in(dbi) ||
        tbranch.free_in(dbi) || fbranch.free_in(dbi),
    Fail(x) => x.free_in(dbi),
  }
 }
}

impl<Id> FreeVar for AlphaSideEffectSC<Id> {
 fn free_in(&self, dbi: u32) -> bool {
  use SideEffectSC::*;
  match self {
    IfMarked { n, c, tbranch, fbranch } =>
          c.free_in(dbi) ||
        tbranch.free_in(dbi) || fbranch.free_in(dbi),
    MarkVar(_, x) => x.free_in(dbi),
    Do(x, y) => x.free_in(dbi) || y.free_in(dbi),
  }
 }
}
