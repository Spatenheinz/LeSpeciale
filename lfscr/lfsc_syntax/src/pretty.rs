use crate::ast::{AlphaTerm::*, AlphaTermSC, NumericSC, AlphaCompoundSC, AlphaPattern, SideEffectSC};
use std::fmt::{Display, Formatter, Result, write};
use crate::ast::{AlphaTerm, Num, Ident};


impl<T: Clone + Display> Display for Ident<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Ident::Symbol(x) => write!(f, "{}", x),
            Ident::DBI(i) => write!(f, "#{}", i),
        }
    }
}
impl Display for Num {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Num::Z(x) => write!(f, "{}", x),
            Num::Q(p,q) => write!(f, "{}/{}", p, q)
        }
    }
}

impl<T: Clone + Display> Display for AlphaTerm<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Number(n) => write!(f, "{}", n),
            Hole => write!(f, "_"),
            Ident(id) => write!(f, "{}", id),
            App(t1, t2) => {
              let mut s = String::new();
              s.push_str(&format!("{}", t1));
              for arg in t2 {
                  s.push_str(&format!(" {}", arg));
              }
              write!(f, "({})", s)

            }
            Let(t1, t2) => write!(f, "(@ {} {})", t1, t2),
            Pi(t1, t2) => write!(f, "(! {} {})", t1, t2),
            AnnLam(t1, t2) => write!(f, "(# {} {})", t1, t2),
            Lam(t) => write!(f, "(\\ {})", t),
            Asc(t1, t2) => write!(f, "(: {} {})", t1, t2),
            SC(t1, t2) => write!(f, "(^ {} {})", t1, t2),
        }
    }
}

impl<T: Clone + Display> Display for AlphaTermSC<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            AlphaTermSC::Number(x) => write!(f, "{}", x),
            AlphaTermSC::Ident(x) => write!(f, "{}", x),
            AlphaTermSC::Let(m, n) => write!(f, "(let {} {})", m, n),
            AlphaTermSC::App(f, args) => {
                todo!()
                // let mut s = String::new();
                // s.push_str(&format!("{}", f));
                // for arg in args {
                //     s.push_str(&format!(" {}", arg));
                // }
                // write!(f, "({})", s)
            }
            AlphaTermSC::Numeric(numeric) => write!(f, "{}", numeric),
            AlphaTermSC::Compound(compound) => write!(f, "{}", compound),
            AlphaTermSC::SideEffect(sideeffects) => write!(f, "{}", sideeffects),
        }
    }
}

impl<T: Clone + Display> Display for NumericSC<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            NumericSC::Sum(sc1, sc2) => write!(f, "(+ {} {})", sc1, sc2),
            NumericSC::Prod(sc1, sc2) => write!(f, "(* {} {})", sc1, sc2),
            NumericSC::Div(sc1, sc2) => write!(f, "(/ {} {})", sc1, sc2),
            NumericSC::Neg(sc) => write!(f, "(- {})", sc),
            NumericSC::ZtoQ(sc) => write!(f, "(ZtoQ {})", sc),
            NumericSC::ZBranch { n: sc1, tbranch: sc2, fbranch: sc3 } =>
                write!(f, "(if_zero {} {} {})", sc1, sc2, sc3),
            NumericSC::NegBranch { n: sc1, tbranch: sc2, fbranch: sc3 } =>
                write!(f, "(if_neg {} {} {})", sc1, sc2, sc3),
        }
    }
}

impl<T: Clone + Display> Display for AlphaCompoundSC<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            AlphaCompoundSC::Match(scrut, patterns) => {
                let mut s = String::new();
                s.push_str(&format!("match {}", scrut));
                for (p, c) in patterns {
                    s.push_str(&format!("({} {})", p, c))
                }
                write!(f, "({})", s)
            },
            AlphaCompoundSC::IfEq { a, b, tbranch, fbranch} => {
                todo!()
            }
            AlphaCompoundSC::Fail(a) => write!(f, "(fail {})", a),
        }
    }
}

impl<T: Clone + Display> Display for AlphaPattern<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            AlphaPattern::Default => write!(f, "default"),
            AlphaPattern::Symbol(x) => write!(f, "{}", x),
            AlphaPattern::App(c, args) => {
                let mut s = String::new();
                for a in 0..*args-1 {
                    s.push_str(&format!(" #{}", a))
                }
                write!(f, "({} {})", c, s)
            }
        }
    }
}

impl<T: Clone + Display> Display for SideEffectSC<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            SideEffectSC::IfMarked { n, c, tbranch, fbranch} => {
                write!(f, "(ifmarked{} {} {} {})", n, c, tbranch, fbranch)
            },
            SideEffectSC::MarkVar(n, sc) => {
                write!(f, "(markvar{} {})", n, sc)
            },
            SideEffectSC::Do(a, b) => write!(f, "(do {} {})", a, b),
        }
    }
}
