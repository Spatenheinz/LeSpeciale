#[derive(Debug, PartialEq, Clone)]
pub enum Num {
    Z(u32),
    Q(u32, u32)
}

#[derive(Debug, PartialEq, Clone)]
pub enum TermLiteral {
    Number(Num),
    Mpq,
    Mpz,
    Type,
    Hole,
}

pub trait FromLit {
    fn from_lit(lit: TermLiteral) -> Self;
}

impl FromLit for &str {
    fn from_lit(lit: TermLiteral) -> Self {
        match lit {
            TermLiteral::Number(Num::Z(_)) => "mpz",
            TermLiteral::Number(Num::Q(_, _)) => "mpq",
            TermLiteral::Mpq => "mpq",
            TermLiteral::Mpz => "mpz",
            TermLiteral::Type => "type",
            _ => panic!("HOLE SHOULD NOT BE A LITERAL")
        }
    }
}


#[derive(Debug, PartialEq, Clone)]
pub enum BinderKind {
    // ! and forall (pi)
    Pi,
    // \ and lam : also called unascription is a lambda binder with None typing
    Lam,
    // % this is the big lambda case
    BigLam,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Term<T> {
    // A literal
    Literal(TermLiteral),
    // Variables can be both explicit and by a De Bruijn Index, Notice this will first occur
    // after alpha normalization. And only bound variables will be DBI.
    Var(T),
    DBI(u32),
    // a binder may be either a dependent binder (pi), a lambda or a big lambda
    // \ and lam : also called unascription is a lambda binder with None typing
    // # this is the ascription function
    // // % this is the big lambda case
    Binder{ kind: BinderKind, var: Option<T>, ty: Option<Box<Type<T>>>, body: Box<Term<T>> },
    // @ and let (this is syntactical sugar) let x = e in b ~ (lambda x . b) e
    // : ascription
    Ascription { ty: Box<Type<T>>, val: Box<Term<T>> },
    // ^ and provided
    SideCondition { x: Box<TermSC<T>>, y: Box<TermSC<T>>, var: T },
    // => alias for nested !-terms.
    Arrow { decls: Decls, result: Box<Term<T>>},
    App {fun: Box<Term<T>>, arg: Box<Term<T>>}
}


#[derive(Debug, PartialEq)]
pub enum AlphaTerm<T> {
    Literal(TermLiteral),
    Var(T),
    DBI(u32),
    // a Pi binder consists of a type and a body
    Pi(Box<AlphaTerm<T>>, Box<AlphaTerm<T>>),
    // a lambda binder consists of a body
    Lam(Box<AlphaTerm<T>>),
    // an annotated lambda  consists of a type and a body
    AnnLam(Box<AlphaTerm<T>>, Box<AlphaTerm<T>>),
    // i dont know about the big lambda
    Asc(Box<AlphaTerm<T>>, Box<AlphaTerm<T>>),
    SC(Box<AlphaTerm<T>>, Box<AlphaTerm<T>>),
    App(Box<AlphaTerm<T>>, Box<AlphaTerm<T>>),
}


type Type<T> = Term<T>;

type Decls = Vec<Decl>;
#[derive(Debug, PartialEq, Clone)]
pub enum Decl {

}

#[derive(Debug, PartialEq, Clone)]
pub enum NumericSC<T> {
    Sum{ lhs: Num, rhs: Num },
    Prod{ lhs: Num, rhs: Num },
    Div{ lhs: Num, rhs: Num },
    Negation{ num: Num },
    ItoR{ num: Num },
    ZBranch { num: Num, tbranch: Term<T>, fbranch: Term<T> },
    NegBranch { num: Num, tbranch: Term<T>, fbranch: Term<T> },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Default,
    Symbol(String),
    App { sym: String, args: Vec<String> }
}

#[derive(Debug, PartialEq, Clone)]
pub enum CompoundSC<T> {
    Match { disc: Term<T>, cases: Vec<(Pattern, Term<T>)> },
    // < order
    Compare { a: Term<T>, b: Term<T>, tbranch: Term<T>, fbranch: Term<T> },
    // =
    IfEq { a: Term<T>, b: Term<T>, tbranch: Term<T>, fbranch: Term<T> },
    Fail,
}

#[derive(Debug, PartialEq, Clone)]
pub enum SideEffectSC<T> {
    IfMarked { n: u32, c: Term<T>, tbranch: Term<T>, fbranch: Term<T> },
    MarkVar { n: u32, c: Term<T> },
    Do { a: Term<T>, b: Term<T> },
}

#[derive(Debug, PartialEq, Clone)]
pub enum TermSC<T> {
    Numeric(NumericSC<T>),
    Compound(CompoundSC<T>),
    SideEffect(SideEffectSC<T>)
}


#[derive(Debug, PartialEq)]
pub enum Command<T> {
    Declare{var: String, ty: Type<T>},
    Define{var: String, term: Term<T>},
    Opaque{var: String, term: Term<T>},
//  Declare Rule: (declare-rule VAR DECLS TYPE): equivalent to (declare VAR (-> DECLS TYPE))
// Declare Type: (declare-type VAR DECLS): equivalent to (declare VAR (-> DECLS type))
    DefConst{var: String, decls: Decls, term: Term<T>},
    Check(Term<T>),
    AssCheck{decls: Decls, ty: Term<T>, term: Term<T>},
    ProgDef{args: Vec<(String, Term<T>)>, ty: Term<T>, body: Term<T>},
    Run(TermSC<T>),
}
