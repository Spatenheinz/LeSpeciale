#[derive(Debug, PartialEq, Clone)]
pub enum Num {
    Z(i32),
    Q(i32, i32)
}

pub trait BuiltIn {
    fn _mpz() -> Self;
    fn _mpq() -> Self;
    fn _type() -> Self;
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
    Number(Num),
    Hole,
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
    SC(TermSC<T>, TermSC<T>),
    // => alias for nested !-terms.
    Arrow { decls: Decls, result: Box<Term<T>>},
    App(Box<Term<T>>, Box<Term<T>>)
}


#[derive(Debug, PartialEq)]
#[repr(C)]
pub enum AlphaTerm<T> {
    Number(Num),
    Hole,
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
    SC(AlphaTermSC<T>, AlphaTermSC<T>),
    App(Box<AlphaTerm<T>>, Box<AlphaTerm<T>>),
}


type Type<T> = Term<T>;

type Decls = Vec<Decl>;
#[derive(Debug, PartialEq, Clone)]
pub enum Decl {

}

#[derive(Debug, PartialEq, Clone)]
pub enum NumericSC<T> {
    Sum(T, T),
    Prod(T, T),
    Div(T, T),
    Neg(T),
    ZtoQ(T),
    ZBranch { n: T, tbranch: T, fbranch: T },
    NegBranch { n: T, tbranch: T, fbranch: T },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern<T> {
    Default,
    Symbol(T),
    App(T, Vec<T>)
}

#[derive(Debug, PartialEq, Clone)]
pub enum CompoundSC<AT, T> {
    Match(AT, Vec<(Pattern<T>, AT)>),
    // < order
    Compare { a: AT, b: AT, tbranch: AT, fbranch: AT },
    // =
    IfEq { a: AT, b: AT, tbranch: AT, fbranch: AT },
    Fail(TermSC<T>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum SideEffectSC<AT> {
    IfMarked { n: u32, c: AT, tbranch: AT, fbranch: AT },
    MarkVar(u32, AT), // can only be a variable however, can be both explicit and DBI
    Do(AT, AT),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TermSC<T> {
    Number(Num),
    Var(T),
    Let(T, Box<TermSC<T>>, Box<TermSC<T>>),
    App(Box<TermSC<T>>, Box<TermSC<T>>),
    Numeric(Box<NumericSC<TermSC<T>>>),
    Compound(Box<CompoundSC<TermSC<T>, T>>),
    SideEffect(Box<SideEffectSC<TermSC<T>>>)
}

#[derive(Debug, PartialEq, Clone)]
pub enum AlphaTermSC<T> {
    Number(Num),
    Var(T),
    DBI(u32),
    Let(Box<AlphaTermSC<T>>, Box<AlphaTermSC<T>>),
    App(Box<AlphaTermSC<T>>, Box<AlphaTermSC<T>>),
    Numeric(Box<NumericSC<AlphaTermSC<T>>>),
    Compound(Box<CompoundSC<AlphaTermSC<T>, T>>),
    SideEffect(Box<SideEffectSC<AlphaTermSC<T>>>)
}

#[derive(Debug, PartialEq)]
pub enum Command<Id> {
    Declare(Id, Term<Id>),
    Define(Id, Term<Id>),
    // Opaque{var: Id, term: T},
//  Declare Rule: (declare-rule VAR DECLS TYPE): equivalent to (declare VAR (-> DECLS TYPE))
// Declare Type: (declare-type VAR DECLS): equivalent to (declare VAR (-> DECLS type))
    // DefConst{var: Id, decls: Decls, term: T},
    Check(Term<Id>),
    // AssCheck{decls: Decls, ty: T, term: T},
    Prog{cache: bool, id: Id, args: Vec<(Id, Term<Id>)>, ty: Term<Id>, body: TermSC<Id>},
    Run(TermSC<Id>),
}
