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
pub enum Ident<Id> {
    Symbol(Id),
    DBI(u32)
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
pub enum Term<Id> {
    Number(Num),
    Hole,
    // Variables can be both explicit and by a De Bruijn Index, Notice this will first occur
    // after alpha normalization. And only bound variables will be DBI.
    Ident(Ident<Id>),
    // a binder may be either a dependent binder (pi), a lambda or a big lambda
    // \ and lam : also called unascription is a lambda binder with None typing
    // # this is the ascription function
    // // % this is the big lambda case
    Binder{ kind: BinderKind, var: Option<Id>, ty: Option<Box<Type<Id>>>, body: Box<Term<Id>> },
    // @ and let (this is syntactical sugar) let x = e in b ~ (lambda x . b) e
    // : ascription
    Ascription { ty: Box<Type<Id>>, val: Box<Term<Id>> },
    // ^ and provided
    SC(TermSC<Id>, TermSC<Id>),
    // => alias for nested !-terms.
    Arrow { decls: Decls, result: Box<Term<Id>>},
    App(Box<Term<Id>>, Box<Term<Id>>)
}


#[derive(Debug, PartialEq)]
#[repr(C)]
pub enum AlphaTerm<Id> {
    Number(Num),
    Hole,
    Ident(Ident<Id>),
    // a Pi binder consists of a type and a body
    Pi(Box<AlphaTerm<Id>>, Box<AlphaTerm<Id>>),
    // a lambda binder consists of a body
    Lam(Box<AlphaTerm<Id>>),
    // an annotated lambda  consists of a type and a body
    AnnLam(Box<AlphaTerm<Id>>, Box<AlphaTerm<Id>>),
    // i dont know about the big lambda
    Asc(Box<AlphaTerm<Id>>, Box<AlphaTerm<Id>>),
    SC(AlphaTermSC<Id>, AlphaTermSC<Id>),
    App(Box<AlphaTerm<Id>>, Box<AlphaTerm<Id>>),
}


type Type<Id> = Term<Id>;

type Decls = Vec<Decl>;
#[derive(Debug, PartialEq, Clone)]
pub enum Decl {

}

#[derive(Debug, PartialEq, Clone)]
pub enum NumericSC<SC> {
    Sum(SC, SC),
    Prod(SC, SC),
    Div(SC, SC),
    Neg(SC),
    ZtoQ(SC),
    ZBranch { n: SC, tbranch: SC, fbranch: SC },
    NegBranch { n: SC, tbranch: SC, fbranch: SC },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern<Id> {
    Default,
    Symbol(Ident<Id>),
    App(Ident<Id>, Vec<Id>)
}


#[derive(Debug, PartialEq, Clone)]
pub enum CompoundSC<SC, Id> {
    Match(SC, Vec<(Pattern<Id>, SC)>),
    // < order
    // Compare { a: SC, b: SC, tbranch: SC, fbranch: SC },
    // =
    IfEq { a: SC, b: SC, tbranch: SC, fbranch: SC },
    Fail(SC),
}

#[derive(Debug, PartialEq, Clone)]
pub enum SideEffectSC<SC> {
    IfMarked { n: u32, c: SC, tbranch: SC, fbranch: SC },
    MarkVar(u32, SC), // can only be a variable however, can be both explicit and DBI
    Do(SC, SC),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TermSC<Id> {
    Number(Num),
    Var(Id),
    Let(Id, Box<TermSC<Id>>, Box<TermSC<Id>>),
    App(Box<TermSC<Id>>, Box<TermSC<Id>>),
    Numeric(Box<NumericSC<TermSC<Id>>>),
    Compound(Box<CompoundSC<TermSC<Id>, Id>>),
    SideEffect(Box<SideEffectSC<TermSC<Id>>>)
}

#[derive(Debug, PartialEq, Clone)]
pub enum AlphaTermSC<Id> {
    Number(Num),
    Ident(Ident<Id>),
    Let(Box<AlphaTermSC<Id>>, Box<AlphaTermSC<Id>>),
    App(Box<AlphaTermSC<Id>>, Box<AlphaTermSC<Id>>),
    Numeric(Box<NumericSC<AlphaTermSC<Id>>>),
    Compound(Box<CompoundSC<AlphaTermSC<Id>, Id>>),
    SideEffect(Box<SideEffectSC<AlphaTermSC<Id>>>)
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
