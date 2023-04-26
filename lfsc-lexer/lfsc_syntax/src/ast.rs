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

impl BuiltIn for &str {
    fn _mpz() -> Self {
        "mpz"
    }
    fn _mpq() -> Self {
        "mpq"
    }
    fn _type() -> Self {
        "type"
    }
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
    Binder{ kind: BinderKind, var: Id, ty: Option<Box<Type<Id>>>, body: Box<Term<Id>> },
    // @ and let (this is syntactical sugar) let x = e in b ~ (lambda x . b) e
    // : ascription
    Ascription { ty: Box<Type<Id>>, val: Box<Term<Id>> },
    // ^ and provided
    SC(TermSC<Id>, TermSC<Id>),
    // => alias for nested !-terms.
    Arrow { decls: Decls, result: Box<Term<Id>>},
    App(Box<Term<Id>>, Box<Term<Id>>)
}
pub type StrTerm<'a> = Term<&'a str>;


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

pub type StrAlphaTerm<'a> = AlphaTerm<&'a str>;


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
pub enum AlphaPattern<Id> {
    Default,
    Symbol(Ident<Id>),
    App(Ident<Id>, u32)
}


#[derive(Debug, PartialEq, Clone)]
pub enum CompoundSC<SC, Pattern> {
    Match(SC, Vec<(Pattern, SC)>),
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
    Compound(Box<CompoundSC<TermSC<Id>, Pattern<Id>>>),
    SideEffect(Box<SideEffectSC<TermSC<Id>>>)
}
pub type StrSC<'a> = TermSC<&'a str>;

#[derive(Debug, PartialEq, Clone)]
pub enum AlphaTermSC<Id> {
    Number(Num),
    Ident(Ident<Id>),
    Let(Box<AlphaTermSC<Id>>, Box<AlphaTermSC<Id>>),
    App(Box<AlphaTermSC<Id>>, Box<AlphaTermSC<Id>>),
    Numeric(Box<NumericSC<AlphaTermSC<Id>>>),
    Compound(Box<CompoundSC<AlphaTermSC<Id>, AlphaPattern<Id>>>),
    SideEffect(Box<SideEffectSC<AlphaTermSC<Id>>>)
}

pub type StrAlphaSC<'a> = AlphaTermSC<&'a str>;

pub type StrCommand<'a> = Command<&'a str, // Identifier
                                  Term<&'a str>, // Term
                                  TermSC<&'a str>, // SideCondition
                                  Vec<(&'a str, Term<&'a str>)>>;
pub type StrAlphaCommand<'a> = Command<&'a str, // Identifier
                                       StrAlphaTerm<'a>, // Alpha normalized term
                                       AlphaTermSC<&'a str>, // Alpha normalized side condition
                                       Vec<StrAlphaTerm<'a>>>;
pub type Program<'a> = Vec<StrCommand<'a>>;

#[derive(Debug, PartialEq)]
pub enum Command<Id, Term, SC, Args> {
    Declare(Id, Term),
    Define(Id, Term),
    // Opaque{var: Id, term: T},
//  Declare Rule: (declare-rule VAR DECLS TYPE): equivalent to (declare VAR (-> DECLS TYPE))
// Declare Type: (declare-type VAR DECLS): equivalent to (declare VAR (-> DECLS type))
    // DefConst{var: Id, decls: Decls, term: T},
    Check(Term),
    // AssCheck{decls: Decls, ty: T, term: T},
    Prog{cache: bool, id: Id, args: Args, ty: Term, body: SC},
    Run(SC),
}
