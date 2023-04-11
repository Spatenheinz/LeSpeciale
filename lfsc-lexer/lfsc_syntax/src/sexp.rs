#[derive(Debug, PartialEq, Clone)]
pub enum Num {
    Natural(u32),
    Rational{ p: u32, q: u32 },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Number(Num),
    Mpq,
    Mpz,
    Type,
    Hole,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Binder {
    Let,    // let x = M in N
    Pi,        // ! -- PI x : A. B
    Asc,    // : -- x : A
    Lambda, // : -- \x. M
    LambdaAnn, //# -- \x : A. M
    BigLam, // % -- \x : A . B
    SideCondition, // ^
    Arrow, // nested !
}

#[derive(Debug, PartialEq, Clone)]
pub enum Atom<T> {
    Literal(Literal),
    Binder(Binder),
    Sym(T),
}

#[derive(Debug, PartialEq, Clone)]
pub enum SExpr<T> {
    Atom(Atom<T>),
    List(Vec<SExpr<T>>),
}
