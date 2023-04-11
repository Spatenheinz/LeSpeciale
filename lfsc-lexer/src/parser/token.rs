use crate::parser::location::Location;

pub type Tokens<'a> = Vec<Token<'a>>;

#[derive(Debug, PartialEq, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub raw: &'a str,
    pub location: Location,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind<'a>, raw: &'a str, location: Location) -> Self {
        Self {
            kind,
            raw,
            location,
        }
    }
    pub const fn new_eof() -> Self {
        Self {
            kind: TokenKind::EOF,
            raw: "",
            location: Location::empty(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind<'a> {
    // Single character symbols
    LParen,
    RParen,
    Percent,
    Bang,
    Pound,
    At,
    Colon,
    BackSlash,
    Caret,
    Hole,
    Tilde,

    // keywords
    Declare,
    Define,
    Check,
    Program,
    Function,
    Opaque,
    Run,

    //Type literals
    Type,
    Mpz,
    Mpq,

    Let,
    Do,
    Match,
    Default,
    MpAdd,
    MpNeg, // should be called sub?
    MpDiv,
    MpMul,
    MpIfNeg,
    MpIfZ,
    MpzToMpq,
    Compare,
    IfEq,
    Fail,

    Provided,
    DeclareRule,
    DeclareType,
    DefineConst,
    Pi,
    Arrow,
    Lam,
    CheckAssuming,
    MarkVar { n: u32 },
    IfMarked { n: u32 },
    // This should be unbounded integers
    Natural(u32),
    Rational { p: u32, q: u32 },

    Ident { name: &'a str },

    EOF,
}

pub fn keywords(s: &str) -> TokenKind {
    match s {
        "declare" => TokenKind::Declare,
        "define" => TokenKind::Define,
        "check" => TokenKind::Check,
        "program" => TokenKind::Program,
        "function" => TokenKind::Function,
        "opaque" => TokenKind::Opaque,
        "run" => TokenKind::Run,

        "type" => TokenKind::Type,
        "let" => TokenKind::Let,
        "do" => TokenKind::Do,
        "match" => TokenKind::Match,
        "default" => TokenKind::Default,
        "mpz" => TokenKind::Mpz,
        "mpq" => TokenKind::Mpq,
        "mp_add" => TokenKind::MpAdd,
        "mp_neg" => TokenKind::MpNeg,
        "mp_div" => TokenKind::MpDiv,
        "mp_mul" => TokenKind::MpMul,
        "mp_if_neg" => TokenKind::MpIfNeg,
        "mp_ifzero" => TokenKind::MpIfZ,
        "mpz_to_mpq" => TokenKind::MpzToMpq,
        "compare" => TokenKind::Compare,
        "ifequal" => TokenKind::IfEq,
        "fail" => TokenKind::Fail,
        "provided" => TokenKind::Provided,
        "declare-rule" => TokenKind::DeclareRule,
        "declare-type" => TokenKind::DeclareType,
        "define-const" => TokenKind::DefineConst,
        "pi" => TokenKind::Pi,
        "->" => TokenKind::Arrow,
        "lam" => TokenKind::Lam,
        "check-assuming" => TokenKind::CheckAssuming,

        "_" => TokenKind::Hole,
        // in case number is missing markVar1 etc.
        _ => {
            if s.starts_with("markvar") {
                match s[7..].parse::<u32>() {
                    Ok(n) => return TokenKind::MarkVar { n },
                    Err(_) => return TokenKind::Ident { name: s },
                }
            }
            if s.starts_with("ifmarked") {
                match s[8..].parse::<u32>() {
                    Ok(n) => return TokenKind::IfMarked { n },
                    Err(_) => return TokenKind::Ident { name: s },
                }
            }
            TokenKind::Ident { name: s }
        }
    }
}

impl<'a> From<&Token<'a>> for TokenKind<'a> {
    fn from(item: &Token<'a>) -> Self {
        item.kind.clone()
    }
}
