use lfsc_syntax::{ast::{Num, Term, Command, TermSC, SideEffectSC,
                        NumericSC, CompoundSC, Pattern, Ident, StrCommand}, binder};

extern crate nom;

use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{ char, digit1, satisfy},
    combinator::{map, recognize, value, opt, eof},
    error::{ParseError, VerboseError, Error},
    multi::{many0, many1, fold_many_m_n},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult, Parser,
};

pub fn parse_file(it: &str) -> IResult<&str, Vec<StrCommand>> {
    delimited(ws, many0(parse_command), eof)(it)
}

pub fn parse_command(it: &str) -> IResult<&str, StrCommand> {
    parens(alt((
        map(preceded(reserved("check"), parse_term),
             Command::Check),
        map(preceded(reserved("define"), pair(parse_ident, parse_term)),
             |(x, term)| Command::Define(x, term)),
        map(preceded(reserved("declare"), pair(parse_ident, parse_term)),
             |(x, term)| Command::Declare(x, term)),
        map(preceded(reserved("program"),
                     tuple((parse_ident,
                            parens(many1(parens(pair(parse_ident, parse_term)))),
                            parse_term,
                            parse_sc))),
             |(id, args, ty, body)| Command::Prog{cache: false, id, args, ty, body}),
        map(preceded(reserved("function"),
                     tuple((parse_ident,
                            parens(many1(parens(pair(parse_ident, parse_term)))),
                            parse_term,
                            parse_sc))),
             |(id, args, ty, body)| Command::Prog{cache: true, id, args, ty, body}),
        map(preceded(reserved("run"), parse_sc),
             Command::Run),
    )))(it)
}

pub fn parse_term(it: &str) -> IResult<&str, Term<&str>> {
    let f = alt((
       parse_hole,
       map(parse_ident, |x| Term::Ident(Ident::Symbol(x))),
       map(parse_num, Term::Number),
       open_followed(parse_term_),
    ))(it);
    f
}

fn parse_term_(it: &str) -> IResult<&str, Term<&str>> {
    if let res @ Ok(..) = terminated(parse_binder, closed)(it) {
        return res;
    }
    let (rest, head) = parse_term(it)?;
    let (rest, tail) = many0(parse_term)(rest)?;
    let (rest, _) = closed(rest)?;
    if tail.is_empty() {
        Ok((rest, head))
    } else {
        Ok((rest, Term::App(Box::new(head), tail)))
    }
    // let (rest, tail) =
    //     fold_many0(parse_term, || head.clone(),
    //         |acc, x| Term::App(Box::new(acc), Box::new(x)))(rest)?;
    // let (rest, _) = closed(rest)?;
    // Ok((rest, tail))
}

pub fn parse_hole(it : &str) -> IResult<&str, Term<&str>> {
    map(reserved("_"), |_| Term::Hole)(it)
}

const KEYWORDS: [&str; 26] = ["let", "pi", "lam", "do", "match",
                             "mp_add", "mp_mul", "mp_div", "mp_neg",
                             "mpz_to_mpq", "mp_ifzero", "mp_ifneg",
                             "markvar", "ifmarked", "default", "fail",
                             "run", "define", "declare", "check", "function",
                             "program", "run", "ifequal", "provided", "default"];

pub fn reserved<'a>(expected: &'a str)
                    -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    terminated(tag(expected),ws1)
}

fn parse_binder(it: &str) -> IResult<&str, Term<&str>> {
    alt((
        map(
            preceded(alt((reserved("let"),reserved("@"))),
                          tuple((parse_ident, parse_term, parse_term))),
            |(var, val, body)|  binder!(let var, val, body)
        ),
        map(
            preceded(alt((reserved("pi"),reserved("!"))),
                     tuple((parse_ident, parse_term, parse_term))),
            |(var, ty, body)| binder!(pi, var : ty,  body)
        ),
        map(
            preceded(reserved(":"), tuple((parse_term, parse_term))),
            |(ty, val)| Term::Ascription { ty: Box::new(ty), val: Box::new(val)},
        ),
        map(
            preceded(reserved("#"), tuple((parse_ident, parse_term, parse_term))),
            |(var, typ, body)| binder!(lam, var : typ, body),
        ),
        map(
            preceded(alt((reserved("lam"),reserved("\\"))), tuple((parse_ident, parse_term))),
            |(var, body)| binder!(lam, var, body),
        ),
        map(preceded(alt((reserved("provided"), reserved("^"))),
                     pair(parse_sc, parse_term)),
            |(x,y)| Term::SC(x, Box::new(y))),
    ))(it)
}

fn parse_sc(it: &str) -> IResult<&str, TermSC<&str>> {
    alt((
        // nums, identifier
        map(parse_num, TermSC::Number),
        map(parse_ident, TermSC::Var),
        // rest needs to be in a list
        open_followed(parse_sc_opt)
    ))(it)
}

fn parse_sc_opt(it: &str) -> IResult<&str, TermSC<&str>> {
    if let res @ Ok(..) = terminated(parse_sc_, closed)(it) {
        return res;
    }
    let (rest, head) = parse_ident(it)?;
    let (rest, tail) = many0(parse_sc)(rest)?;
    let (rest, _) = closed(rest)?;
    if tail.is_empty() {
        Ok((rest, TermSC::Var(head)))
    } else {
         Ok((rest, TermSC::App(head, tail)))
    }
}

fn parse_sc_(it: &str) -> IResult<&str, TermSC<&str>> {
    alt((
        map(
            preceded(reserved("let"),
                          tuple((parse_ident, parse_sc, parse_sc))),
            |(var, val, body)|  TermSC::Let(var, Box::new(val), Box::new(body))),
        // side effects
        map(parse_effect, |x| TermSC::SideEffect(Box::new(x))),
        // numerics
        map(parse_numeric, |x| TermSC::Numeric(Box::new(x))),
        // compounds
        map(parse_compound, |x| TermSC::Compound(Box::new(x))),
        ))(it)

}

fn parse_compound(it : &str) -> IResult<&str, CompoundSC<Term<&str>, TermSC<&str>, Pattern<&str>>> {
    alt((
        map(preceded(reserved("fail"),
                     parse_term), CompoundSC::Fail),
        // map(preceded(reserved("compare"),
        //               tuple((parse_sc, parse_sc, parse_sc, parse_sc))),
        //     |(a,b,tbranch,fbranch)|
        //     CompoundSC::Compare{a,b,tbranch, fbranch}),
        map(preceded(reserved("ifequal"),
                     tuple((parse_sc, parse_sc, parse_sc, parse_sc))),
            |(a,b,tbranch,fbranch)|
            CompoundSC::IfEq{a,b,tbranch, fbranch}),
        map(preceded(reserved("match"),
                     pair(parse_sc,
                          many1(parens(pair(parse_pat, parse_sc))))),
            |(x,branches)| CompoundSC::Match(x,branches)),
    ))(it)
}

fn parse_pat(it : &str) -> IResult<&str, Pattern<&str>> {
    let f = alt((
        map(reserved("default"), |_| Pattern::Default),
        map(parse_ident, |x| Pattern::Symbol(Ident::Symbol(x))),
        map(parens(pair(parse_ident, many1(parse_ident))),
            |(x,ys)| Pattern::App(Ident::Symbol(x),ys)),
    ))(it);
    f
}

fn parse_numeric(it : &str) -> IResult<&str, NumericSC<TermSC<&str>>> {
    let f = alt((
        map(preceded(reserved("mp_add"),
                 bin_op), |(x,y)| NumericSC::Sum(x,y)),
        map(preceded(reserved("mp_mul"),
                 bin_op), |(x,y)| NumericSC::Prod(x,y)),
        map(preceded(reserved("mp_div"),
                 bin_op), |(x,y)| NumericSC::Div(x,y)),
        map(preceded(alt((reserved("mp_neg"),reserved("~"))),
                 parse_sc), NumericSC::Neg),
        map(preceded(reserved("mpz_to_mpq"),
                 parse_sc), NumericSC::ZtoQ),
        map(preceded(reserved("mp_ifzero"),
                     tuple((parse_sc, parse_sc, parse_sc))),
            |(n, tbranch, fbranch)| NumericSC::ZBranch { n, tbranch, fbranch }),
        map(preceded(reserved("mp_ifneg"),
                     tuple((parse_sc, parse_sc, parse_sc))),
            |(n, tbranch, fbranch)| NumericSC::NegBranch { n, tbranch, fbranch }),
    ))(it);
    f
}

fn bin_op(it : &str) -> IResult<&str, (TermSC<&str>, TermSC<&str>)> {
    pair(parse_sc, parse_sc)(it)
}

fn parse_effect(it: &str) -> IResult<&str, SideEffectSC<TermSC<&str>>> {
    alt((
        map(preceded(reserved("do"), pair(parse_sc, parse_sc)),
            |(a, b)| SideEffectSC::Do(a, b)),
        map(pair(marks("markvar"), parse_ident),
            |(n, v)| SideEffectSC::MarkVar(n,TermSC::Var(v))),
        map(tuple((marks("ifmarked"), parse_sc, parse_sc, parse_sc)),
            |(n, c, tbranch, fbranch)|
            SideEffectSC::IfMarked{n, c, tbranch, fbranch}),
    ))(it)
}

fn marks<'a>(p: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, u32> {
    move |it : &'a str| {
        let (rest, _) = tag(p)(it)?;
        let (rest, n) = lexeme(
            opt(fold_many_m_n(1, 2, satisfy(|c: char| c.is_ascii_digit()),
                             || 0, |acc, x| acc * 10 + x as u32)))(rest)?;
        match n {
            Some(n) => Ok((rest, n)),
            None => Ok((rest, 1)),
        }
    }
}

pub fn parse_num(it: &str) -> IResult<&str, Num> {
    let (rest, p) = lexeme(digit1)(it)?;
    match lexeme(preceded(tag("/"), digit1::<&str, VerboseError<&str>>))(rest) {
        Ok((rest, q)) => Ok((
            rest,
            Num::Q(p.parse::<i32>().unwrap(),
                   q.parse::<i32>().unwrap()))),
        Err(_) => {
            Ok((rest,
                Num::Z(p.parse::<i32>().unwrap())))
        }
    }
}

// Helpers for composability
fn closed(it: &str) -> IResult<&str, ()> {
    value((), lexeme(char(')')))(it)
}

fn open_followed<'a, P, O, E: ParseError<&'a str>>(p: P)
                            -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    P: Parser<&'a str, O, E>,
{
    preceded(lexeme(char('(')), p)
}

fn parens<'a, P, O, E: ParseError<&'a str>>(p: P)
                            -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    P: Parser<&'a str, O, E>,
{
    delimited(lexeme(char('(')), p, lexeme(char(')')))
}

pub fn comment<'a, E: ParseError<&'a str>>(i: &'a str)
                                           -> IResult<&'a str, (), E> {
    value((), preceded(char(';'), take_until("\n")))(i)
}

pub fn ws1<'a, E: ParseError<&'a str>>(i: &'a str)
                                       -> IResult<&'a str, (), E> {
    value(
        (),
        many1(alt((comment, value((), satisfy(|c| c.is_whitespace()))))),
    )(i)
}
pub fn ws<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (), E> {
    value(
        (),
        many0(alt((comment,
                   value((), satisfy(|c| c.is_whitespace()))))),
    )(i)
}

fn lexeme<'a, P, O, E: ParseError<&'a str>>(p: P)
                            -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    P: Parser<&'a str, O, E>,
{
    terminated(p, ws)
}

fn chars(c: char) -> bool {
    ('\"'..'$').contains(&c) || ('&'..'\'').contains(&c) || ('*'..'/').contains(&c)
    || ('<'..'?').contains(&c) || ('A'..'[').contains(&c) || c >= ']' || ('_'..'z').contains(&c)
}
fn follow(c: char) -> bool {
    ('!'..'\'').contains(&c) || c >= '*'
}

pub fn parse_ident(it: &str) -> IResult<&str, &str> {
    let (rest, x) = lexeme(recognize(pair(
                            satisfy(chars),
                            many0(satisfy(follow)))))(it)?;
    if KEYWORDS.contains(&x) {
        return Err(nom::Err::Error(Error::new(rest, nom::error::ErrorKind::Tag)))
    }
    Ok((rest, x))
}

#[cfg(test)]
mod tests {
}
