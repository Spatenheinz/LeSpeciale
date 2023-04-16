use std::ops::RangeFrom;

use lfsc_syntax::{ast::{Num, Term, Command, TermSC, SideEffectSC}, binder, var};

extern crate nom;

use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::{complete::{
        alpha1, alphanumeric1, char, digit1, satisfy,
    }, is_digit},
    combinator::{map, recognize, value, opt},
    error::{ParseError, VerboseError, Error},
    multi::{many0, many1, fold_many_m_n, many_m_n},
    sequence::{delimited, pair, preceded, terminated, tuple},
    AsChar,IResult, InputIter, Slice, Parser,
};

pub fn parse_command(it: &str) -> IResult<&str, Command<&str>> {
    parens(alt((
        map(preceded(reserved("check"), parse_term),
             |x| Command::Check(x)),
        map(preceded(reserved("define"), pair(parse_ident, parse_term)),
             |(x, term)| Command::Define(x, term)),
        map(preceded(reserved("declare"), pair(parse_ident, parse_term)),
             |(x, term)| Command::Declare(x, term))
    )))(it)
}

pub fn parse_term(it: &str) -> IResult<&str, Term<&str>> {
    alt((
       parse_hole,
       map(parse_ident, |x| Term::Var(x)),
       map(parse_num, |x| Term::Number(x)),
       open_followed(parse_term_),
    ))(it)
}

fn parse_term_(it: &str) -> IResult<&str, Term<&str>> {
    if let res @ Ok(..) = terminated(parse_binder, closed)(it) {
        return res;
    }
    let (rest, opt) = parse_term(it)?;
    if let Ok((rest, x)) = terminated(parse_term, closed)(rest) {
        return Ok((rest,Term::App { fun: Box::new(opt), arg: Box::new(x) }));
    }
    let (rest, _) = closed(rest)?;
    Ok((rest, opt))
}

pub fn parse_hole(it : &str) -> IResult<&str, Term<&str>> {
    map(reserved("_"), |_| Term::Hole)(it)
}

const KEYWORDS: [&str; 4] = ["let", "pi", "lam", "forall"];

pub fn reserved<'a>(expected: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    terminated(tag(expected),ws1)
}

pub fn parse_var(it : &str) -> IResult<&str, Term<&str>> {
    map(parse_ident, |x| Term::Var(x))(it)
}

//TODO: Might overflow
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
            |(var, ty, body)| binder!(pi, var : ty,  body),
        ),
        map(
            preceded(reserved(":"), tuple((parse_term, parse_term))),
            |(ty, val)| Term::Ascription { ty: Box::new(ty), val: Box::new(val)},
        ),
        map(
            preceded(alt((reserved("lam"),tag("!"))), tuple((parse_ident, parse_term))),
            |(var, body)| binder!(lam, var, body),
        ),
        // map(
        //     preceded((tag("#"), tuple((parse_ident, parse_term, parse_term))),
        //     |(var, ty, body)| binder!(biglam, var : ty, body),
        // ),
        // preceded(alt((reserved("provided"), reserved("^"))), parse_sc)
    ))(it)
}

fn parse_sc(it: &str) -> IResult<&str, TermSC<&str>> {
    alt((
        map(parse_num, |x| TermSC::Number(x)),
        map(parse_ident, |x| TermSC::Var(x)),
        open_followed(parse_sc_)
    ))(it)
}
fn parse_sc_(it: &str) -> IResult<&str, TermSC<&str>> {
    alt((
        map(
            preceded(alt((reserved("let"),reserved("@"))),
                          tuple((parse_ident, parse_term, parse_term))),
            |(var, val, body)|  TermSC::Let(var, val, body)),
        // side effects
        // map(preceded(reserved("do")),
        //              tuple((parse_ident, parse_term, parse_term))),
        //     |(var, ty, body)| TermSC::Pi(var, ty, body)),
        ))(it)

}

fn parse_effect(it: &str) -> IResult<&str, SideEffectSC<&str>> {
    alt((
        map(preceded(reserved("do"), pair(parse_sc, parse_sc)),
            |(a, b)| SideEffectSC::Do(a, b)),
        map(pair(marks("markvar"), parse_ident),
            |(n, v)| SideEffectSC::MarkVar(n,v)),
        map(tuple((marks("ifmarked"), parse_sc, parse_sc, parse_sc)),
            |(n, c, tbranch, fbranch)|
            SideEffectSC::IfMarked{n, c, tbranch, fbranch}),
    ))(it)
}

fn marks<'a>(p: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, u32> {
    move |it : &'a str| {
        let (rest, _) = tag(p)(it)?;
        let (rest, n) = lexeme(
            opt(fold_many_m_n(1, 2, satisfy(|c: char| c.is_digit(10)),
                             || 0, |acc, x| acc * 10 + x as u32)))(rest)?;
        match n {
            Some(n) => Ok((rest, n)),
            None => Ok((rest, 1)),
        }
    }
}


    // nums, identifier
    // Following needs be parenthesized
    //
    // SIDEEFFECTS-SC
    // do A B... looks like it is nested
    // mark var
    // if marked

    // Compound
    // match
    // if equal
    // compare
    // fail

    // NUMERICS
    // mpadd (SUM)
    // mpmul (PROD)
    // mpdiv (DIV)
    // mpneg (NEG)
    // mpztompq
    // mpifneg
    // mpifzero

    // ~

    // let binding
    // application

pub fn parse_num(it: &str) -> IResult<&str, Num> {
    let (rest, p) = lexeme(digit1)(it)?;
    match lexeme(preceded(tag("/"), digit1::<&str, VerboseError<&str>>))(rest) {
        Ok((rest, q)) => Ok((
            rest,
            Num::Q(u32::from_str_radix(p, 10).unwrap(),
                                       u32::from_str_radix(q, 10).unwrap()))),
        Err(_) => {
            Ok((rest,
                Num::Z(p.parse::<u32>().unwrap())))
        }
    }
}


fn closed(it: &str) -> IResult<&str, ()> {
    let (rest,_) = lexeme(char(')'))(it)?;
    Ok((rest,()))
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
    value((), pair(char(';'), is_not("\n")))(i)
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
        many0(alt((comment, value((), satisfy(|c| c.is_whitespace()))))),
    )(i)
}

fn lexeme<'a, P, O, E: ParseError<&'a str>>(p: P)
                            -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    P: Parser<&'a str, O, E>,
{
    terminated(p, ws)
}

pub fn parse_ident(it: &str) -> IResult<&str, &str> {
    let (rest, x) = lexeme(recognize(pair(
                            alt((alpha1, tag("_"))),
                            many0(alt((alphanumeric1, tag("_")))),
                          )))(it)?;
    if KEYWORDS.contains(&x) {
        return Err(nom::Err::Error(Error::new(rest, nom::error::ErrorKind::Tag)))
    }
    Ok((rest, x))
}

#[cfg(test)]
mod tests {
}
