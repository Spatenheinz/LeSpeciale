use std::ops::RangeFrom;

use lfsc_syntax::{ast::{Num, Term, TermLiteral}, binder, var};

extern crate nom;

use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{
        alpha1, alphanumeric1, char, digit1, multispace0, multispace1, one_of, satisfy, space0,
    },
    combinator::{cut, map, map_parser, map_res, opt, recognize, value},
    error::{context, Error, ParseError, VerboseError},
    multi::many0,
    sequence::{delimited, pair, preceded, terminated, tuple},
    AsChar, FindToken, IResult, InputIter, InputLength, InputTake, InputTakeAtPosition, Parser,
    Slice, UnspecializedInput,
};

fn parse_term_literal(it: &str) -> IResult<&str, TermLiteral> {
    alt((
        parse_num,
        map(tag("mpq"), |_| TermLiteral::Mpq),
        (map(tag("mpz"), |_| TermLiteral::Mpz)),
        (map(tag("type"), |_| TermLiteral::Type)),
        (map(tag("_"), |_| TermLiteral::Hole)),
    ))(it)
}

pub fn parse_term(it: &str) -> IResult<&str, Term<&str>> {
    alt((
        map(parse_term_literal, |x| Term::Literal(x)),
        map(parse_ident, |x| Term::Var(x)),
        parens(parse_inner_term)
    ))(it)
}

//TODO: Might overflow
fn parse_inner_term(it: &str) -> IResult<&str, Term<&str>> {
    lexeme(alt((
        map(parse_term_literal, |x| Term::Literal(x)),
        map(
            preceded(lexeme(alt((tag("let"),tag("@")))), tuple((parse_ident, parse_term, parse_term))),
            |(var, val, body)| binder!(let var, val, body)
        ),
        map(
            preceded(alt((tag("forall"),tag("!"))), tuple((parse_ident, parse_term, parse_term))),
            |(var, ty, body)| binder!(pi, var : ty,  body),
        ),
        map(
            preceded(tag("!"), tuple((parse_term, parse_term))),
            |(ty, val)| Term::Ascription { ty: Box::new(ty), val: Box::new(val)},
        ),
        map(
            preceded(alt((tag("lam"),tag("!"))), tuple((parse_ident, parse_term))),
            |(var, body)| binder!(lam, var, body),
        ),
        map(
            preceded(alt((tag("#"),tag("!"))), tuple((parse_ident, parse_term, parse_term))),
            |(var, ty, body)| binder!(biglam, var : ty, body),
        ),
        parse_term,
    )))(it)
}

// pub fn parse_type(it: &str) -> IResult<&str, Type, VerboseError<&str>> {
//     Ok((it, Type::Default))
// }

pub fn parse_num(it: &str) -> IResult<&str, TermLiteral> {
    let (rest, p) = lexeme(digit1)(it)?;
    match lexeme(preceded(tag("/"), digit1::<&str, VerboseError<&str>>))(rest) {
        Ok((rest, q)) => Ok((
            rest,
            TermLiteral::Number(Num::Q(p.parse::<u32>().unwrap(),
                                       q.parse::<u32>().unwrap())))),
        Err(_) => {
            Ok((rest,
                TermLiteral::Number(Num::Z(p.parse::<u32>().unwrap()))))
        }
    }
}

pub fn parens<I, P, O, E: ParseError<I>>(p: P) -> impl FnMut(I) -> IResult<I, O, E>
where
    I: Slice<RangeFrom<usize>> + InputIter,
    <I as InputIter>::Item: AsChar,
    P: Parser<I, O, E>,
{
    delimited(char('('), p, char(')'))
}

pub fn comment<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (), E> {
    value((), pair(char(';'), is_not("\n")))(i)
}

pub fn ws<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (), E> {
    value(
        (),
        many0(alt((comment, value((), satisfy(|c| c.is_whitespace()))))),
    )(i)
}

fn lexeme<'a, P, O, E: ParseError<&'a str>>(p: P) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    P: Parser<&'a str, O, E>,
{
    terminated(p, ws)
}

pub fn parse_ident(it: &str) -> IResult<&str, &str> {
    lexeme(recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    )))(it)
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::{Num, TermLiteral, Term};

    use super::{parse_term_literal, parse_term};

    #[test]
    fn parse_mpq() {
        let src = "mpq";
        assert_eq!(parse_term_literal(src), Ok(("", TermLiteral::Mpq)))
    }
    #[test]
    fn parse_mpz() {
        let src = "mpz";
        assert_eq!(parse_term_literal(src), Ok(("", TermLiteral::Mpz)))
    }
    #[test]
    fn parse_natural() {
        let src = "42";
        assert_eq!(
            parse_term_literal(src),
            Ok(("", TermLiteral::Number(Num::Natural("42".to_owned()))))
        )
    }
    #[test]
    fn parse_rational() {
        let src = "42/1337";
        assert_eq!(
            parse_term_literal(src),
            Ok((
                "",
                TermLiteral::Number(Num::Rational {
                    p: "42".to_owned(),
                    q: "1337".to_owned()
                })
            ))
        )
    }
    #[test]
    fn parse_hole() {
        let src = "_";
        assert_eq!(parse_term_literal(src), Ok(("", TermLiteral::Hole)))
    }
    #[test]
    fn parse_type() {
        let src = "type";
        assert_eq!(parse_term_literal(src), Ok(("", TermLiteral::Type)))
    }

    #[test]
    fn parse_term_type() {
        let src = "type    ";
        assert_eq!(parse_term(src), Ok(("", Term::Literal(TermLiteral::Type))))
    }
}
