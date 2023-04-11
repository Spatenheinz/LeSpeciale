// use lfsc_syntax::{ast::*, binder, var};
// use crate::parser::parser::{PResult, ParseError, Parser};
// use crate::parser::token::*;

// #[macro_use]
// mod local_macro {

//     macro_rules! lparen {
//         () => {
//             TokenKind::LParen
//         };
//     }
//     macro_rules! rparen {
//         () => {
//             TokenKind::RParen
//         };
//     }

//     // would be nice with concat_idents but its unstable
//     macro_rules! between {
//         () => {};
//         (single, $self:ident, $f:ident) => {{
//             $self.single_open_paren()?;
//             let x = $self.$f()?;
//             $self.single_close_paren()?;
//             Ok(x)
//         }};
//     }
// }

// #[derive(Debug)]
// pub struct SExpParser<'a> {
//     pub parser: Parser<'a>,
//     pub depth: usize,
// }

// impl<'a> SExpParser<'a> {
//     pub fn new(parser: Parser<'a>) -> Self {
//         Self { parser, depth: 0 }
//     }

//     fn single_open_paren(&mut self) -> PResult<usize> {
//         self.parser.expect(lparen!())?;
//         self.depth += 1;
//         Ok(self.depth - 1)
//     }

//     fn single_close_paren(&mut self) -> PResult<()> {
//         self.parser.expect(rparen!())?;
//         self.depth -= 1;
//         Ok(())
//     }

//     pub fn parse_program(&mut self) -> PResult<Vec<Command<&'a str>>> {
//         let mut commands = Vec::new();
//         while !self.parser.is_eof() {
//             commands.push(between!(single, self, parse_command)?);
//         }
//         Ok(commands)
//     }

//     fn parse_command(&mut self) -> PResult<Command<&'a str>> {
//         match self.parser.next_kind() {
//             TokenKind::Declare => {
//                 let var = self.parse_var()?;
//                 let ty = self.parse_term()?;
//                 Ok(Command::Declare { var, ty })
//             }
//             TokenKind::Define => {
//                 let var = self.parse_var()?;
//                 let term = self.parse_term()?;
//                 Ok(Command::Define { var, term })
//             }
//             TokenKind::Check => Ok(Command::Check(self.parse_term()?)),
//             _ => Err(ParseError::NCommandTop),
//         }
//     }

//     fn maybe_parse_simple(&mut self) -> Option<Term<&'a str>> {
//         let term = match self.parser.peek_kind() {
//             TokenKind::Ident { name } => Some(Term::Var(name)),
//             TokenKind::Mpq => Some(Term::Literal(TermLiteral::Mpq)),
//             TokenKind::Mpz => Some(Term::Literal(TermLiteral::Mpz)),
//             TokenKind::Type => Some(Term::Literal(TermLiteral::Type)),
//             TokenKind::Natural(num) => Some(Term::Literal(TermLiteral::Number(Num::Natural(num)))),
//             TokenKind::Rational { p, q } => {
//                 Some(Term::Literal(TermLiteral::Number(Num::Rational { p, q })))
//             }
//             TokenKind::Hole => Some(Term::Literal(TermLiteral::Hole)),
//             _ => None,
//         }?;
//         self.parser.next();
//         Some(term)
//     }

//     /// A term is either a literal or a list (what we refer to as a compound term)
//     fn parse_term(&mut self) -> PResult<Term<&'a str>> {
//         if let Some(s) = self.maybe_parse_simple() {
//             return Ok(s);
//         };
//         let mut terms: Vec<Term<&'a str>> = Vec::new();
//         let test = self.parse_inner_term(&mut terms);
//         let (head, tail) = test?.split_first().ok_or(ParseError::EmptyTerm)?;
//         Ok(tail.iter().fold(head, |f, x| Term::App {
//             fun: f,
//             arg: x.clone(),
//         }))
//     }
//     fn parse_inner_term<'b>(&'b mut self, terms: &'b mut Vec<Term<&'a str>>) -> PResult<&Vec<Term<&'a str>>> {
//         let depth = self.single_open_paren()?;
//         while depth != self.depth {
//             if self.parser.peek_kind() == rparen!() {
//                 self.single_close_paren()?;
//                 continue;
//             }
//             if self.parser.peek_kind() == lparen!() {
//                 terms.push(self.parse_term()?);
//                 continue;
//             }
//             if let Some(s) = self.maybe_parse_simple() {
//                 terms.push(s);
//                 continue;
//             };
//             let term = match self.parser.next_kind() {
//                 TokenKind::Let | TokenKind::At => {
//                     let var = self.parse_var()?;
//                     let val = self.parse_term()?;
//                     let body = self.parse_term()?;
//                     Ok(Term::App { fun: binder!(lam, var, body).into(), arg: val.into()})
//                 },
//                 TokenKind::Pi | TokenKind::Bang => {
//                     let var = self.parse_var()?;
//                     let ty = self.parse_term()?;
//                     let body = self.parse_term()?;
//                     Ok(binder!(pi, var : ty, body))
//                 },
//                 TokenKind::Colon => {
//                     let ty = self.parse_term()?;
//                     let val = self.parse_term()?;
//                     Ok(Term::Ascription { ty: ty.into(), val: val.into() })
//                 },
//                 TokenKind::Lam | TokenKind::BackSlash => {
//                     let var = self.parse_var()?;
//                     let body = self.parse_term()?;
//                     Ok(binder!(lam, var, body))
//                 },
//                 TokenKind::Pound => {
//                     let var = self.parse_var()?;
//                     let ty = self.parse_term()?;
//                     let body = self.parse_term()?;
//                     Ok(binder!(lam, var : ty, body))
//                 },
//                 TokenKind::Caret => {
//                     todo!()
//                 },
//                 TokenKind::Arrow => {
//                     todo!()
//                 },
//                 TokenKind::Percent => {
//                     let var = self.parse_var()?;
//                     let ty = self.parse_term()?;
//                     let body = self.parse_term()?;
//                     Ok(binder!(biglam, var : ty, body))
//                 }
//                 _ => todo!(),
//             }?;
//             self.single_close_paren()?;
//             terms.push(term);
//         }
//         Ok(terms)
//     }
//     // fn parse_sideeffect_side_condition(&self) -> PResult<TermSC> {
//     //     todo!()
//     // }
//     // fn parse_compound_side_condition(&self) -> PResult<TermSC> {
//     //     todo!()
//     // }
//     // fn parse_term_side_condition(&self) -> PResult<TermSC> {
//     //     todo!()
//     // }
//     // fn parse_inner_term(&mut self) -> PResult<Term<String>> {
//     //     match self.parser.next_kind() {
//     //         TokenKind::Let | TokenKind::At => {
//     //             let var = self.parse_var()?;
//     //             let val = self.parse_term()?;
//     //             let body = self.parse_term()?;
//     //             Ok(Term::Let {
//     //                 var,
//     //                 val: val.into,
//     //                 body: body.into(),
//     //             })
//     //         },
//     //         TokenKind::Pi | TokenKind::Bang => {
//     //             let var = self.parse_var()?;
//     //             let ty = self.parse_term()?;
//     //             let body = self.parse_term()?;
//     //             Ok(Term::Dependent { var, ty: ty.into(), body: body.into() })
//     //         },
//     //         TokenKind::Colon => {
//     //             let ty = self.parse_term()?;
//     //             let val = self.parse_term()?;
//     //             Ok(Term::Ascription { ty: ty.into(), val: val.into })
//     //         },
//     //         TokenKind::Lam | TokenKind::BackSlash => {
//     //             let var = self.parse_var()?;
//     //             let body = self.parse_term()?;
//     //             Ok(Term::UnAscription { var, body: body.into() })
//     //         },
//     //         TokenKind::Pound => {
//     //             let var = self.parse_var()?;
//     //             let ty = self.parse_term()?;
//     //             let body = self.parse_term()?;
//     //             Ok(Term::AscFun { var, ty: ty.into(), body: body.into() })
//     //         },
//     //         TokenKind::Caret => {
//     //             todo!()
//     //         },
//     //         TokenKind::Arrow => {
//     //             todo!()
//     //         }
//     //         _ => {
//     //             // I am not satisfied about having to call this again but whatever.
//     //             if let Some(s) = self.maybe_parse_simple() {
//     //                 Ok(s)
//     //             } else {
//     //                 Err(self.parser.expect(TokenKind::Arrow).unwrap_err())
//     //             }
//     //         }
//     //     }
//     // }
//     fn parse_var(&mut self) -> PResult<String> {
//         // should use expect??
//         if let TokenKind::Ident { name } = self.parser.next_kind() {
//             return Ok(name.to_owned());
//         }
//         println!("{:?}", self.parser.peek_kind());
//         Err(ParseError::NotIdent)
//     }
// }

// #[cfg(test)]
// mod tests {
//     use crate::parser::{
//         lexer::tokenize,
//         parser::{ParseError, Parser},
//         token::TokenKind,
//     };
//     // use lfsc_syntax::ast::{Command, Num, Term, TermLiteral};
//     // use lfsc_syntax::var;

//     use super::SExpParser;

//     #[test]
//     fn parse_empty_program() {
//         let inp = "";
//         let lex = tokenize(inp);
//         let parser = Parser::new(&lex);
//         let mut sexp = SExpParser::new(parser);
//         assert!(sexp.parse_program().is_ok_and(|x| x.is_empty()));
//     }
//     #[test]
//     fn parse_unbalanced_program() {
//         let inp = "(declare d type))";
//         let lex = tokenize(inp);
//         let parser = Parser::new(&lex);
//         let mut sexp = SExpParser::new(parser);
//         assert!(sexp.parse_program().is_err_and(|x| match x {
//             ParseError::Unexpected { tok: y } => y.kind == rparen!(),
//             _ => false,
//         }));
//     }
//     // #[test]
//     // fn parse_let_term() {
//     //     let inp = "(let x 10 x)";
//     //     let lex = tokenize(inp);
//     //     let parser = Parser::new(&lex);
//     //     let mut sexp = SExpParser::new(parser);
//     //     assert_eq!(
//     //         sexp.parse_term(),
//     //         Ok(Term::Let {
//     //             var: "x".to_owned(),
//     //             val: Term::Literal(TermLiteral::Number(Num::Natural(
//     //                 "10".to_owned()
//     //             ))).into(),
//     //             body: Term::Var("x".to_owned()).into()
//     //         })
//     //     );
//     // }
//     // #[test]
//     // fn parse_var_must_not_be_parenthezised() {
//     //     let inp = "(let (x) 10 x)";
//     //     let lex = tokenize(inp);
//     //     let parser = Parser::new(&lex);
//     //     let mut sexp = SExpParser::new(parser);
//     //     assert_eq!(sexp.parse_term(), Err(ParseError::NotIdent));
//     // }
//     // #[test]
//     // fn parse_simple_declaration() {
//     //     let inp = "(define d (@ x 10 x))";
//     //     let lex = tokenize(inp);
//     //     let parser = Parser::new(&lex);
//     //     let mut sexp = SExpParser::new(parser);
//     //     dbg!(&lex);
//     //     assert_eq!(
//     //         sexp.parse_program(),
//     //         Ok(vec![
//     //             (Command::Define {
//     //                 var: "d".to_owned(),
//     //                 term: Term::Let {
//     //                     var: "x".to_owned(),
//     //                     val: Term::Literal(TermLiteral::Number(Num::Natural(
//     //                         "10".to_owned()
//     //                     ))).into(),
//     //                     body: Term::Var("x".to_owned()).into()
//     //                 }
//     //             })
//     //         ])
//     //     );
//     // }
//     // #[test]
//     // fn parse_is_eof() {
//     //     let inp = "(((let x 10 x)))";
//     //     let lex = tokenize(inp);
//     //     let parser = Parser::new(&lex);
//     //     let mut sexp = SExpParser::new(parser);
//     //     let term = sexp.parse_term();
//     //     assert!(sexp.parser.is_eof());
//     //     assert_eq!(term, Ok(Term::Let {
//     //                     var: "x".to_owned(),
//     //                     val: Term::Literal(TermLiteral::Number(Num::Natural(
//     //                         "10".to_owned()
//     //                     ))).into(),
//     //                     body: Term::Var("x".to_owned()).into()
//     //                 }))
//     // }

//     // #[test]
//     // fn parse_app() {
//     //     let inp = "((@ x 10 x) (z))";
//     //     let lex = tokenize(inp);
//     //     let parser = Parser::new(&lex);
//     //     let mut sexp = SExpParser::new(parser);
//     //     assert_eq!(
//     //         sexp.parse_term(),
//     //         Ok(Term::FunApp {
//     //             f: Term::Let {
//     //                 var: var!(x),
//     //                 val: Term::Literal(TermLiteral::Number(Num::Natural(
//     //                     "10".to_owned()
//     //                 ))).into(),
//     //                 body: Term::Var("x".to_owned()).into()
//     //             }.into(),
//     //             x: Term::Var("z".to_owned()).into()
//     //         }
//     //     ))
//     // }
// }
