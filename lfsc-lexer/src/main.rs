// #![no_std]
// extern crate alloc;
// use alloc::string::{String, ToString};
//
mod parser;
mod typechecking;

// use parser::lexer::*;
// use parser::parser::*;
// use parser::sexp_parser::*;
use lfsc_anorm::alpha::*;
use parser::parser2::*;
use typechecking::nbe::*;
use typechecking::context::*;

// use lfsc_syntax::{term, var, term_, binder, rec, app};
use lfsc_syntax::ast::BinderKind::*;
use lfsc_syntax::ast::Term::*;

use std::rc::Rc;
use std::cell::RefCell;

fn main() {
    // let src = include_str!("overflow.plf");
    let src = include_str!("smol.plf");
    println!("{:?}", parse_term(src));
    // let term =
    //         App { fun: Binder { kind: Lam,
    //                             var: None,
    //                             ty: None,
    //                             body: App {
    //                                 fun: Binder { kind: Lam,
    //                                               var: None,
    //                                               ty: None,
    //                                               body: DBI(1).into() }.into(),
    //                                 arg: DBI(0).into() }.into() }.into(),
    //               arg: App { fun: Binder { kind: Lam,
    //                                        var: None,
    //                                        ty: None,
    //                                        body: DBI(0).into() }.into(),
    //                          arg: Var("type").into() }.into() };
    // let term = alpha_normalize(term);
    // println!("{:?}", &term);
    // let global_ctx = init_with_str();
    // let ctx = LocalContext::new();
    // dbg!(global_ctx.eval(&term, &ctx));


    // let lex = tokenize(src);
    // let parser = Parser::new(&lex);
    // let mut sexp = SExpParser::new(parser);
    // let parsed = sexp.parse_program();
    // println!("{:?}", parsed);
    // for i in parsed.iter() {
    //     println!("{:?}", i);
    // }
}
