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
use lfsc_syntax::ast::Command;
use lfsc_syntax::ast::Program;
use parser::parser::*;
use typechecking::nbe::*;
use typechecking::context::*;

// use lfsc_syntax::{term, var, term_, binder, rec, app};
use lfsc_syntax::ast::BinderKind::*;
use lfsc_syntax::ast::Term::*;

use std::rc::Rc;
use std::cell::RefCell;

use crate::typechecking::handle_command;
use crate::typechecking::values::TResult;

fn main() {
    let str = &std::fs::read_to_string("src/smol.plf").unwrap();
    let parsed = parse_file(str);
    println!("{:?}", parsed);
    let mut normalized = vec![];
    for x in parsed.unwrap().1.into_iter() {
        normalized.push(alpha_convert_command(x));

    };
    println!("");
    let gctx = Rc::new(init_with_str());
    // let gctx = init_with_str();
    for x in normalized.iter() {
      let _ = handle_command(x, gctx.clone());
    }
    // let ty = gctx.get_type(&"holds").unwrap();
    // println!("ref of ty: {:?}", Rc::strong_count(&ty));
    // drop(gctx);
    dbg!(gctx);
    // let path = std::fs::read_dir("./signatures").unwrap();
    // for i in path {
    //     let i = i.unwrap();
    //     println!("{:?}", i.file_name());
    //     let str = &std::fs::read_to_string(i.path()).unwrap();
    //     let (rest, prog) = parse_file(str).unwrap();
    // }
}
