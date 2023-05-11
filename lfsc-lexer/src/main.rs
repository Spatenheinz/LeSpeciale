mod parser;
mod typechecking;
use lfsc_anorm::alpha::*;
use parser::parser::*;
use typechecking::context::*;


use std::rc::Rc;

use crate::typechecking::handle_command;

fn main() {
    let str = &std::fs::read_to_string("src/smol.plf").unwrap();
    let parsed = parse_file(str);
    println!("{:?}", parsed);
    let mut normalized = vec![];
    for x in parsed.unwrap().1.into_iter() {
        let a = alpha_convert_command(x);
        println!("{:?}", a);
        normalized.push(a);
    };
    println!();
    // let gctx = Rc::new(init_with_str());
    let mut gctx = init_with_str();
    // let gctx = init_with_str();
    for x in normalized.iter() {
      if let Err(x) =  handle_command(x, &mut gctx) {
          println!("Error: {:?}", x);
          return
      }
    }
    dbg!(gctx);
    // let ty = gctx.get_type(&"holds").unwrap();
    // println!("ref of ty: {:?}", Rc::strong_count(&ty));
    // drop(gctx);
    // let path = std::fs::read_dir("./signatures").unwrap();
    // for i in path {
    //     let i = i.unwrap();
    //     println!("{:?}", i.file_name());
    //     let str = &std::fs::read_to_string(i.path()).unwrap();
    //     let (rest, prog) = parse_file(str).unwrap();
    // }
}
