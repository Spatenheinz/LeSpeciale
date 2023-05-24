mod parser;
mod typechecking;
use std::{path::PathBuf, fmt::format};

use lfsc_anorm::alpha::*;
use lfsc_syntax::ast::{AProgram, Program, StrCommand};
use parser::parser::*;
use typechecking::context::*;

use std::env;

use crate::typechecking::handle_command;

macro_rules! insert_str {
    ($vec:ident, $($i:expr),*) => {
            $($vec.push(include_str!($i));)*
    };
}

fn read_file<'a>(path: PathBuf) -> Result<String, String> {
    let str = std::fs::read_to_string(path);
    match str {
        Ok(s) => Ok(s),
        Err(e) => Err(format!("Error reading file: {}", e))
    }
}

// fn parse_prog<'a>(path: &'a str) -> Result<Program<'a>, String> {
fn parse_prog(path: &str) -> Result<Vec<StrCommand>, String> {
    match parse_file(path) {
        Ok((_, prog)) => Ok(prog),
        Err(e) => Err(format!("Error parsing file: {}", e))
    }
}

fn normalize_file(prog: Vec<StrCommand>) -> Result<AProgram, String> {
    let mut normalized = vec![];
    for x in prog.into_iter() {
        let a = alpha_convert_command(x);
        // println!("{:?}", a);
        normalized.push(a);
    };
    Ok(normalized)
}

fn check_file<'a, 'b>(prog: &'b AProgram<'a>,
                      gctx: &mut GlobalContext<'b, &'a str>) -> Result<(), String>
where 'a: 'b
{
    for x in prog.iter() {
        handle_command(x, gctx)?
    }
    Ok(())
}

fn main() -> Result<(), String> {
    let mut string_life = Vec::with_capacity(14);
    insert_str!(string_life,
                "../signatures/core_defs.plf",
                "../signatures/util_defs.plf",
                "../signatures/theory_def.plf",
                "../signatures/equality_rules.plf",
                "../signatures/nary_programs.plf",
                "../signatures/quantifiers_rules.plf",
                "../signatures/arith_programs.plf",
                "../signatures/arith_rules.plf",
                "../signatures/boolean_programs.plf",
                "../signatures/boolean_rules.plf",
                "../signatures/cnf_rules.plf",
                "../signatures/strings_programs.plf",
                "../signatures/strings_rules.plf"
    );
    let mut normal = vec![];
    let args: Vec<String> = env::args().collect();
    let file = &args[1];
    let s = std::fs::read_to_string(file).unwrap();
    string_life.push(&s);
    for i in string_life.iter() {
        let a = parse_prog(i)?;
        for x in a.into_iter() {
            let b = alpha_convert_command(x);
            normal.push(b)
        }
    }
    let mut gctx = init_with_str();
    for i in normal.iter() {
        handle_command(i, &mut gctx)?;
        // if let Err(e) = handle_command(i, &mut gctx) {
        //     // println!("{:?}", e);
        //     return Err(format!("Error: {:?}", e))
        // }
    }
    // check_file(&normal, &mut gctx)?;
    Ok(())
}
