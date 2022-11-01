use std::{fs, path};

use anyhow::{Context, Result};
use clap::Parser;

mod ast;

#[macro_use]
extern crate lazy_static;
mod lexer;
mod parser;

#[derive(Parser)]
struct GravlaxCliArgs {
    /// A path to the `lox` source code
    source_code: path::PathBuf,
}
fn main() -> Result<()> {
    let args = GravlaxCliArgs::parse();
    let source_code = fs::read_to_string(&args.source_code)
        .with_context(|| format!("could not read source file `{}`", &args.source_code.display()))?;
    println!("file content: {}", source_code);
    let mut lexer = lexer::Lexer::new(source_code);
    let scanner_output = lexer.lex();
    match scanner_output {
        Ok(tokens) => {
            println!("{:?}", tokens);
        }
        Err(lexing_errors) => {
            for error in lexing_errors {
                println!("{}", error)
            }
        }
    }
    Ok(())
}
