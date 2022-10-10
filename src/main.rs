use std::{fs, path};

use anyhow::{Context, Result};
use clap::Parser;

mod ast;
mod errors;
mod lexer;
mod parser;

#[derive(Parser)]
struct GravlaxCliArgs {
    /// A path to the `lox` source code
    source_code: path::PathBuf,
}
fn main() -> Result<()> {
    let args = GravlaxCliArgs::parse();
    let source_code = fs::read_to_string(&args.source_code).with_context(|| {
        format!(
            "could not read source file `{}`",
            &args.source_code.display()
        )
    })?;
    println!("file content: {}", source_code);
    Ok(())
}
