use std::error::Error;

use crate::{ast::SymbolTable, cli::Options};

// TODO: Figure out best way to have the stdlib available
// $X9_PATH?
fn stdlib_dir() -> std::io::Result<&'static str> {
    Ok("./stdlib")
}

pub(crate) fn load_X9_stdlib(
    opts: &Options,
    symbol_table: &SymbolTable,
) -> Result<(), Box<dyn Error>> {
    todo!()
}