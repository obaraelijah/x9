use crate::parser::read;
use crate::{ast::SymbolTable, cli::Options};
use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use walkdir::WalkDir;

// TODO: Figure out best way to have the stdlib available
// $X9_PATH?
fn stdlib_dir() -> std::io::Result<&'static str> {
    Ok("./stdlib")
}

pub(crate) fn recursively_load_dir<P: AsRef<std::path::Path>>(
    show_loading: bool,
    root: P,
    symbol_table: &SymbolTable,
) -> Result<(), Box<dyn Error>> {
    let walker = WalkDir::new(root)
        .follow_links(true)
        .contents_first(true)
        .into_iter()
        .filter_entry(|f| {
            f.file_name()
                .to_str()
                .map(|s| !s.starts_with('.') && s.ends_with(".x9"))
                .unwrap_or(false)
        });
    for entry in walker {
        let entry = entry?;
        let mut strbuf = String::new();
        File::open(entry.path())?.read_to_string(&mut strbuf)?;
        for expr in read(strbuf.as_str()) {
            let prog = match expr {
                Ok(prog) => prog,
                Err(e) => {
                    eprintln!("{e:?}");
                    continue;
                }
            };
            match prog.eval(symbol_table) {
                Ok(p) => {
                    if show_loading {
                        println!("{p}");
                    }
                }
                Err(e) => {
                    eprintln!("{e:?}");
                    continue;
                }
            }
        }
    }
    Ok(())
}

// Load standard library for x9 language
pub(crate) fn load_x9_stdlib(
    opts: &Options,
    symbol_table: &SymbolTable,
) -> Result<(), Box<dyn Error>> {
    let root_dir = stdlib_dir()?;
    recursively_load_dir(opts.show_loading_stdlib, root_dir, symbol_table)
}

pub fn run_file(file_name: &str, symbol_table: &SymbolTable) -> Result<i32, anyhow::Error> {
    symbol_table.load_file(file_name).map(|_| 0) // TODO: Figure out appropriate success mapping
}
