use crate::{ast::SymbolTable, cli::Options};
use crate::parser::read;
use std::error::Error;
use std::fs::File;
use std::io;
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

pub(crate) fn load_X9_stdlib(
    opts: &Options,
    symbol_table: &SymbolTable,
) -> Result<(), Box<dyn Error>> {
    Ok(())
}
