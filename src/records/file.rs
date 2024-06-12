use std::fs::{self, OpenOptions};

use anyhow::anyhow;
use im::Vector;

use crate::ast::{Expr, LispResult, SymbolTable};

#[derive(Debug)]
pub(crate) struct FileRecord {
    path: String,
    file: Option<std::fs::File>,
}

// TODO: Remove this
impl Default for FileRecord {
    fn default() -> Self {
        Self {
            path: "<no path>".to_string(),
            file: None,
        }
    }
}

impl PartialEq for FileRecord {
    fn eq(&self, other: &Self) -> bool {
        self.path == other.path
    }
}

impl FileRecord {
    pub(crate) const RECORD_NAME: &'static str = "FileRecord";

    pub(crate) fn from_x7(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
        symbol_table
            .lookup(&Self::RECORD_NAME.into())?
            .call_fn(exprs, symbol_table)
    }

    fn new(f: std::fs::File, path: String) -> FileRecord {
        FileRecord {
            file: Some(f),
            path,
        }
    }

    pub(crate) fn open_file(path: String) -> LispResult<Self> {
        // TODO: Allow access to OpenOptions in x9.
        // Open the file with liberal permissions.
        let f = OpenOptions::new()
            .write(true)
            .create(true)
            .read(true)
            .open(path.clone())
            .map_err(|e| anyhow!("Could not open file \"{}\" because {}", &path, e))?;
        // Make the path pretty.
        let abs_path = fs::canonicalize(path)
            .map_err(|e| anyhow!("Could not canonicalize path! {}", e))?
            .to_str()
            .ok_or_else(|| anyhow!("Could not represent path as UTF-8 string"))?
            .into();
        Ok(FileRecord::new(f, abs_path))
    }
}
