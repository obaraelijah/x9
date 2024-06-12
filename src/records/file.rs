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
}
