use std::{
    fs::{self, File, OpenOptions},
    io::{Read, Seek, Write},
};

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

    fn rewind_file(&mut self) -> LispResult<()> {
        self.get_file()?
            .seek(std::io::SeekFrom::Start(0))
            .map(|_| ())
            .map_err(|e| anyhow!("{:?}", e))
    }

    fn read_all(&mut self) -> LispResult<String> {
        let mut buf = String::new();
        self.get_file()?
            .read_to_string(&mut buf)
            .map_err(|e| anyhow!("Failed to read to string {}", e))?;
        self.rewind_file()?;
        Ok(buf)
    }

    fn read_to_string(&mut self) -> LispResult<Expr> {
        self.read_all().map(Expr::string)
    }

    fn len(&mut self) -> LispResult<usize> {
        self.get_file()?
            .metadata()
            .map(|m| m.len() as usize)
            .map_err(|e| anyhow!("Failed to get metadata for file, {}", e))
    }

    fn get_file(&mut self) -> LispResult<&mut File> {
        self.file
            .as_mut()
            .ok_or_else(|| anyhow!("Somehow called methods on uninit file"))
    }

    fn write_to_file(&mut self, content: String) -> LispResult<()> {
        self.get_file()?
            .write_all(content.as_bytes())
            .map_err(|e| anyhow!("{:?}", e))
            .map(|_| ())
    }
}
