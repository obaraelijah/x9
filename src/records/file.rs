use crate::ast::{Expr, LispResult, SymbolTable};
use crate::records::RecordDoc;
use std::{
    fs::{self, File, OpenOptions},
    io::{Read, Seek, Write},
};

use anyhow::anyhow;
use im::Vector;

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

    fn try_shrink(&mut self) -> LispResult<()> {
        if self.len()? == 0 {
            Ok(())
        } else {
            self.get_file()?
                .set_len(0)
                .map_err(|e| anyhow!("Failed to shrink file to 0, {}", e))
        }
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

    fn write(&mut self, content: String) -> LispResult<Expr> {
        let content_len = Expr::num(content.len());
        // Set the length to 0.
        self.try_shrink()?;
        // Write the string
        self.write_to_file(content)?;
        // Set the cursor to pos 0
        self.rewind_file()?;
        // Flush the changes
        self.get_file()?
            .flush()
            .map_err(|e| anyhow!("Failed to flush file {}", e))?;
        Ok(content_len)
    }

    fn read_lines(&mut self) -> LispResult<Expr> {
        let contents = self.read_all()?;
        let split: im::Vector<Expr> = contents
            .split('\n')
            .map(|s| Expr::string(s.into()))
            .collect();
        Ok(Expr::Tuple(split))
    }

    fn append(&mut self, content: String) -> LispResult<Expr> {
        let content_len = Expr::num(content.len());
        self.get_file()?
            .seek(std::io::SeekFrom::End(0))
            .map_err(|e| anyhow!("Could not seek to end of file! {}", e))?;
        self.write_to_file(content)?;
        self.rewind_file()?;
        Ok(content_len)
    }

    fn append_to_file(&mut self, content: String) -> LispResult<Expr> {
        self.append(content)
    }

    fn append_line(&mut self, content: String) -> LispResult<Expr> {
        self.append(format!("\n{}", content))
    }
}

impl RecordDoc for FileRecord {
    fn name() -> &'static str {
        "FileRecord"
    }

    fn type_doc() -> &'static str {
        "Manipulate files in x9."
    }

    fn method_doc() -> &'static [(&'static str, &'static str)] {
        &[
        // TODO:  Examples
        ]
    }
}
