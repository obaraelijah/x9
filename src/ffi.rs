use std::{error::Error, path::Path};

use crate::ast::{Expr, SymbolTable};

pub trait ForeignData
where
    Self: Sized,
{
    /// Convert from Self to x9's Expr type.
    fn to_x9(&self) -> Result<Expr, Box<dyn Error + Send>>;
    /// Convert x9's Expr type to Self.
    fn from_x9(expr: &Expr) -> Result<Self, Box<dyn Error + Send>>;
}

/// Struct to help type erase anyhow::Error to ease
/// interfacing with external programs.
struct ErrorBridge(anyhow::Error);

impl std::error::Error for ErrorBridge {}

impl ErrorBridge {
    fn new(err: anyhow::Error) -> Box<dyn Error + Send> {
        Box::new(ErrorBridge(err))
    }
}

impl std::fmt::Debug for ErrorBridge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl std::fmt::Display for ErrorBridge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

/// A X9 interpreter instance.
/// This type can be safely & cheaply cloned. This will copy
/// modifications to the symbol table (def, defn, foreign functions, etc).
///
/// # Example
///
/// ```rust
/// use x9::ffi::X9Interpreter;
///
/// let interpreter = X9Interpreter::new();
/// ```
#[derive(Clone)]
pub struct X9Interpreter {
    symbol_table: SymbolTable,
}

impl Default for X9Interpreter {
    fn default() -> Self {
        X9Interpreter::new()
    }
}

impl Drop for X9Interpreter {
    fn drop(&mut self) {
        self.symbol_table.wait_on_threads();
    }
}

impl X9Interpreter {
    /// Make a new interpreter instance.
    pub fn new() -> Self {
        todo!()
    }

    /// Recursively load a provided standard library directory.
    pub fn load_lib_dir<P: AsRef<Path>>(&self, lib_path: P) -> Result<(), Box<dyn Error>> {
        crate::modules::recursively_load_dir(false, lib_path, &self.symbol_table)
    }
}

/// Trait to help convert x9's Expr to primitive types.
pub trait ExprHelper {
    fn to_i64(&self) -> Result<u64, Box<dyn Error + Send>>;
}

#[macro_export]
macro_rules! convert_arg {
    ($t:ident, $e:expr) => {{
        match $t::from_x9($e) {
            Ok(v) => v,
            Err(e) => return Err(anyhow!("{e:?}")),
        }
    }};
}
