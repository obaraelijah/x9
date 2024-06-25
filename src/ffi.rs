use std::{error::Error, path::Path, sync::Arc};

use anyhow::anyhow;
use bigdecimal::ToPrimitive;
use im::Vector;

use crate::ast::{Expr, Function, SymbolTable};

/// ForeignData is a trait that allows x9 to reason about
/// foreign data types by mapping Self to x9's Expr
/// and vice-versa.
///
/// As the mapping may not be 1:1, all conversions are fallible
/// result types.
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
///  Thread Safety Note:
/// 
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
        X9Interpreter { 
            symbol_table: crate::stdlib::create_stdlib_symbol_table_no_cli(),
        }
    }

    /// Recursively load a provided standard library directory.
    pub fn load_lib_dir<P: AsRef<Path>>(&self, lib_path: P) -> Result<(), Box<dyn Error>> {
        crate::modules::recursively_load_dir(false, lib_path, &self.symbol_table)
    }

    /// Add a foreign function to this x9 interpreter instance.
    /// # Example:
    ///
    /// ```rust
    /// use x9::ffi::{ExprHelper, ForeignData, X9Interpreter};
    /// use std::sync::Arc;
    ///
    /// let interpreter = X9Interpreter::new();
    /// let my_sum_fn = |args: Vec<u64>| Ok(args.iter().sum());
    /// // Add the my-sum to interpreter
    /// interpreter.add_function_ptr("my-sum", 1, Arc::new(my_sum_fn));
    ///
    /// // And verify we get u64 with value 6 out of it.
    /// assert_eq!(interpreter.run_program::<u64>("(my-sum 1 2 3)").unwrap(), 6);
    /// ```
    ///
    pub fn add_function_ptr<T: 'static + ForeignData>(
        &self,
        function_symbol: &str,
        minimum_args: usize,
        f: Arc<dyn Fn(Vec<T>) -> Result<T, Box<dyn Error + Send>> + Sync + Send>,
    ) {
        let x9_fn = move |args: Vector<Expr>, _sym: &SymbolTable| {
            let args: Result<Vec<_>, _> = args.iter().map(|item| T::from_x9(item)).collect();
            args.and_then(|args| (f)(args).and_then(|e| e.to_x9()))
                .map_err(|e| anyhow!("{:?}", e))
        };
        let x9_fn = Arc::new(x9_fn);

        let f = Function::new(function_symbol.into(), minimum_args, x9_fn, true);
        self.symbol_table.add_symbol(function_symbol.into(), Expr::function(f))
    }
}

/// Trait to help convert x9's Expr to primitive types.
/// 
/// Conversions are always fallible as you can callf
/// it on a wrong variant, or the primitive conversion
/// may not be possible (e.g. Expr::Num(2^100).to_u64())
pub trait ExprHelper {
    fn to_u64(&self) -> Result<u64, Box<dyn Error + Send>>;
    fn to_i64(&self) -> Result<i64, Box<dyn Error + Send>>;
    fn to_usize(&self) -> Result<usize, Box<dyn Error + Send>>;
    fn to_f64(&self) -> Result<f64, Box<dyn Error + Send>>;
    fn to_f32(&self) -> Result<f32, Box<dyn Error + Send>>;

    fn to_string(&self) -> Result<String, Box<dyn Error + Send>>;
}

impl ExprHelper for Expr {
    fn to_u64(&self) -> Result<u64, Box<dyn Error + Send>> {
        self.get_num().map_err(ErrorBridge::new).and_then(|n| {
            ToPrimitive::to_u64(&n)
                .ok_or_else(|| ErrorBridge::new(anyhow!("Could not convert {} to u64", self)))
        })
    }

    fn to_i64(&self) -> Result<i64, Box<dyn Error + Send>> {
        self.get_num().map_err(ErrorBridge::new).and_then(|n| {
            ToPrimitive::to_i64(&n)
                .ok_or_else(|| ErrorBridge::new(anyhow!("Could not convert {} to u64", self)))
        })
    }

    fn to_usize(&self) -> Result<usize, Box<dyn Error + Send>> {
        self.get_num().map_err(ErrorBridge::new).and_then(|n| {
            ToPrimitive::to_usize(&n)
                .ok_or_else(|| ErrorBridge::new(anyhow!("Could not convert {} to u64", self)))
        })
    }

    fn to_f64(&self) -> Result<f64, Box<dyn Error + Send>> {
        self.get_num().map_err(ErrorBridge::new).and_then(|n| {
            ToPrimitive::to_f64(&n)
                .ok_or_else(|| ErrorBridge::new(anyhow!("Could not convert {} to u64", self)))
        })
    }

    fn to_f32(&self) -> Result<f32, Box<dyn Error + Send>> {
        self.get_num().map_err(ErrorBridge::new).and_then(|n| {
            ToPrimitive::to_f32(&n)
                .ok_or_else(|| ErrorBridge::new(anyhow!("Could not convert {} to u64", self)))
        })
    }

    fn to_string(&self) -> Result<String, Box<dyn Error + Send>> {
        self.get_string().map_err(ErrorBridge::new)
    }
}

impl ExprHelper for bigdecimal::BigDecimal {
    fn to_u64(&self) -> Result<u64, Box<dyn Error + Send>> {
        ToPrimitive::to_u64(self)
            .ok_or_else(|| ErrorBridge::new(anyhow!("Could not convert {} to u64", self)))
    }

    fn to_i64(&self) -> Result<i64, Box<dyn Error + Send>> {
        ToPrimitive::to_i64(self)
            .ok_or_else(|| ErrorBridge::new(anyhow!("Could not convert {} to u64", self)))
    }

    fn to_usize(&self) -> Result<usize, Box<dyn Error + Send>> {
        ToPrimitive::to_usize(self)
            .ok_or_else(|| ErrorBridge::new(anyhow!("Could not convert {} to u64", self)))
    }

    fn to_f64(&self) -> Result<f64, Box<dyn Error + Send>> {
        ToPrimitive::to_f64(self)
            .ok_or_else(|| ErrorBridge::new(anyhow!("Could not convert {} to u64", self)))
    }

    fn to_f32(&self) -> Result<f32, Box<dyn Error + Send>> {
        ToPrimitive::to_f32(self)
            .ok_or_else(|| ErrorBridge::new(anyhow!("Could not convert {} to u64", self)))
    }

    fn to_string(&self) -> Result<String, Box<dyn Error + Send>> {
        Ok(ToString::to_string(&self))
    }
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
