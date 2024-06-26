use std::{collections::HashMap, error::Error, path::Path, sync::Arc};

use anyhow::anyhow;
use itertools::Itertools;
use num_traits::cast::ToPrimitive;
use im::Vector;

use crate::{ast::{Expr, Function, LispResult, SymbolTable}, interner::InternedString, parser::read, records::RecordType};

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
        self.symbol_table
            .add_symbol(function_symbol.into(), Expr::function(f))
    }

    /// Add a foreign function to this x9 interpreter instance.
    ///
    /// You'll want to use `.to_x9_fn()` here.
    ///
    /// # Example:
    ///
    /// ```rust
    /// use x9::ffi::{ExprHelper, ForeignData, X9Interpreter, IntoX9Function};
    ///
    /// let interpreter = X9Interpreter::new();
    /// let my_sum_fn = |a: u64, b: u64| a + b;
    ///
    /// // Add the function
    /// interpreter.add_function("my-sum", my_sum_fn.to_x9_fn());
    ///
    /// // Verify the output is correct
    /// assert_eq!(interpreter.run_program::<u64>("(my-sum 1 2)").unwrap(), 3);
    /// ```
    pub fn add_function(
        &self,
        function_symbol: &'static str,
        fn_tuple: (usize, crate::ast::X9FunctionPtr),
    ) {
        let (minimum_args, fn_ptr) = fn_tuple;
        let f = Function::new(function_symbol.into(), minimum_args, fn_ptr, true);
        self.symbol_table
            .add_symbol(function_symbol.into(), Expr::function(f));
    }

    /// Add a foreign function to this x9 interpreter instance, that doesn't
    /// evaluate it's arguments.
    ///
    /// Useful when dynamically generating functions.
    ///
    /// You'll want to use `.to_x9_fn()` here.
    pub fn add_unevaled_function(
        &self,
        function_symbol: &'static str,
        fn_tuple: (usize, crate::ast::X9FunctionPtr),
    ) {
        let (minimum_args, fn_ptr) = fn_tuple;
        let f = Function::new(function_symbol.into(), minimum_args, fn_ptr, false);
        self.symbol_table
            .add_symbol(function_symbol.into(), Expr::function(f));
    }

    /// Manually construct an x9 function, and add it to the interpreter.
    ///
    /// #Example
    ///
    /// ```rust
    /// use x9::ffi::{IntoX9Function, Variadic, X9Interpreter};
    /// use x9::symbols::Expr;
    ///
    /// fn embed_foreign_script(interpreter: &X9Interpreter) {
    ///     // (def-dyn-function my-sum (a b) (+ a b))
    ///     let interpreter_clone = interpreter.clone();
    ///     let f = move |args: Variadic<Expr>| {
    ///         let args = args.into_vec();
    ///         let fn_name = match args[0].get_symbol_string() {
    ///             Ok(sym) => sym,
    ///             Err(e) => return Err(e),
    ///         };
    ///         let f_args = args[1].clone(); // (arg1 arg2)
    ///         let f_body = args[2].clone(); // (redis "set" arg1 arg2)
    ///         let res = interpreter_clone.add_dynamic_function(fn_name, f_args, f_body);
    ///         res
    ///     };
    ///     interpreter
    ///         .add_unevaled_function("def-dyn-function", f.to_x9_fn());
    /// }
    /// ```
    pub fn add_dynamic_function<I: Into<InternedString>>(
        &self,
        function_sym: I,
        named_args: Expr,
        body: Expr,
    ) -> LispResult<Expr> {
        let arg_symbols = named_args.get_list()?;
        let args_len = arg_symbols.len();
        let interned_fn_name = function_sym.into();
        let f = Arc::new(move |_args: Vector<Expr>, sym: &SymbolTable| body.eval(sym));
        let f = Function::new_named_args(
            interned_fn_name,
            args_len,
            f,
            arg_symbols
                .into_iter()
                .map(|e| e.get_symbol_string())
                .try_collect()?,
            true,
            HashMap::new(),
        )?;

        self.symbol_table.add_symbol(interned_fn_name, Expr::function(f));
        Ok(Expr::Nil)
    }

    /// Run an x9 program.
    ///
    /// # Example:
    ///
    /// ```rust
    /// use x9::ffi::{ExprHelper, ForeignData, X9Interpreter, IntoX9Function};
    ///
    /// let interpreter = X9Interpreter::new();
    /// let my_sum_fn = |args: Vec<u64>| args.iter().sum::<u64>();
    /// // Add the my-sum to interpreter
    /// interpreter.add_function("my-sum", my_sum_fn.to_x9_fn());
    ///
    /// // And verify we get u64 with value 6 out of it.
    /// assert_eq!(interpreter.run_program::<u64>("(my-sum '(1 2 3))").unwrap(), 6);
    /// ```
    ///
    pub fn run_program<T: 'static + ForeignData>(
        &self,
        program: &str,
    )  -> Result<T, Box<dyn Error + Send>> {
        let mut last_expr = Expr::Nil;
        for expr in read(program) {
            last_expr = expr
                .and_then(|expr| expr.eval(&self.symbol_table))
                .map_err(ErrorBridge::new)?;
        }
        T::from_x9(&last_expr)
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

// This is hubris, but let's do it so the README looks nice.

impl ForeignData for u64 {
    fn to_x9(&self) -> Result<Expr, Box<dyn Error + Send>> {
        Ok(Expr::Num((*self).into()))
    }

    fn from_x9(expr: &Expr) -> Result<Self, Box<dyn Error + Send>> {
        expr.to_u64()
    }
}

impl ForeignData for i64 {
    fn to_x9(&self) -> Result<Expr, Box<dyn Error + Send>> {
        Ok(Expr::Num((*self).into()))
    }

    fn from_x9(expr: &Expr) -> Result<Self, Box<dyn Error + Send>> {
        expr.to_i64()
    }
}

impl ForeignData for u32 {
    fn to_x9(&self) -> Result<Expr, Box<dyn Error + Send>> {
        Ok(Expr::Num((*self).into()))
    }

    fn from_x9(expr: &Expr) -> Result<Self, Box<dyn Error + Send>> {
        expr.to_u64().map(|e| e as u32)
    }
}

impl ForeignData for f32 {
    fn to_x9(&self) -> Result<Expr, Box<dyn Error + Send>> {
        // use num_traits::FromPrimitive;
        let n = num_traits::FromPrimitive::from_f32(*self)
            .ok_or_else(|| ErrorBridge::new(anyhow!("Could not convert {self} to u64")))?;
        Ok(Expr::Num(n))
    }

    fn from_x9(expr: &Expr) -> Result<Self, Box<dyn Error + Send>> {
        expr.to_f32()
    }
}

impl ForeignData for usize {
    fn to_x9(&self) -> Result<Expr, Box<dyn Error + Send>> {
        let n = num_traits::FromPrimitive::from_usize(*self)
            .ok_or_else(|| ErrorBridge::new(anyhow!("Could not convert {self} to u64")))?;
        Ok(Expr::Num(n))
    }

    fn from_x9(expr: &Expr) -> Result<Self, Box<dyn Error + Send>> {
        expr.to_usize()
    }
}

impl ForeignData for () {
    fn to_x9(&self) -> Result<Expr, Box<dyn Error + Send>> {
        Ok(Expr::Nil)
    }

    fn from_x9(expr: &Expr) -> Result<Self, Box<dyn Error + Send>> {
        expr.get_nil().map_err(ErrorBridge::new)
    }
}

impl ForeignData for bool {
    fn to_x9(&self) -> Result<Expr, Box<dyn Error + Send>> {
        Ok(Expr::Bool(*self))
    }

    fn from_x9(expr: &Expr) -> Result<Self, Box<dyn Error + Send>> {
        match expr {
            Expr::Bool(b) => Ok(*b),
            otherwise => Err(Box::new(ErrorBridge(anyhow!("{otherwise:?}")))),
        }
    }
}

impl ForeignData for String {
    fn to_x9(&self) -> Result<Expr, Box<dyn Error + Send>> {
        Ok(Expr::string(self.clone()))
    }

    fn from_x9(expr: &Expr) -> Result<Self, Box<dyn Error + Send>> {
        Ok(format!("{expr}"))
    }
}

impl ForeignData for Expr {
    fn to_x9(&self) -> Result<Expr, Box<dyn Error + Send>> {
        Ok(self.clone())
    }

    fn from_x9(expr: &Expr) -> Result<Self, Box<dyn Error + Send>> {
        Ok(expr.clone())
    }
}

impl ForeignData for RecordType {
    fn to_x9(&self) -> Result<Expr, Box<dyn Error + Send>> {
        Ok(Expr::Record(self.clone()))
    }

    fn from_x9(expr: &Expr) -> Result<Self, Box<dyn Error + Send>> {
        expr.get_record().map_err(ErrorBridge::new)
    }
}

impl ForeignData for LispResult<usize> {
    fn to_x9(&self) -> Result<Expr, Box<dyn Error + Send>> {
        match self {
            Ok(e) => Ok(Expr::num(*e)),
            Err(ref e) => Err(ErrorBridge::new(anyhow!("{:?}", e))),
        }
    }

    fn from_x9(expr: &Expr) -> Result<Self, Box<dyn Error + Send>> {
        Ok(expr.get_usize())
    }
}

impl ForeignData for LispResult<Expr> {
    fn to_x9(&self) -> Result<Expr, Box<dyn Error + Send>> {
        match self {
            Ok(e) => Ok(e.clone()),
            Err(ref e) => Err(ErrorBridge::new(anyhow!("{:?}", e))),
        }
    }

    fn from_x9(expr: &Expr) -> Result<Self, Box<dyn Error + Send>> {
        Ok(Ok(expr.clone()))
    }
}

impl ForeignData for Vector<Expr> {
    fn to_x9(&self) -> Result<Expr, Box<dyn Error + Send>> {
        Ok(Expr::Tuple(self.clone()))
    }

    fn from_x9(expr: &Expr) -> Result<Self, Box<dyn Error + Send>> {
        expr.get_list().map_err(ErrorBridge::new)
    }
}

impl<T: ForeignData> ForeignData for Vec<T> {
    fn to_x9(&self) -> Result<Expr, Box<dyn Error + Send>> {
        Ok(Expr::Tuple(
            self.iter()
                .map(T::to_x9)
                .collect::<Result<Vector<_>, _>>()?,
        ))  
    }

    fn from_x9(expr: &Expr) -> Result<Self, Box<dyn Error + Send>> {
        let res = expr
            .get_list()
            .map_err(ErrorBridge::new)?
            .iter()
            .map(T::from_x9)
            .collect::<Result<Vec<_>, _>>()?;
        Ok(res)
    }
}

impl<T: ForeignData> ForeignData for Result<T, Box<dyn Error + Send>> {
    fn to_x9(&self) -> Result<Expr, Box<dyn Error + Send>> {
        match self {
            Ok(t) => t.to_x9(),
            Err(e) => Err(ErrorBridge::new(anyhow!("{:?}", e))),
        }
    }

    fn from_x9(expr: &Expr) -> Result<Self, Box<dyn Error + Send>> {
        Ok(T::from_x9(expr))
    }
}

impl<T: ForeignData> ForeignData for Option<T> {
    fn to_x9(&self) -> Result<Expr, Box<dyn Error + Send>> {
        match self {
            Some(item) => item.to_x9(),
            None => Ok(Expr::Nil),
        }
    }

    fn from_x9(expr: &Expr) -> Result<Self, Box<dyn Error + Send>> {
        match expr {
            Expr::Nil => Ok(None),
            otherwise => Ok(Some(T::from_x9(otherwise)?)),
        }
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
