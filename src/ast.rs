use crate::records::RecordType;
use anyhow::{anyhow, bail, Context, Result};
use bigdecimal::{BigDecimal, FromPrimitive, ToPrimitive, Zero};
use core::cmp::Ordering;
use im::Vector;
use itertools::Itertools;
use parking_lot::RwLock;
use std::collections::HashMap;
use std::sync::Arc;

use crate::interner::InternedString;
use crate::iterators::IterType;

#[macro_export]
macro_rules! bad_types {
    ($custom:expr) => {
        Err(anyhow!($crate::ast::ProgramError::BadTypes)).with_context(|| $custom)
    };

    ($expected:expr, $given:expr) => {{
        use anyhow::{anyhow, Context};
        Err(anyhow!($crate::ast::ProgramError::BadTypes)).with_context(|| {
            format!(
                "Error: Expected {}, but got type '{}': {:?}",
                $expected,
                $given.get_type_str(),
                $given
            )
        })
    }};
}

pub type Integer = i64;
pub type Num = BigDecimal;
pub type Dict = im::HashMap<Expr, Expr>;
pub type Symbol = InternedString;

#[allow(clippy::derived_hash_with_manual_eq)] // It's probably OK.
#[derive(Clone, Hash)]
pub enum Expr {
    Num(Num),
    Integer(Integer),
    Symbol(Symbol),
    List(Vector<Expr>),
    String(Arc<String>),
    Nil,
    Quote(Vector<Expr>),
    Tuple(Vector<Expr>),
    Bool(bool),
    Function(Function),
    Dict(Dict),
    ByteCompiledFunction(ByteCompiledFunction),
    LazyIter(IterType),
    Record(RecordType),
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Expr::Num(l), Expr::Num(r)) => l.eq(r),
            (Expr::Integer(l), Expr::Integer(r)) => l.eq(r),
            (Expr::Num(l), Expr::Integer(r)) => l.eq(&r.to_bigdecimal()),
            (Expr::Integer(l), Expr::Num(r)) => l.to_bigdecimal().eq(r),
            (Expr::Symbol(l), Expr::Symbol(r)) => l.eq(r),
            (Expr::String(l), Expr::String(r)) => l.eq(r),
            (Expr::List(l), Expr::List(r)) => l.eq(r),
            (Expr::Tuple(l), Expr::List(r)) => l.eq(r),
            (Expr::List(l), Expr::Tuple(r)) => l.eq(r),
            (Expr::Tuple(l), Expr::Tuple(r)) => l.eq(r),
            (Expr::Quote(l), Expr::Quote(r)) => l.eq(r),
            (Expr::Bool(l), Expr::Bool(r)) => l.eq(r),
            (Expr::Dict(l), Expr::Dict(r)) => l.eq(r),
            (Expr::Nil, Expr::Nil) => true,
            (Expr::LazyIter(_), Expr::LazyIter(_)) => false,
            _ => false,
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::String(s) => write!(f, "{s}"),
            rest => write!(f, "{rest:?}"),
        }
    }
}

fn debug_join(exprs: &Vector<Expr>) -> String {
    exprs
        .iter()
        .map(|s| format!("{s:?}"))
        .collect::<Vec<String>>()
        .join(" ")
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Integer(n) => write!(f, "{n}"),
            Expr::Nil => write!(f, "nil"),
            Expr::Num(n) => write!(f, "{n}"),
            Expr::String(s) => write!(f, "\"{s}\""),
            Expr::Symbol(s) => write!(f, "{s}"),
            Expr::Function(ff) => write!(f, "{ff}"),
            Expr::Quote(l) => write!(f, "'({})", debug_join(l)),
            Expr::Bool(b) => write!(f, "{b}"),
            Expr::List(l) => write!(f, "({})", debug_join(l)),
            Expr::Tuple(l) => write!(f, "^({})", debug_join(l)),
            Expr::Dict(l) => write!(f, "{l:?}"),
            Expr::ByteCompiledFunction(ff) => write!(f, "{ff}"),
            Expr::LazyIter(i) => write!(f, "{i}"),
            Expr::Record(l) => write!(f, "{l:?}"),
        }
    }
}

pub(crate) trait ToNumericExpr {
    fn to_expr(self) -> Expr;
    fn to_bigdecimal(self) -> Num;
}

impl ToNumericExpr for usize {
    fn to_expr(self) -> Expr {
        match self.try_into() {
            Ok(res) => Expr::Integer(res),
            _ => Expr::Num(FromPrimitive::from_usize(self).unwrap()),
        }
    }

    fn to_bigdecimal(self) -> Num {
        FromPrimitive::from_usize(self).unwrap()
    }
}

impl ToNumericExpr for u64 {
    fn to_expr(self) -> Expr {
        Expr::Num(FromPrimitive::from_u64(self).unwrap())
    }

    fn to_bigdecimal(self) -> Num {
        FromPrimitive::from_u64(self).unwrap()
    }
}

impl ToNumericExpr for u32 {
    fn to_expr(self) -> Expr {
        Expr::Num(FromPrimitive::from_u32(self).unwrap())
    }

    fn to_bigdecimal(self) -> Num {
        FromPrimitive::from_u32(self).unwrap()
    }
}

impl ToNumericExpr for i32 {
    fn to_expr(self) -> Expr {
        Expr::Integer(self as Integer)
    }

    fn to_bigdecimal(self) -> Num {
        FromPrimitive::from_i32(self).unwrap()
    }
}

impl ToNumericExpr for Integer {
    fn to_expr(self) -> Expr {
        Expr::Integer(self)
    }

    fn to_bigdecimal(self) -> Num {
        FromPrimitive::from_i64(self).unwrap()
    }
}

impl ToNumericExpr for f32 {
    fn to_expr(self) -> Expr {
        if self.trunc() == self {
            Expr::num(self.trunc() as u32)
        } else {
            Expr::Num(BigDecimal::from_f32(self).unwrap())
        }
    }

    fn to_bigdecimal(self) -> Num {
        FromPrimitive::from_f32(self).unwrap()
    }
}

impl ToNumericExpr for BigDecimal {
    fn to_expr(self) -> Expr {
        if self.is_integer() {
            match self.to_i64() {
                Some(i) => Expr::Integer(i),
                None => Expr::Num(self),
            }
        } else {
            Expr::Num(self)
        }
    }

    fn to_bigdecimal(self) -> Num {
        self
    }
}

impl Expr {
    pub(crate) fn full_order_list(&self) -> LispResult<Vector<Expr>> {
        let list = self.get_list()?;
        if list.is_empty() {
            Ok(list)
        } else {
            let head = &list[0]; // ref for type comparison
            if !list.iter().skip(1).all(|e| match (head, e) {
                (Expr::Num(_), Expr::Num(_)) => true,
                (Expr::Integer(_), Expr::Integer(_)) => true,
                (Expr::Num(_), Expr::Integer(_)) => true,
                (Expr::Integer(_), Expr::Num(_)) => true,
                (Expr::String(_), Expr::String(_)) => true,
                _ => false,
            }) {
                // only floats (sorta) + strings are totally ordered
                bad_types!("list of identically typed, ordered elements", self)
            } else {
                Ok(list)
            }
        }
    }

    pub(crate) fn num<T: ToNumericExpr>(number: T) -> Self {
        number.to_expr()
    }

    pub(crate) fn string(s: String) -> Self {
        Expr::String(Arc::new(s))
    }

    pub(crate) fn function(f: Function) -> Self {
        Expr::Function(f)
    }

    pub(crate) fn get_type_str(&self) -> &'static str {
        match self {
            Expr::Num(_) => "num",
            Expr::String(_) => "str",
            Expr::Integer(_) => "int",
            Expr::Bool(_) => "bool",
            Expr::Symbol(_) => "symbol",
            Expr::List(_) => "list",
            Expr::Tuple(_) => "tuple",
            Expr::Nil => "nil",
            Expr::Quote(_) => "quote",
            Expr::Function(_) => "function",
            Expr::Dict(_) => "map",
            Expr::ByteCompiledFunction(_) => "func",
            Expr::LazyIter(_) => "iterator",
            Expr::Record(_) => "record",
        }
    }

    pub(crate) fn push_front(&self, item: Expr) -> LispResult<Expr> {
        let mut list = self.get_list()?;
        list.push_front(item);
        let res = match self {
            Expr::List(_) => Expr::List(list),
            Expr::Quote(_) => Expr::Quote(list),
            Expr::Tuple(_) => Expr::Tuple(list),
            _ => unreachable!(),
        };
        Ok(res)
    }

    pub(crate) fn get_list(&self) -> LispResult<Vector<Expr>> {
        match self {
            Expr::List(l) => Ok(l.clone()),
            Expr::Nil => Ok(Vector::new()),
            Expr::Tuple(l) => Ok(l.clone()),
            Expr::Quote(l) => Ok(l.clone()),
            _ => bad_types!("list", self),
        }
    }

    pub(crate) fn get_nil(&self) -> LispResult<()> {
        match self {
            Expr::Nil => Ok(()),
            _ => bad_types!("nil", self),
        }
    }

    pub(crate) fn get_num(&self) -> LispResult<Num> {
        match self {
            Expr::Num(n) => Ok(n.clone()),
            Expr::Integer(n) => Ok(n.to_bigdecimal()),
            _ => bad_types!("num", self),
        }
    }

    pub(crate) fn get_int(&self) -> LispResult<Integer> {
        match self {
            // Expr::Num(n) => Ok(n.clone()), // TODO: Handle num being non-int
            Expr::Integer(n) => Ok(*n),
            _ => bad_types!("num", self),
        }
    }

    pub(crate) fn get_usize(&self) -> LispResult<usize> {
        let res = self.get_num()?.to_usize().ok_or(anyhow!(
            "Cannot represent {} as it needs to fit in a usize",
            self.get_num()?
        ))?;
        Ok(res)
    }

    pub fn get_string(&self) -> LispResult<String> {
        if let Expr::String(s) = self {
            Ok(s.to_string())
        } else {
            bad_types!("string", self)
        }
    }

    pub(crate) fn get_dict(&self) -> LispResult<Dict> {
        match self {
            Expr::Dict(d) => Ok(d.clone()),
            _ => bad_types!("dict", self),
        }
    }

    pub(crate) fn get_function(&self) -> LispResult<&Function> {
        match self {
            Expr::Function(f) => Ok(f),
            _ => bad_types!("func", self),
        }
    }
    pub(crate) fn get_symbol(&self) -> LispResult<Symbol> {
        match self {
            Expr::Symbol(s) => Ok(*s),
            _ => bad_types!("symbol", self),
        }
    }

    pub(crate) fn is_symbol_underscore(&self) -> bool {
        self.get_symbol_string()
            .ok()
            .map(|s| s.to_string() == "_")
            .unwrap_or(false)
    }

    pub fn get_symbol_string(&self) -> LispResult<InternedString> {
        match self {
            Expr::Symbol(s) => Ok(*s),
            Expr::Record(r) => Ok(InternedString::new(r.get_type_str())),
            _ => bad_types!("symbol", self),
        }
    }

    pub(crate) fn len(&self, symbol_table: &SymbolTable) -> LispResult<usize> {
        let len = match self {
            Expr::List(l) => l.len(),
            Expr::Tuple(l) => l.len(),
            Expr::Quote(l) => l.len(),
            Expr::Dict(m) => m.len(),
            Expr::String(s) => s.len(),
            Expr::Symbol(s) => s.len(),
            _ => return bad_types!("collection (list, tuple, record, etc)", self),
        };
        Ok(len)
    }

    pub(crate) fn is_iterator(&self) -> bool {
        match self {
            Expr::LazyIter(_) => true,
            _ => false,
        }
    }

    pub(crate) fn get_iterator(&self) -> LispResult<IterType> {
        match self {
            Expr::LazyIter(l) => Ok(l.clone()),
            _ => bad_types!("iterator", self),
        }
    }

    pub(crate) fn symbol_matches(&self, sym: &'static str) -> bool {
        if let Expr::Symbol(s) = self {
            s == sym
        } else {
            false
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct ByteCompiledFunction {
    pub symbol: Symbol,
    pub minimum_args: usize,
    named_args: Box<[Symbol]>,
    pub loc: usize,
    // TODO: Handle closures
}

impl ByteCompiledFunction {
    pub fn new(symbol: Symbol, minimum_args: usize, named_args: Box<[Symbol]>, loc: usize) -> Self {
        Self {
            symbol,
            minimum_args,
            named_args,
            loc,
        }
    }
}

impl std::fmt::Debug for ByteCompiledFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Fn<{}, min_args={}, loc={}, [ ",
            self.symbol, self.minimum_args, self.loc
        )?;
        for arg in self.named_args.iter() {
            write!(f, "{arg}")?;
        }
        write!(f, "], byte_compiled>")
    }
}

impl std::fmt::Display for ByteCompiledFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

// Smart pointer to a trait object
pub(crate) type X9FunctionPtr =
    Arc<dyn Fn(Vector<Expr>, &SymbolTable) -> LispResult<Expr> + Sync + Send>;

#[derive(Clone)]
pub struct Function {
    pub symbol: InternedString,
    pub minimum_args: usize,
    f: X9FunctionPtr,
    pub named_args: Box<[InternedString]>,
    extra_arg: Option<InternedString>,
    eval_args: bool,
    closure: Option<HashMap<InternedString, Expr>>,
}

use std::hash::{Hash, Hasher};

impl Hash for Function {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.symbol.hash(state);
        self.minimum_args.hash(state);
        self.named_args.hash(state);
        self.eval_args.hash(state);
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        // TODO: See if this is an issue. This should only appear in
        // one code generation unit (i.e. this crate), so it should be safe.
        #[allow(ambiguous_wide_pointer_comparisons)]
        Arc::ptr_eq(&self.f, &other.f)
    }
}

impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Fn<{}, {}, [ ", self.symbol, self.minimum_args)?;
        for arg in self.named_args.iter() {
            write!(f, "{} ", arg)?;
        }
        if let Some(extra_arg) = self.extra_arg {
            write!(f, "& {} ", extra_arg)?;
        }
        write!(f, "]>")
    }
}

macro_rules! try_collect {
    ($args:expr, $symbol_table:expr) => {{
        // Clone the list of arguments to avoid modifying the original list directly
        let mut args_clone = $args;

        // Iterate over each argument
        for arg in args_clone.iter_mut() {
            // Evaluate the argument using the provided symbol table
            // If evaluation fails, propagate the error using the `?` operator
            *arg = arg.eval(&$symbol_table)?;
        }
        args_clone
    }};
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl Function {
    pub fn new(symbol: String, minimum_args: usize, f: X9FunctionPtr, eval_args: bool) -> Self {
        Self {
            symbol: symbol.into(),
            minimum_args,
            f,
            named_args: Vec::with_capacity(0).into_boxed_slice(),
            extra_arg: None,
            eval_args,
            closure: None,
        }
    }

    pub fn new_named_args(
        symbol: InternedString,
        minimum_args: usize,
        f: X9FunctionPtr,
        named_args: Vec<InternedString>,
        eval_args: bool,
        closure: HashMap<InternedString, Expr>,
    ) -> LispResult<Self> {
        let extra_arg_symbol = InternedString::extra_arg_symbol();

        let (named_args, extra_arg) =
            if let Some(pos) = named_args.iter().position(|e| *e == extra_arg_symbol) {
                debug_assert!(named_args[pos] == extra_arg_symbol);
                let mut named_args = named_args;
                let rest = named_args.split_off(pos + 1);
                let extra_arg = *rest
                    .get(0)
                    .ok_or_else(|| anyhow!(ProgramError::ExpectedRestSymbol))?;
                named_args.pop();
                (named_args, Some(extra_arg))
            } else {
                (named_args, None)
            };

        Ok(Self {
            symbol,
            minimum_args,
            f,
            named_args: named_args.into_boxed_slice(),
            extra_arg,
            eval_args,
            closure: Some(closure),
        })
    }

    // Evaluating & executes a function with a given set of arguments
    pub(crate) fn call_fn(&self, args: Vector<Expr>, symbol_table: &SymbolTable) -> Result<()> {
        if self.minimum_args > args.len() {
            let args_joined = args.iter().join(" ");
            let args_pretty = if args_joined.is_empty() {
                "<nothing>".to_string()
            } else {
                args_joined
            };
            bail!(anyhow!(
                "Too few args supplied for {}. Expected {}, was given {} of length {}",
                &self,
                self.minimum_args,
                args_pretty,
                args.len()
            ));
        }
        // Handle closures
        // let closure;
        // let mut symbol_table = symbol_table;
        Ok(())
    }
}

#[derive(Debug)]
pub(crate) enum ProgramError {
    BadTypes, // context
    // InvalidCharacterInSymbol,
    // CannotStartExprWithNonSymbol,
    CondNoExecutionPath,
    CondBadConditionNotEven,
    DivisionByZero,
    // FailedToParseInt,
    // FailedToParseString,
    NotAFunction(Expr),
    // NotAList,
    // NotEnoughArgs(usize),
    // NotImplementedYet,
    ExpectedRestSymbol,
    // UnexpectedEOF,
    WrongNumberOfArgs(usize),
    // FailedToParse(String),
    // Custom(String),
}

impl std::fmt::Display for ProgramError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

pub type LispResult<T> = anyhow::Result<T>;

impl std::ops::Rem<&Expr> for Expr {
    type Output = LispResult<Expr>;

    fn rem(self, other: &Expr) -> LispResult<Expr> {
        match (&self, &other) {
            (Expr::Num(l), Expr::Num(r)) => Ok(Expr::num(l % r)),
            (Expr::Integer(l), Expr::Integer(r)) => Ok(Expr::num(l % r)),
            (Expr::Integer(l), Expr::Num(r)) => Ok(Expr::num(l.to_bigdecimal() % r)),
            (Expr::Num(l), Expr::Integer(r)) => Ok(Expr::num(l % r.to_bigdecimal())),
            _ => bad_types!(format!(
                "Remainder requires left and right are num types, was given {:?} % {:?}",
                &self, &other
            )),
        }
    }
}

impl std::ops::Add<&Expr> for Expr {
    type Output = LispResult<Expr>;

    fn add(self, other: &Expr) -> LispResult<Expr> {
        match (&self, &other) {
            (Expr::Num(l), Expr::Num(r)) => Ok(Expr::num(l + r)),
            (Expr::Integer(l), Expr::Integer(r)) => match l.checked_add(*r) {
                Some(res) => Ok(Expr::num(res)),
                None => Ok(Expr::num(l.to_bigdecimal() + r.to_bigdecimal())),
            },
            (Expr::Integer(l), Expr::Num(r)) => Ok(Expr::num(l.to_bigdecimal() + r)),
            (Expr::Num(l), Expr::Integer(r)) => Ok(Expr::num(l + r.to_bigdecimal())),
            (Expr::String(l), Expr::String(r)) => Ok(Expr::string(l.to_string() + r)),
            (Expr::Num(l), Expr::String(r)) => Ok(Expr::string(format!("{}{}", l, r))),
            (Expr::String(l), Expr::Num(r)) => Ok(Expr::string(format!("{}{}", l, r))),
            (Expr::String(l), Expr::Integer(r)) => Ok(Expr::string(format!("{}{}", l, r))),
            (Expr::Integer(l), Expr::String(r)) => Ok(Expr::string(format!("{}{}", l, r))),
            (Expr::List(l), Expr::List(r)) => {
                let mut res = l.clone();
                res.append(r.clone());
                Ok(Expr::List(res))
            }
            (Expr::Tuple(l), Expr::Tuple(r)) => {
                let mut res = l.clone();
                res.append(r.clone());
                Ok(Expr::Tuple(res))
            }
            (Expr::List(l), Expr::Nil) => Ok(Expr::List(l.clone())),
            (Expr::Nil, Expr::List(r)) => Ok(Expr::List(r.clone())),
            (Expr::Nil, Expr::Nil) => Ok(Expr::Nil),
            (Expr::List(l), Expr::Tuple(r)) => {
                Ok(Expr::Tuple(l.iter().chain(r).cloned().collect()))
            }
            (Expr::Tuple(l), Expr::List(r)) => {
                Ok(Expr::Tuple(l.iter().chain(r).cloned().collect()))
            }
            _ => bad_types!(format!(
                "Addition between these types doesn't make sense: {} + {}",
                &self, other
            )),
        }
    }
}

impl std::ops::Sub<&Expr> for Expr {
    type Output = LispResult<Expr>;

    fn sub(self, other: &Expr) -> LispResult<Expr> {
        match (&self, &other) {
            (Expr::Num(l), Expr::Num(r)) => Ok(Expr::num(l - r)),
            (Expr::Integer(l), Expr::Integer(r)) => match l.checked_sub(*r) {
                Some(res) => Ok(Expr::num(res)),
                None => Ok(Expr::num(l.to_bigdecimal() - r.to_bigdecimal())),
            },
            (Expr::Integer(l), Expr::Num(r)) => Ok(Expr::num(l.to_bigdecimal() - r)),
            (Expr::Num(l), Expr::Integer(r)) => Ok(Expr::num(l - r.to_bigdecimal())),
            _ => bad_types!(format!(
                "Subtraction between these types doesn't make sense: {} - {}",
                &self, other
            )),
        }
    }
}

impl std::ops::Mul<&Expr> for Expr {
    type Output = LispResult<Expr>;

    fn mul(self, other: &Expr) -> LispResult<Expr> {
        match (&self, &other) {
            (Expr::Num(l), Expr::Num(r)) => Ok(Expr::num(l * r)),
            (Expr::Integer(l), Expr::Integer(r)) => match l.checked_mul(*r) {
                Some(res) => Ok(Expr::num(res)),
                None => Ok(Expr::Num(l.to_bigdecimal() * r.to_bigdecimal())), // res is larger than i64
            },
            (Expr::Integer(l), Expr::Num(r)) => Ok(Expr::num(l.to_bigdecimal() * r)),
            (Expr::Num(l), Expr::Integer(r)) => Ok(Expr::num(l * r.to_bigdecimal())),
            (Expr::String(l), Expr::Integer(r)) => {
                if *r < 0 {
                    bad_types!(format!(
                        "Repeating a string negative times doesn't make sense: {} * {}",
                        &self, other
                    ))
                } else {
                    Ok(Expr::string(l.to_string().repeat(*r as usize)))
                }
            }

            (Expr::String(l), Expr::Num(r)) => {
                if *r >= BigDecimal::zero() {
                    Ok(Expr::string(
                        l.to_string().repeat(Expr::num(r.clone()).get_usize()?),
                    ))
                } else {
                    bad_types!(format!(
                        "Repeating a string negative times doesn't make sense: {} * {}",
                        &self, other
                    ))
                }
            }
            _ => bad_types!(format!(
                "Multiplication between these types doesn't make sense: {} * {}",
                &self, other
            )),
        }
    }
}

impl std::ops::Div<&Expr> for Expr {
    type Output = LispResult<Expr>;

    fn div(self, other: &Expr) -> LispResult<Expr> {
        match (&self, &other) {
            (Expr::Num(l), Expr::Num(r)) => {
                if *r == BigDecimal::zero() {
                    bail!(ProgramError::DivisionByZero);
                } else {
                    Ok(Expr::num(l / r))
                }
            }
            (_, Expr::Integer(0)) => bail!(ProgramError::DivisionByZero),
            (Expr::Integer(l), Expr::Integer(r)) => {
                if *r == 0 {
                    bail!(ProgramError::DivisionByZero);
                }
                match (l / r, l % r) {
                    (res, 0) => Ok(Expr::Integer(res)),
                    _ => Ok(Expr::num(l.to_bigdecimal() / r.to_bigdecimal())),
                }
            }
            (Expr::Num(l), Expr::Integer(r)) => Ok(Expr::num(l / r.to_bigdecimal())),
            (Expr::Integer(l), Expr::Num(r)) => {
                if *r == BigDecimal::zero() {
                    bail!(ProgramError::DivisionByZero)
                } else {
                    Ok(Expr::num(l.to_bigdecimal() / r))
                }
            }
            _ => bad_types!(format!(
                "Division between these types doesn't make sense: {} / {}",
                &self, other
            )),
        }
    }
}

impl PartialOrd for Expr {
    fn partial_cmp(&self, other: &Expr) -> Option<Ordering> {
        match (self, other) {
            (Expr::Num(l), Expr::Num(r)) => l.partial_cmp(r),
            (Expr::Integer(l), Expr::Integer(r)) => l.partial_cmp(r),
            (Expr::Num(l), Expr::Integer(r)) => l.partial_cmp(&r.to_bigdecimal()),
            (Expr::Integer(l), Expr::Num(r)) => l.to_bigdecimal().partial_cmp(r),
            (Expr::String(l), Expr::String(r)) => l.partial_cmp(r),
            _ => None,
        }
    }
}

impl Eq for Expr {}

impl Ord for Expr {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Expr::Num(l), Expr::Num(r)) => l.cmp(r),
            (Expr::Integer(l), Expr::Integer(r)) => l.cmp(r),
            (Expr::Num(l), Expr::Integer(r)) => l.cmp(&r.to_bigdecimal()),
            (Expr::Integer(l), Expr::Num(r)) => l.to_bigdecimal().cmp(r),
            (Expr::String(l), Expr::String(r)) => l.cmp(r),
            _ => Ordering::Less,
        }
    }
}

impl Expr {
    pub(crate) fn call_fn(
        &self,
        args: Vector<Expr>,
        symbol_table: &SymbolTable,
    ) -> Result<()>{
        match self {
            Expr::Function(f) => f.call_fn(args, symbol_table),
            _ => bail!(ProgramError::NotAFunction(self.clone())),
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Default)]
pub struct SymbolTable {
    globals: Arc<RwLock<HashMap<InternedString, Expr>>>,
    locals: Arc<RwLock<HashMap<InternedString, Expr>>>,
}
