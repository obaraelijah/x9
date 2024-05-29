use anyhow::anyhow;
use bigdecimal::{BigDecimal, FromPrimitive, ToPrimitive};
use im::Vector;
use std::sync::Arc;

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
pub type Symbol = String;

#[derive(Clone, Hash)]
pub enum Expr {
    Num(Num),
    Integer(Integer),
    Symbol(String),
    List(Vector<Expr>),
    String(Arc<String>),
    Nil,
    Quote(Vector<Expr>),
    Tuple(Vector<Expr>),
    Bool(bool),
    Function(Function),
    Dict(Dict),
    ByteCompiledFunction(ByteCompiledFunction),
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
            _ => false,
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::String(s) => write!(f, "{}", s),
            rest => write!(f, "{:?}", rest),
        }
    }
}

fn debug_join(exprs: &Vector<Expr>) -> String {
    exprs
        .iter()
        .map(|s| format!("{:?}", s))
        .collect::<Vec<String>>()
        .join(" ")
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Integer(n) => write!(f, "{}", n),
            Expr::Nil => write!(f, "nil"),
            Expr::Num(n) => write!(f, "{}", n),
            Expr::String(s) => write!(f, "\"{}\"", s),
            Expr::Symbol(s) => write!(f, "{}", s),
            Expr::Function(ff) => write!(f, "{}", ff),
            Expr::Quote(l) => write!(f, "'({})", debug_join(l)),
            Expr::Bool(b) => write!(f, "{}", b),
            Expr::List(l) => write!(f, "({})", debug_join(l)),
            Expr::Tuple(l) => write!(f, "^({})", debug_join(l)),
            Expr::Dict(l) => write!(f, "{:?}", l),
            Expr::ByteCompiledFunction(ff) => write!(f, "{}", ff),
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
    pub(crate) fn num<T: ToNumericExpr>(number: T) -> Self {
        number.to_expr()
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
        }
    }

    pub(crate) fn string(s: String) -> Self {
        Expr::String(Arc::new(s))
    }

    pub(crate) fn push_front(&self, item: Expr) -> LispResult<Expr> {
        todo!()
    }

    pub(crate) fn function(f: Function) -> Self {
        Expr::Function(f)
    }

    pub(crate) fn get_list(&self) -> LispResult<Vector<Expr>> {
        if let Expr::List(l) = self {
            Ok(l.clone())
        } else if let Expr::Nil = self {
            Ok(Vector::new())
        } else if let Expr::Tuple(l) = self {
            Ok(l.clone())
        } else {
            bad_types!("list", self)
        }
    }
}

pub type LispResult<T> = anyhow::Result<T>;

#[derive(Clone)]
pub struct Function {
    eval_args: bool,
}

use std::hash::{Hash, Hasher};

impl Hash for Function {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.eval_args.hash(state);
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        // TODO: See if this is an issue. This should only appear in
        // one code generation unit (i.e. this crate), so it should be safe.
        todo!()
    }
}

impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug)]
pub(super) enum ProgramError {
    BadTypes, //context
}

impl std::fmt::Display for ProgramError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Eq for Expr {}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct ByteCompiledFunction {
    pub symbol: Symbol,
    pub minimum_args: usize,
    named_args: Box<[Symbol]>,
    pub loc: usize,
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
            write!(f, "{} ", arg)?;
        }
        write!(f, "], byte_compiled>")
    }
}

impl std::fmt::Display for ByteCompiledFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
