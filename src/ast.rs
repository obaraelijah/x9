use bigdecimal::{BigDecimal, FromPrimitive, ToPrimitive};
use std::sync::Arc;

pub type Integer = i64;
pub type Num = BigDecimal;


pub enum Expr {
    Num(Num),
    Integer(Integer),
    Symbol(String),
    String(Arc<String>),
    Nil,
    Bool(bool),
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::String(s) => write!(f, "{}", s),
            rest => write!(f, "{:?}", rest),
        }
    }
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Integer(n) => write!(f, "{}", n),
            Expr::Nil => write!(f, "nil"),
            Expr::Num(n) => write!(f, "{}", n),
            Expr::String(s) => write!(f, "\"{}\"", s),
            Expr::Symbol(s) => write!(f, "{}", s),
            Expr::Bool(b) => write!(f, "{}", b),
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

impl ToNumericExpr for Integer {
    fn to_expr(self) -> Expr {
        
        Expr::Integer(self)
    }

    fn to_bigdecimal(self) -> Num {
        FromPrimitive::from_i64(self).unwrap()
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
}

pub type LispResult<T> = anyhow::Result<T>;
