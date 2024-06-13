use im::Vector;

use crate::ast::{Expr, LispResult, SymbolTable};

// ARITHMETIC

// TODO: Check if the types make sense to compare. (i.e. ordering, etc)
fn lt_exprs(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    let first = &exprs[0];
    let rest = exprs.iter().skip(1).all(|e| first < e);
    Ok(Expr::Bool(rest))
}

fn lte_exprs(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    let first = &exprs[0];
    let rest = exprs.iter().skip(1).all(|e| first <= e);
    Ok(Expr::Bool(rest))
}

fn gt_exprs(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    let first = &exprs[0];
    let rest = exprs.iter().skip(1).all(|e| first > e);
    Ok(Expr::Bool(rest))
}

fn gte_exprs(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    let first = &exprs[0];
    let rest = exprs.iter().skip(1).all(|e| first >= e);
    Ok(Expr::Bool(rest))
}
