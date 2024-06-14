use bigdecimal::BigDecimal;
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

fn rem_exprs(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    // TODO: Check for exact length
    exprs[0].clone() % &exprs[1]
}

fn or(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    for expr in exprs {
        if expr.is_truthy(symbol_table)? {
            return Ok(Expr::Bool(true));
        }
    }
    Ok(Expr::Bool(false))
}

fn and(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    for expr in exprs {
        if !expr.is_truthy(symbol_table)? {
            return Ok(Expr::Bool(false));
        }
    }
    Ok(Expr::Bool(true))
}

fn xor(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    if !exprs.is_empty() {
        let mut res = exprs[0].is_truthy(symbol_table)?;
        for b in exprs.iter().skip(1) {
            res ^= b.is_truthy(symbol_table)?;
        }
        Ok(Expr::Bool(res))
    } else {
        Ok(Expr::Bool(true))
    }
}

fn not(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    // TODO get exact len
    Ok(Expr::Bool(!exprs[0].is_truthy(symbol_table)?))
}

fn eq_exprs(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    let first = &exprs[0];
    let all_eq = exprs.iter().all(|x| first == x);
    Ok(Expr::Bool(all_eq)) 
}

fn add_exprs(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    let mut init = exprs[0].clone();
    exprs.iter().skip(1).try_fold(init, |acc, x| acc + x)
}

fn sub_exprs(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    let init = exprs[0].clone();
    if exprs.len() == 1 {
        return Ok(Expr::num(BigDecimal::from(-1) * init.get_num()?));
    }
    exprs.iter().skip(1).try_fold(init, |acc, x| acc - x)
}