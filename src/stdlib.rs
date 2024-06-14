use std::{
    io::Write,
    sync::{atomic::AtomicBool, Arc},
};

use anyhow::{anyhow, bail, ensure};
use bigdecimal::{BigDecimal, One, ToPrimitive};
use im::Vector;
use itertools::Itertools;

use crate::{
    ast::{Expr, Function, LispResult, SymbolTable},
    bad_types,
};

/// Macro to check if we have the right number of args,
/// and throw a nice error if we don't.
macro_rules! exact_len {
    // Single length case
    ($args:expr, $len:literal) => {
        use anyhow::ensure;
        use crate::ast::ProgramError;
        ensure!($args.len() == $len, ProgramError::WrongNumberOfArgs($len));
    };
    // Multiple lengths case
    ($args:expr, $($len:literal),+) => {
        {
            let expected_lengths = [$($len),+];
            if !expected_lengths.contains(&$args.len()) {
                bail!(anyhow!(format!(
                    "Wrong number of args! Expected number of args to be one of {:?} but received {}",
                    expected_lengths,
                    $args.len()
                )));
            }
        }
    };
}

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
    exact_len!(exprs, 2);
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
    exact_len!(exprs, 1);
    Ok(Expr::Bool(!exprs[0].is_truthy(symbol_table)?))
}

fn eq_exprs(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    let first = &exprs[0];
    let all_eq = exprs.iter().all(|x| first == x);
    Ok(Expr::Bool(all_eq))
}

fn add_exprs(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    let mut init = exprs[0].clone();
    for e in exprs.iter().skip(1) {
        init = (init + e)?;
    }
    // TODO: Figure out why this is slightly slower
    // exprs.iter().skip(1).try_fold(init, |acc, x| acc + x)

    Ok(init)
}

fn sub_exprs(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    let init = exprs[0].clone();
    if exprs.len() == 1 {
        return Ok(Expr::num(BigDecimal::from(-1) * init.get_num()?));
    }
    exprs.iter().skip(1).try_fold(init, |acc, x| acc - x)
}

fn mult_exprs(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    let init = exprs[0].clone();
    exprs.iter().skip(1).try_fold(init, |acc, x| acc * x)
}

fn div_exprs(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    let init = exprs[0].clone();
    exprs.iter().skip(1).try_fold(init, |acc, x| acc / x)
}

fn inc_exprs(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    let res = match exprs[0].clone() {
        Expr::Integer(i) => Expr::Integer(i + 1), // TODO: Handle overflow
        Expr::Num(n) => Expr::num(n + bigdecimal::BigDecimal::one()),
        otherwise => return bad_types!("num or int", otherwise),
    };
    Ok(res)
}

fn dec_exprs(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    let res = match exprs[0].clone() {
        Expr::Integer(i) => Expr::Integer(i - 1), // TODO: Handle overflow
        Expr::Num(n) => Expr::num(n - bigdecimal::BigDecimal::one()),
        otherwise => return bad_types!("num or int", otherwise),
    };
    Ok(res)
}

fn pow(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 2);
    let base = exprs[0].get_num()?;
    let exp = exprs[1].get_num()?.round(0).to_u32().unwrap(); // TODO: Handle error
    if exp == 0 {
        return Ok(Expr::num(BigDecimal::one()));
    }
    let mut res = base.clone();
    for _ in 0..(exp - 1) {
        res *= &base;
    }
    Ok(Expr::num(res))
}

fn int(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    if let Ok(s) = exprs[0].get_string() {
        let res: u64 = s
            .parse()
            .map_err(|_| anyhow!("Could not convert to an int."))?;
        return Ok(Expr::num(res));
    }
    let res = match &exprs[0] {
        Expr::Integer(i) => Expr::Integer(*i),
        Expr::Num(i) => Expr::num(i.round(0)),
        otherwise => return bad_types!("num", otherwise),
    };
    Ok(res)
}

fn floor(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    let n = exprs[0]
        .get_num()?
        .to_f64()
        .ok_or_else(|| anyhow!("Number cannot be converted to a floating point"))?
        .trunc()
        .to_u64()
        .ok_or_else(|| anyhow!("Truncated floating point could not be converted a u64"))?;
    Ok(Expr::num(BigDecimal::from(n)))
}

// PRINT

fn print(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    for expr in &exprs {
        print!("{}", expr);
        if let Err(e) = std::io::stdout().flush() {
            eprintln!("Failed to flush stdout! {e}");
        }
    }
    Ok(Expr::num(exprs.len()))
}

fn println(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    let item = exprs.iter().join("");
    println!("{item}");
    Ok(Expr::Nil)
}

fn input(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    let mut buf = String::new();
    print(exprs, _symbol_table)?;
    std::io::stdin()
        .read_line(&mut buf)
        .map_err(|e| anyhow!("{e}"))?;
    Ok(Expr::string(buf.trim().to_string()))
}

fn type_of(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    Ok(Expr::string(exprs[0].get_type_str().into()))
}

fn do_loop(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1, 2);
    // TODO: Handle args / add recur
    let _args = exprs[0].get_list()?;
    let body = &exprs[1];
    let break_flag = Arc::new(AtomicBool::new(false));
    let break_flag_clone = break_flag.clone();
    let break_fn_f = move |ex: Vector<Expr>, _sym: &SymbolTable| {
        exact_len!(ex, 0);
        break_flag_clone.store(true, std::sync::atomic::Ordering::SeqCst);
        Ok(Expr::Nil)
    };
    let break_fn = Function::new("break".into(), 0, Arc::new(break_fn_f), false);
    let mut new_sym = symbol_table.clone();
    new_sym.add_func_local_str("break", Expr::function(break_fn));
    loop {
        if break_flag.load(std::sync::atomic::Ordering::SeqCst) {
            break;
        }
        body.eval(&new_sym)?;
    }
    // (loop () (println "Hello World"))
    // (loop (a b c) (expression))
    // (break)
    // (recur 1 2 3)
    Ok(Expr::Nil)
}

// Dict

fn make_dict(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    ensure!(
        exprs.len() % 2 == 0,
        "Error: dict requires an even list of arguments."
    );
    let mut dict = im::HashMap::new();
    for (key, value) in exprs.iter().tuples() {
        dict.insert(key.clone(), value.clone());
    }
    Ok(Expr::Dict(dict))
}

fn assoc(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    let mut dict = exprs[0].get_dict()?;
    for (key, value) in exprs.iter().skip(1).tuples() {
        dict.insert(key.clone(), value.clone());
    }
    Ok(Expr::Dict(dict))
}

fn remove(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    let mut dict = exprs[0].get_dict()?;
    for key in exprs.iter().skip(1) {
        dict.remove(key);
    }
    Ok(Expr::Dict(dict))
}

fn get_dict(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 2);
    let dict = exprs[0].get_dict()?;
    let res = dict.get(&exprs[1]).cloned().unwrap_or(Expr::Nil);
    Ok(res)
}