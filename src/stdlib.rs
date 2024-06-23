use std::{
    io::Write,
    sync::{atomic::AtomicBool, Arc},
    time::{Duration, Instant},
};

use anyhow::{anyhow, bail, ensure, Context};
use bigdecimal::{BigDecimal, One,FromPrimitive, ToPrimitive};
use im::{vector, Vector};
use itertools::Itertools;

use crate::modules::load_x9_stdlib;
use crate::records::{ DictRecord, RecordDoc};
use crate::{
    ast::{Expr, Function, LispResult, ProgramError, SymbolTable},
    bad_types,
    cli::Options,
    interner::InternedString,
    iterators::{
        Distinct, Inspect, IterType, LazyFilter, LazyList, LazyMap, NaturalNumbers, Skip, Take,
        TakeWhile,
    },
};

/// Macro to check if we have the right number of args,
/// and throw a nice error if we don't.
#[macro_export]
macro_rules! exact_len {
    ($args:expr, $len:literal) => {
        use anyhow::ensure;
        use $crate::ast::ProgramError;
        ensure!($args.len() == $len, ProgramError::WrongNumberOfArgs($len))
    };
    ($args:expr, $($len:literal),*) => {
        {
            let mut is_ok_len = false;
            $(
                is_ok_len = is_ok_len || $args.len() == $len;
            )*
                if !is_ok_len {
                    let lengths = [$(
                        $len,
                    )*];
                    bail!(anyhow!(format!("Wrong number of args! Expected number of args to be in {:?} but received {}", lengths, $args.len())));
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

fn sqrt_exprs(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    let num_f64 = match &exprs[0] {
        Expr::Integer(i) => *i as f64,
        Expr::Num(i) => {
            if let Some(f) = i.to_f64() {
                f
            } else {
                return i
                    .sqrt()
                    .map(Expr::num)
                    .ok_or_else(|| anyhow!("Cannot square root a negative number!"));
            }
        }
        otherwise => return bad_types!("num or int", otherwise),
    };
    Ok(Expr::num(BigDecimal::from_f64(num_f64.sqrt()).unwrap()))
}

fn nth_root(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 2);
    let root = exprs[0]
        .get_num()?
        .to_f64()
        .ok_or_else(|| anyhow!("Failed to convert root to floating point"))?;
    if root <= 0.0 {
        bail!("{root} must be non-negative to nth_root!");
    }
    let num = exprs[1]
        .get_num()?
        .to_f64()
        .ok_or_else(|| anyhow!("Failed to convert num to floating point"))?;
    // TODO: Try out newton's method here (and find a good way of making guesses)

    // TODO: What if this fails? (sqrt -1, etc)
    let res = num.powf(1.0 / root);
    Ok(Expr::num(BigDecimal::from_f64(res).unwrap()))
}

// copilot coming in clutch :flushed:
fn print_smiley_face(_exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    println!("( ͡° ͜ʖ ͡°)");
    Ok(Expr::Nil)
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

// MISC

fn ident(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    Ok(exprs[0].clone())
}

fn ident_exists(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    let iden = exprs[0].get_symbol()?;
    Ok(Expr::Bool(symbol_table.symbol_exists(&iden)))
}

fn quote(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    Ok(Expr::Quote(exprs))
}

fn symbol(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    if let Ok(s) = exprs[0].get_string() {
        Ok(Expr::Symbol(s.into()))
    } else {
        bad_types!("string", exprs[0])
    }
}

fn string(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    if let Ok(s) = exprs[0].get_string() {
        Ok(Expr::string(s))
    } else {
        Ok(Expr::string(format!("{}", &exprs[0])))
    }
}

fn bool(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    exprs[0].is_truthy(symbol_table).map(Expr::Bool)
}

fn eval(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    exprs[0].eval(symbol_table)
}

fn parse(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    let program = exprs[0].get_string()?;
    let parse_res: Vector<Expr> = crate::parser::read(&program)
        .into_iter()
        .collect::<LispResult<_>>()?;
    Ok(Expr::Tuple(parse_res))
}

fn apply(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 2);
    exprs[0].call_fn(exprs[1].get_list()?, symbol_table)
}

fn err(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    let msg = exprs.iter().join("");
    Err(anyhow!(msg))
}

fn all_symbols(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 0);
    let all_syms = symbol_table.get_canonical_doc_order();
    Ok(Expr::List(
        all_syms
            .into_iter()
            .map(|e| Expr::Symbol(e.into()))
            .collect(),
    ))
}

fn include(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    let file_path = exprs[0].get_string()?;
    symbol_table.load_file(file_path)
}

fn doc(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    // TODO: Make records nice (merge Record and RecordDoc?)
    let sym = exprs[0].get_symbol_string()?;
    if let Some(doc) = symbol_table.get_doc_item(&sym.to_string()) {
        return Ok(Expr::string(doc));
    }

    let sym_eval = exprs[0].eval(symbol_table)?;
    if let Ok(f) = sym_eval.get_function() {
        if let Some(doc) = symbol_table.get_doc_item(&f.symbol.to_string()) {
            return Ok(Expr::string(doc));
        }
    }

    // Last ditch effort: eval it
    let doc = symbol_table
        .get_doc_item(&sym_eval.get_symbol_string()?.to_string())
        .unwrap_or_else(|| format!("No documentation for {}", sym));
    Ok(Expr::string(doc))
}

fn inline_transform(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 2);
    let data = exprs[0].get_list()?;
    let functions = exprs[1].get_list()?;

    Ok(Expr::Tuple(
        functions
            .iter()
            .zip(data)
            .map(|(f, x)| f.call_fn(im::Vector::unit(x), symbol_table))
            .collect::<LispResult<Vector<Expr>>>()?,
    ))
}

// XXX: Closure lifetime resolution is some magic shit.
//      For some reason it compiles now no idea why  ¯\_(ツ)_/¯
// #[inline(always)]
// fn lifetimes_are_hard<F>(f: F) -> F
// where
//     F: for<'c> Fn(Vector<Expr>, &'c SymbolTable) -> LispResult<Expr> + Sync + Send,
// {
//     f
// }

fn partial(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    if exprs.is_empty() {
        bail!("Partial requires at least one argument!");
    }
    let f = exprs[0]
        .get_function()
        .with_context(|| "Note: Partial requires the first item to be a function")?
        .clone();
    let rest_args = exprs.skip(1);
    let rest_args_len = rest_args.len();
    let remaining = f.minimum_args.saturating_sub(rest_args_len);
    let partial_fn_name = format!("Partial<{}; remaining={}>", f, remaining);
    let new_f = move |args: Vector<Expr>, sym: &SymbolTable| {
        let mut new_arg_list: Vector<Expr> = rest_args.iter().chain(args.iter()).cloned().collect();
        if new_arg_list.len() >= f.minimum_args {
            f.call_fn(new_arg_list, sym)
        } else {
            new_arg_list.push_front(Expr::function(f.clone()));
            partial(new_arg_list, sym)
        }
    };
    let ff = Function::new(partial_fn_name, remaining, Arc::new(new_f), true);
    Ok(Expr::function(ff))
}

// TODO: Make this work
fn comp(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    let compose = move |es, sym: &SymbolTable| {
        let mut res: Vector<Expr> = es;
        for func in exprs.iter().rev() {
            let fn_call = func.call_fn(res, sym);
            res = match fn_call {
                Ok(e) => Vector::unit(e),
                // Ok(l) => match l.get_list() {
                //     Ok(li) => li,
                //     Err(e) => return Err(e),
                // },
                Err(e) => return Err(e),
            }
        }
        Ok(Expr::List(res))
    };
    let f = Function::new("AnonCompFn".into(), 1, Arc::new(compose), true);
    Ok(Expr::function(f))
}

fn def(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 2);
    symbol_table.add_local(&exprs[0], &exprs[1].eval(symbol_table)?)?;
    Ok(Expr::Nil)
}

fn exprs_do(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    for expr in exprs.clone().slice(..exprs.len() - 1).iter() {
        expr.eval(symbol_table)?;
    }
    exprs[exprs.len() - 1].eval(symbol_table)
}

fn panic(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    let msg = if let Expr::String(s) = &exprs[0] {
        s.to_string()
    } else {
        format!("{}", exprs[0])
    };
    panic!("{}", msg);
}

fn sleep(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    let dur = exprs[0]
        .get_num()?
        .to_u64()
        .ok_or_else(|| anyhow!("Failed to convert {} to u64", exprs[0]))?;
    std::thread::sleep(Duration::from_secs(dur));
    Ok(Expr::Nil)
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

// FUNC

fn cond(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    ensure!(exprs.len() % 2 == 0, ProgramError::CondBadConditionNotEven);
    for (pred, body) in exprs.iter().tuples() {
        if pred.eval(symbol_table)?.is_truthy(symbol_table)? {
            return body.eval(symbol_table);
        }
    }
    bail!(ProgramError::CondNoExecutionPath)
}

fn expr_match(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    let item = exprs[0].eval(symbol_table)?;
    let mut iter = exprs.iter().skip(1);
    ensure!(
        (exprs.len() - 1) % 2 == 0,
        anyhow!("Match requires an even list of then")
    );
    while let Some(lhs) = iter.next() {
        let then = iter.next().unwrap();
        if lhs.is_symbol_underscore() {
            return then.eval(symbol_table);
        }
        let lhs = lhs.eval(symbol_table)?;
        if lhs == item {
            return then.eval(symbol_table);
        }
    }
    bail!(anyhow!("No execution paths for match!"))
}

fn if_gate(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 3);
    if exprs[0].eval(symbol_table)?.is_truthy(symbol_table)? {
        exprs[1].eval(symbol_table)
    } else {
        exprs[2].eval(symbol_table)
    }
}

fn map(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 2);
    let f = &exprs[0];
    if let Ok(iter) = exprs[1].get_iterator() {
        return LazyMap::lisp_res(iter, f.get_function()?.clone());
    }
    let mut l = exprs[1].get_list()?;
    for expr in l.iter_mut() {
        let old = expr.clone();
        *expr = f.call_fn(Vector::unit(old), symbol_table)?;
    }
    Ok(Expr::List(l))
}

fn mapt(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    map(exprs, symbol_table).map(|list| match list {
        Expr::List(l) => Expr::Tuple(l),
        // If the result is a lazy iterator, keep it unchanged
        ll @ Expr::LazyIter(_) => ll,
        _ => unreachable!(),
    })
}

fn threading_operator(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    let (item, funcs) = exprs.split_at(1);
    let mut res = item.get(0).cloned().unwrap_or(Expr::Nil);
    for func in funcs {
        res = func.call_fn(Vector::unit(res), symbol_table)?;
    }
    Ok(res)
}
// Like map, but doesn't produce a list.
fn foreach(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 2);
    let f = &exprs[0];
    if let Ok(iter) = exprs[1].get_iterator() {
        while let Some(x) = iter.next(symbol_table) {
            f.call_fn(Vector::unit(x?), symbol_table)?;
        }
    } else if let Ok(list) = exprs[1].get_list() {
        for x in list.iter() {
            f.call_fn(Vector::unit(x.clone()), symbol_table)?;
        }
    } else {
        bail!(ProgramError::BadTypes)
    };
    Ok(Expr::Nil)
}

fn filter(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    if exprs.len() == 1 {
        // TODO: Transducer case
        // return Transducer::new(|exprs, sym| filter(&exprs[0]))
        todo!()
    }
    exact_len!(exprs, 2);
    let f = &exprs[0];
    if let Ok(iter) = exprs[1].get_iterator() {
        return LazyFilter::lisp_res(iter, f.get_function()?.clone());
    }
    let l = exprs[1].get_list()?;
    let mut res = Vector::new();
    for expr in l {
        if f.call_fn(Vector::unit(expr.clone()), symbol_table)?
            .is_truthy(symbol_table)?
        {
            res.push_back(expr);
        }
    }
    Ok(Expr::List(res))
}

/// Example Usage
/// (reduce + 0 (list 1 2 3 4 5)) ; => 15
///(reduce * 1 (list 1 2 3 4 5)) ; => 120
fn reduce_iterator(
    f: &Expr,
    init: Option<Expr>,
    tail: IterType,
    symbol_table: &SymbolTable,
) -> LispResult<Expr> {
    let mut init = match init {
        Some(e) => e,
        None => tail.next(symbol_table).ok_or_else(|| {
            anyhow!("Attempted to reduce without initial argument using an empty list")
        })??,
    };
    while let Some(next) = tail.next(symbol_table) {
        init = f.call_fn(vector![init, next?], symbol_table)?;
    }
    Ok(init)
}

/// reduce
/// (f init coll)
fn reduce(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 2, 3);
    let f = &exprs[0];
    let (mut init, list) = if exprs.len() == 2 {
        if exprs[1].is_iterator() {
            let iter = exprs[1].get_iterator()?;
            return reduce_iterator(f, None, iter, symbol_table);
        }
        let mut list = exprs[1].get_list()?;
        ensure!(
            !list.is_empty(),
            "Attempted to reduce without initial argument using an empty list"
        );
        let head = list.pop_front().unwrap();
        (head, list)
    } else {
        if exprs[2].is_iterator() {
            let iter = exprs[2].get_iterator()?;
            return reduce_iterator(f, Some(exprs[1].clone()), iter, symbol_table);
        }
        (exprs[1].clone(), exprs[2].get_list()?)
    };
    for item in list {
        init = f.call_fn(vector![init, item], symbol_table)?;
    }
    Ok(init)
}

fn any(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 2);
    let pred = exprs[0].get_function()?;
    let body = &exprs[1].get_list()?;
    for b in body.iter().cloned() {
        if pred
            .call_fn(im::Vector::unit(b), symbol_table)?
            .is_truthy(symbol_table)?
        {
            return Ok(Expr::Bool(true));
        }
    }
    Ok(Expr::Bool(false))
}

fn all(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 2);
    let pred = exprs[0].get_function()?;
    let body = &exprs[1].get_list()?;
    for b in body.iter().cloned() {
        if !pred
            .call_fn(im::Vector::unit(b), symbol_table)?
            .is_truthy(symbol_table)?
        {
            return Ok(Expr::Bool(false));
        }
    }
    Ok(Expr::Bool(true))
}

fn skip(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 2);
    let skips_left = exprs[0].get_usize()?;
    let inner = exprs[1].get_iterator()?;
    Skip::lisp_res(skips_left, inner)
}

fn lazy(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    if let Ok(iter) = exprs[0].get_iterator() {
        return Ok(Expr::LazyIter(iter));
    }
    if let Ok(list) = exprs[0].get_list() {
        return LazyList::lisp_new(list);
    }
    // let iter = match &exprs[0] {
    //     Expr::LazyIter(iter) => iter.clone(),
    //     _ => return bad_types!("iter", &exprs[0]),
    // };
    // Ok(Expr::LazyIter(iter))
    Ok(Expr::Nil)
}

fn bind(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    let symbols = &exprs[0];
    let mut sym_copy = symbol_table.clone();
    let list = symbols.get_list()?;
    ensure!(
        list.len() % 2 == 0,
        anyhow!("Error: bind requires an even list of expressions, but was given a list of length {}. List given was: {}", list.len(), symbols)
    );

    // TODO: Use func_locals to avoid lock juggling overhead
    for (bind_sym, value) in list.into_iter().tuples() {
        let evaled_value = value.eval(&sym_copy)?;
        // List pattern sugar
        if let Ok(list) = bind_sym.get_list() {
            let vals_iter = evaled_value
                .get_list()?
                .into_iter()
                .chain(repeat(Expr::Nil));
            for (sym, val) in list.into_iter().zip(vals_iter) {
                sym_copy.add_func_local(sym, val)?;
            }
        } else {
            // TODO: Don't clone here
            sym_copy.add_func_local(bind_sym, evaled_value)?;
        }
    }

    exprs[1].eval(&sym_copy)
}

fn make_func(
    exprs: Vector<Expr>,
    symbol_table: &SymbolTable,
    name: InternedString,
) -> LispResult<Expr> {
    exact_len!(exprs, 2);
    let arg_symbols = exprs[0].get_list()?;
    let min_args = match arg_symbols.iter().position(|e| e.symbol_matches("&")) {
        Some(index) => index,
        None => arg_symbols.len(),
    };
    let body = exprs[1].clone();
    let f = Arc::new(move |_args: Vector<Expr>, sym: &SymbolTable| body.eval(sym));
    let f = Function::new_named_args(
        name,
        min_args,
        f,
        arg_symbols
            .iter()
            .map(|e| e.get_symbol_string())
            .try_collect()?,
        true,
        symbol_table
            .get_func_locals()
            .iter()
            .map(|(k, v)| (*k, v.clone()))
            .collect(),
    )?;
    Ok(Expr::function(f))
}

fn func(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    make_func(exprs, symbol_table, "AnonFn".into())
}

fn defn(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 3, 4);
    let (name, doc, args, body) = if exprs.len() == 3 {
        (exprs[0].clone(), None, exprs[1].clone(), exprs[2].clone())
    } else {
        (
            exprs[0].clone(),
            Some(exprs[1].eval(symbol_table)?.get_string()?),
            exprs[2].clone(),
            exprs[3].clone(),
        )
    };

    let sym_name = name.get_symbol_string()?;

    // Make a function
    let func = make_func(vector![args, body], symbol_table, sym_name)?;

    // Add the function to the symbol table
    def(vector![name, func.clone()], symbol_table)?;

    // If given docs, add it to the symbol table
    if let Some(doc) = doc {
        symbol_table.add_doc_item(sym_name.to_string(), doc);
    }

    // return the function
    Ok(func)
}

// TODO: Find a nicer name for this
fn anon_fn_sugar(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    let body = exprs[0].clone();
    let f = Arc::new(move |args: Vector<Expr>, sym: &SymbolTable| {
        let sym_args: Vec<_> = (1..args.len() + 1)
            .map(|i| format!("${}", i).into())
            .collect();
        let new_sym = sym.with_locals(&sym_args, None, args);
        body.eval(&new_sym)
    });
    let f = Function::new("AnonFn".into(), 0, f, true);
    Ok(Expr::Function(f))
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

fn set_dict(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 3);
    let mut dict = exprs[0].get_dict()?;
    let key = exprs[1].clone();
    let value = exprs[2].clone();
    dict.insert(key, value);
    Ok(Expr::Dict(dict))
}

fn values(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    let dict = exprs[0].get_dict()?;
    Ok(Expr::Tuple(dict.values().cloned().collect()))
}

fn time(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    let start = Instant::now();
    let _ = exprs[0].eval(symbol_table)?;
    let end = start.elapsed().as_millis() as u64;
    Ok(Expr::num(end))
}

fn interner_stats(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 0);
    let stats = InternedString::stats();
    Ok(Expr::string(stats))
}

// LISTS

fn list(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    Ok(Expr::List(exprs))
}

fn tuple(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    Ok(Expr::Tuple(exprs))
}

fn nth(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    let index = exprs[0].get_usize()?;
    if let Ok(string) = exprs[1].get_string() {
        Ok(string
            .chars()
            .nth(index)
            .map(|c| Expr::string(c.into()))
            .unwrap_or(Expr::Nil))
    } else {
        let list = exprs[1].get_list()?;
        list.get(index).cloned().ok_or_else(|| {
            anyhow::anyhow!(
                "Failed to nth as list has length {} but attempted to index {}",
                list.len(),
                index
            )
        })
    }
}

fn flatten(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    let l = exprs[0].get_list()?;
    let mut res = Vector::new();
    for item in l {
        match item {
            Expr::List(l) | Expr::Tuple(l) | Expr::Quote(l) => {
                l.into_iter().for_each(|i| res.push_back(i));
            }
            otherwise => res.push_back(otherwise),
        }
    }
    Ok(Expr::Tuple(res))
}

fn chars(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    Ok(Expr::Tuple(
        exprs[0]
            .get_string()?
            .chars()
            .map(|c| Expr::string(c.into()))
            .collect(),
    ))
}

fn split(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 2);
    let split_by = exprs[0].get_string()?;
    let string = exprs[1].get_string()?;
    Ok(Expr::Tuple(
        string
            .split(&split_by)
            .map(|substr| Expr::string(substr.into()))
            .collect(),
    ))
}

fn replace(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 3);
    let from = exprs[0].get_string()?;
    let to = exprs[1].get_string()?;
    let string = exprs[2].get_string()?;
    Ok(Expr::string(string.replace(&from, &to)))
}

fn cons(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 2);
    exprs[1].push_front(exprs[0].clone())
}

fn head(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    if let Ok(list) = exprs[0].get_list() {
        if list.is_empty() {
            return Ok(Expr::Nil);
        } else {
            return Ok(list[0].clone());
        }
    }
    let string = exprs[0].get_string()?;
    if string.is_empty() {
        Ok(Expr::Nil)
    } else {
        let first_char = match string.chars().next() {
            Some(c) => c.to_string(),
            None => "".to_string(),
        };
        Ok(Expr::string(first_char))
    }
}

fn tail(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    if let Ok(mut list) = exprs[0].get_list() {
        if list.is_empty() {
            return Ok(Expr::Nil);
        } else {
            return Ok(Expr::Tuple(list.slice(1..)));
        }
    }
    let string = exprs[0].get_string()?;
    if string.is_empty() {
        Ok(Expr::Nil)
    } else {
        let rest = string.chars().skip(1).collect();
        Ok(Expr::string(rest))
    }
}

fn zip(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 2);
    let l_iter = exprs[0].get_list()?;
    let r_iter = exprs[1].get_list()?;
    Ok(Expr::List(
        l_iter
            .into_iter()
            .zip(r_iter)
            .map(|(l, r)| Expr::Tuple(vector![l, r]))
            .collect(),
    ))
}

fn rev(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    // TODO: Any number of args.
    exact_len!(exprs, 1);
    if let Ok(list) = exprs[0].get_list() {
        return Ok(Expr::Tuple(list.into_iter().rev().collect()));
    }
    if let Ok(s) = &exprs[0].get_string() {
        return Ok(Expr::string(s.chars().rev().collect()));
    }
    bad_types!("string or list/quote/tuple", exprs[0])
}

fn range(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    if exprs.is_empty() {
        return NaturalNumbers::lisp_res(None, None);
    }
    // TODO: Always lazy calculate range, or add a new function for it.
    exact_len!(exprs, 1, 2);
    let (mut start, end) = if exprs.len() == 1 {
        use bigdecimal::Zero;
        (BigDecimal::zero(), exprs[0].get_num()?)
    } else {
        (exprs[0].get_num()?, exprs[1].get_num()?)
    };
    match (start.to_i64(), end.to_i64()) {
        // fast path
        (Some(start), Some(end)) => Ok(Expr::Tuple((start..end).map(Expr::num).collect())),
        _ => {
            let mut ret = Vector::new();
            while start < end {
                ret.push_back(Expr::num(start.clone()));
                start += BigDecimal::one();
            }
            Ok(Expr::Tuple(ret))
        }
    }
}

fn take(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 2);
    let num = exprs[0].get_usize()?;
    if let Ok(list) = exprs[1].get_list() {
        if num >= list.len() {
            return Ok(Expr::List(list));
        }
        let mut list = list;
        list.split_off(num);
        return Ok(Expr::List(list));
    }
    let iter = exprs[1].get_iterator()?;
    Take::lisp_res(num, iter)
}

fn take_while(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 2);
    let pred = exprs[0].get_function()?;
    if let Ok(list) = exprs[1].get_list() {
        let mut new_list = Vector::new();
        for value in list.into_iter() {
            if !pred
                .call_fn(Vector::unit(value.clone()), symbol_table)?
                .is_truthy(symbol_table)?
            {
                return Ok(Expr::List(new_list));
            }
            new_list.push_back(value);
        }
        return Ok(Expr::List(new_list));
    }
    let iter = exprs[1].get_iterator()?;
    TakeWhile::lisp_res(pred.clone(), iter)
}

fn find(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 2);
    let pred = exprs[0].get_function()?;
    let iter = exprs[1].get_iterator()?; // todo handle other iterable types
    while let Some(item) = iter.next(symbol_table) {
        let item = item?;
        if pred
            .call_fn(Vector::unit(item.clone()), symbol_table)?
            .is_truthy(symbol_table)?
        {
            return Ok(item);
        }
    }
    Ok(Expr::Nil)
}

fn slice(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 3);
    let lower = exprs[0].get_usize()?;
    let upper = exprs[1].get_usize()?;
    let mut list = {
        if let Ok(s) = exprs[2].get_string() {
            return Ok(Expr::string(s[lower..upper].to_string()));
        } else {
            exprs[2].get_list()?
        }
    };
    if lower >= list.len() {
        return Ok(Expr::Tuple(Vector::new()));
    }
    if upper < list.len() {
        let (left, _) = list.split_at(upper);
        list = left;
    }
    list = list.split_off(lower);
    Ok(Expr::Tuple(list))
}
fn doall(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    use crate::iterators::LazyIter;
    exprs[0].get_iterator()?.eval(symbol_table)
}

fn go(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    let ff = exprs[0].get_function()?.clone();
    let sym_clone = symbol_table.clone();
    let join_handle = std::thread::spawn(move || ff.call_fn(vector![], &sym_clone));
    symbol_table.add_join_handle(join_handle);
    Ok(Expr::Nil)
}

// TODO: Add chan

fn shuffle(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    let mut list: Vec<_> = exprs[0].get_list()?.iter().cloned().collect();
    use rand::seq::SliceRandom;
    use rand::thread_rng;
    list.shuffle(&mut thread_rng());
    Ok(Expr::Tuple(list.into()))
}

fn random_bool(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 0);
    let b: bool = rand::random();
    Ok(Expr::Bool(b))
}

fn random_int(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 2);
    let lower = exprs[0].get_num()?;
    let lower = lower
        .to_usize()
        .ok_or_else(|| anyhow!("Failed to convert {} to a usize!", &lower))?;
    let upper = exprs[1].get_num()?;
    let upper = upper
        .to_usize()
        .ok_or_else(|| anyhow!("Failed to convert {} to a usize!", &upper))?;
    let b: usize = rand::random::<usize>() % upper + lower;
    Ok(Expr::num(b))
}

fn primes(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    let num = exprs[0].get_num()?;
    let less_than: u32 = num
        .to_u32()
        .ok_or_else(|| anyhow!("Could not fit {} into a u32!", num))?;
    let mut seen = vec![2];
    for n in (3..less_than).step_by(2) {
        if !seen.iter().any(|e| n % e == 0) {
            seen.push(n);
        }
    }
    Ok(Expr::List(seen.iter().map(|&i| Expr::num(i)).collect()))
}

fn divisors(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    let num = exprs[0].get_int()?;
    let mut res = Vector::new();
    for i in 1..=num {
        if num % i == 0 {
            res.push_back(Expr::Integer(i));
            if i >= num / 2 {
                break;
            }
        }
    }
    res.push_back(Expr::Integer(num));
    Ok(Expr::Tuple(res))
}

fn clrf(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 0);
    Ok(Expr::string("\r\n".to_string()))
}

fn timestamp(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    let curr_time = chrono::Local::now();
    Ok(Expr::string(
        curr_time.format(&exprs[0].get_string()?).to_string(),
    ))
}

fn name_of(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    if let Expr::Function(f) = &exprs[0] {
        Ok(Expr::string(f.symbol.to_string()))
    } else if let Expr::Record(r) = &exprs[0] {
        Ok(Expr::string(r.type_name()))
    } else {
        Ok(Expr::Nil)
    }
}

// RECORDS

fn call_method(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    let rec = exprs[0].get_record()?;
    let method = &exprs[1].get_string()?;
    let args = exprs.clone().slice(2..);
    rec.call_method(method, args, symbol_table)
}

fn doc_methods(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    let sym: Cow<str> = match &exprs[0] {
        Expr::Symbol(s) => Cow::Owned(s.to_string()),
        Expr::Record(r) => r.type_name().into(),
        otherwise => return bad_types!("Symbol or Record", otherwise),
    };
    let docs = symbol_table
        .get_doc_methods(&sym)
        .into_iter()
        .map(|(doc, method)| Expr::Tuple(vector![Expr::string(doc), Expr::string(method)]))
        .collect();
    Ok(Expr::List(docs))
}

fn len(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    Ok(Expr::num(exprs[0].len(symbol_table)?))
}

fn sort(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    let mut list = exprs[0].full_order_list()?;
    list.sort();
    Ok(Expr::Tuple(list))
}
fn distinct(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    if let Ok(_iter) = exprs[0].get_iterator() {
        return Distinct::lisp_res(exprs, symbol_table);
    }
    let sorted = sort(exprs, symbol_table)?.get_list().unwrap();
    let mut v: Vec<_> = sorted.into_iter().collect();
    v.dedup();
    Ok(Expr::List(v.drain(..).collect()))
}

fn inspect(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    Inspect::lisp_res(exprs, symbol_table)
}

fn max_by(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 2); // TODO: Allow init arg
    let max_by_fn = exprs[0].get_function()?;
    let collection = &exprs[1].get_iterator()?;

    let mut curr_max = match collection.next(symbol_table) {
        Some(res) => res?,
        None => bail!("max-by called on an empty collection!"),
    };

    let mut curr_max_f = max_by_fn.call_fn(Vector::unit(curr_max.clone()), symbol_table)?;

    while let Some(item) = collection.next(symbol_table) {
        let item = item?;
        let item_f = max_by_fn.call_fn(Vector::unit(item.clone()), symbol_table)?;
        if item_f > curr_max_f {
            curr_max = item;
            curr_max_f = item_f;
        }
    }

    Ok(curr_max)
}

fn catch_err(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 1);
    let ret = match exprs[0].eval(symbol_table) {
        Ok(_) => Expr::Nil,
        Err(e) => Expr::string(format!("{}, with root cause:\n{}", e, e.root_cause())),
    };
    Ok(ret)
}

fn assert_eq(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
    exact_len!(exprs, 2, 3);
    let l = &exprs[0].eval(symbol_table)?;
    let r = &exprs[1].eval(symbol_table)?;
    let msg = match exprs.get(2) {
        Some(e) => e.get_string()?,
        None => String::new(),
    };

    if l != r {
        Err(anyhow!(
            "{}Left does not equal Right -- {} != {}\n\nHelp:\n\nLeft evaluated to: {}\nRight evaluated to: {}",
            msg,
            &exprs[0],
            &exprs[1],
            l,
            r
        ))
    } else {
        Ok(Expr::Nil)
    }
}

use std::borrow::Cow;
use std::iter::repeat;

macro_rules! make_stdlib_fns {
    ( $(($sym:literal, $minargs:expr, $func:expr, $eval_args:expr, $doc:expr)),* ) => {
        {
            let mut globals = Vec::new();
            let mut docs = Vec::new();
            $(
                let f = Function::new($sym.into(), $minargs, Arc::new($func), $eval_args);
                globals.push(($sym.into(), Expr::function(f)));
                docs.push(($sym.into(), $doc.into()));
            )*
            SymbolTable::with_globals(globals, docs)
        }
    };
}

macro_rules! document_records {
    ($sym:expr, $($rec:ident),*) => {
        $(
            // Document the record itself.
            $sym.add_doc_item($rec::name().into(), $rec::type_doc().into());
            for (method, method_doc) in $rec::method_doc() {
                $sym.add_doc_item(format!("{}.{}", $rec::name(), method), (*method_doc).into());
            }
        )*
    };
}



/// Create a symbol table without the x9 defined stdlib and
/// no user passed arguments. Useful for benchmarks.
pub fn create_stdlib_symbol_table_no_cli() -> SymbolTable {
    let opt = Options {
        // We haven't solved the $X9_PATH issue - i.e. where does
        // the x9 stdlib on the filesystem?
        do_not_load_native_stdlib: true,
        ..Default::default()
    };
    create_stdlib_symbol_table(&opt)
}

pub fn create_stdlib_symbol_table(opts: &Options) -> SymbolTable {
    todo!()
}
