#![allow(clippy::unnecessary_wraps)]
use crate::ast::{Expr, Function, LispResult, SymbolTable};
use im::Vector;
use parking_lot::Mutex;
use rand::random;
use std::collections::HashSet;
use std::{hash::Hash, ops::Deref};

pub type IterType = Box<dyn LazyIter>;

pub trait LazyIter: std::fmt::Debug + std::fmt::Display + Sync + Send {
    fn next(&self, symbol_table: &SymbolTable) -> Option<LispResult<Expr>>;
    fn name(&self) -> &'static str;
    fn clone(&self) -> Box<dyn LazyIter>;
    fn id(&self) -> u64;
    fn eval(&self, symbol_table: &SymbolTable) -> LispResult<Expr> {
        let mut res = Vector::new();
        while let Some(ee) = self.next(symbol_table) {
            res.push_back(ee?)
        }
        Ok(Expr::List(res))
    }
}

impl Hash for IterType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id().hash(state)
    }
}

impl PartialEq for IterType {
    fn eq(&self, _other: &IterType) -> bool {
        false
    }
}

impl Clone for Box<dyn LazyIter> {
    fn clone(&self) -> Box<dyn LazyIter> {
        LazyIter::clone(self)
    }
}

impl LazyIter for IterType {
    fn next(&self, symbol_table: &SymbolTable) -> Option<LispResult<Expr>> {
        self.deref().next(symbol_table)
    }

    fn name(&self) -> &'static str {
        self.deref().name()
    }

    fn clone(&self) -> Box<dyn LazyIter> {
        self.deref().clone()
    }

    fn id(&self) -> u64 {
        self.deref().id()
    }
}

#[derive(Clone)]
pub(crate) struct LazyMap {
    inner: IterType,
    f: Function,
    id: u64,
}

impl LazyIter for LazyMap {
    fn next(&self, symbol_table: &SymbolTable) -> Option<LispResult<Expr>> {
        self.inner
            .next(symbol_table)
            .map(|lispres| lispres.and_then(|e| self.f.call_fn(Vector::unit(e), symbol_table)))
    }

    fn name(&self) -> &'static str {
        "Map"
    }

    fn clone(&self) -> IterType {
        Box::new(Clone::clone(self))
    }

    fn id(&self) -> u64 {
        self.id
    }
}

// TODO: Figure out this i.e Expurnge it
// impl std::fmt::Display for LazyMap {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{:?}", self.inner)
//     }
// }

impl LazyMap {
    pub(crate) fn lisp_res(inner: IterType, f: Function) -> LispResult<Expr> {
        Ok(Expr::LazyIter(Box::new(LazyMap {
            inner,
            f,
            id: random(),
        })))
    }
}

#[derive(Clone)]
pub(crate) struct LazyFilter {
    inner: IterType,
    f: Function,
    id: u64,
}

impl LazyIter for LazyFilter {
    fn next(&self, symbol_table: &SymbolTable) -> Option<LispResult<Expr>> {
        loop {
            match self.inner.next(symbol_table)? {
                Ok(item) => {
                    let pred_res = self
                        .f
                        .call_fn(Vector::unit(item.clone()), symbol_table)
                        .and_then(|fn_res| fn_res.is_truthy(symbol_table));
                    // Result<bool, Err>
                    match pred_res {
                        Ok(false) => continue,
                        Ok(true) => return Some(Ok(item)),
                        Err(e) => return Some(Err(e)),
                    }
                }
                Err(e) => return Some(Err(e)),
            }
        }
    }

    fn name(&self) -> &'static str {
        "LazyFilter"
    }

    fn clone(&self) -> Box<dyn LazyIter> {
        Box::new(Clone::clone(self))
    }

    fn id(&self) -> u64 {
        self.id
    }
}

// TODO: Figure out this i.e Expurnge it
// impl std::fmt::Display for LazyFilter {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{:?}", self.inner)
//     }
// }

impl LazyFilter {
    pub(crate) fn lisp_res(inner: IterType, f: Function) -> LispResult<Expr> {
        Ok(Expr::LazyIter(Box::new(LazyFilter {
            inner,
            f,
            id: random(),
        })))
    }
}

use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

#[derive(Default, Debug)]
struct Counter(AtomicUsize);

impl Clone for Counter {
    fn clone(&self) -> Self {
        let value = self.0.load(Ordering::SeqCst);
        Counter(AtomicUsize::new(value))
    }
}

impl Counter {
    fn new(value: usize) -> Self {
        Counter(AtomicUsize::new(value))
    }

    fn value(&self) -> usize {
        self.0.load(Ordering::SeqCst)
    }

    fn zero() -> Counter {
        Counter(AtomicUsize::new(0))
    }

    fn fetch_add_one(&self) -> usize {
        self.0.fetch_add(1, Ordering::SeqCst)
    }
}

impl std::fmt::Display for Counter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.value())
    }
}

#[derive(Clone)]
pub(crate) struct NaturalNumbers {
    counter: Counter,
    end: Option<usize>,
    id: u64,
}

impl NaturalNumbers {
    pub(crate) fn lisp_res(start: Option<usize>, end: Option<usize>) -> LispResult<Expr> {
        Ok(Expr::LazyIter(Box::new(NaturalNumbers {
            counter: Counter::new(start.unwrap_or(0)),
            end,
            id: random(),
        })))
    }
}

// impl std::fmt::Display for NaturalNumbers {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{:?}", self.counter)
//     }
// }

impl LazyIter for NaturalNumbers {
    fn next(&self, _symbol_table: &SymbolTable) -> Option<LispResult<Expr>> {
        let res = self.counter.fetch_add_one();
        if res >= self.end.unwrap_or(usize::MAX) {
            None
        } else {
            Some(Ok(Expr::num(res)))
        }
    }

    fn name(&self) -> &'static str {
        "NaturalNumbers"
    }

    fn clone(&self) -> Box<dyn LazyIter> {
        Box::new(Clone::clone(self))
    }

    fn id(&self) -> u64 {
        self.id
    }
}

use std::sync::Arc;

#[derive(Debug, Clone)]
pub(crate) struct LazyList {
    inner: Arc<Vector<Expr>>,
    index: Counter,
}

impl LazyList {
    pub(crate) fn lisp_new(inner: Vector<Expr>) -> LispResult<Expr> {
        let lazy = LazyList {
            inner: Arc::new(inner),
            index: Counter::zero(),
        };
        Ok(Expr::LazyIter(Box::new(lazy)))
    }
}

impl std::fmt::Display for LazyList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl LazyIter for LazyList {
    fn next(&self, _symbol_table: &SymbolTable) -> Option<LispResult<Expr>> {
        self.inner.get(self.index.fetch_add_one()).cloned().map(Ok)
    }

    fn name(&self) -> &'static str {
        "Lazy"
    }

    fn clone(&self) -> Box<dyn LazyIter> {
        Box::new(Clone::clone(self))
    }

    fn id(&self) -> u64 {
        0
    }
}

#[derive(Debug)]
pub(crate) struct Skip {
    inner: IterType,
    skipped: usize,
    have_skipped: AtomicBool,
}

impl Clone for Skip {
    fn clone(&self) -> Self {
        Self {
            inner: LazyIter::clone(&self.inner),
            skipped: self.skipped,
            have_skipped: AtomicBool::new(self.have_skipped.load(Ordering::SeqCst)),
        }
    }
}

impl Skip {
    pub(crate) fn lisp_res(skips_left: usize, inner: IterType) -> LispResult<Expr> {
        Ok(Expr::LazyIter(Box::new(Skip {
            inner,
            have_skipped: AtomicBool::new(false),
            skipped: skips_left,
        })))
    }
}

impl std::fmt::Display for Skip {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Skip<{}, {}>", self.skipped, self.inner,)
    }
}

impl LazyIter for Skip {
    fn next(&self, symbol_table: &SymbolTable) -> Option<LispResult<Expr>> {
        if !self.have_skipped.load(Ordering::SeqCst) {
            self.have_skipped.store(true, Ordering::SeqCst);
            for _ in 0..self.skipped {
                if let Err(e) = self.inner.next(symbol_table)? {
                    return Some(Err(e));
                }
            }
        }
        self.inner.next(symbol_table)
    }

    fn name(&self) -> &'static str {
        "Skip"
    }

    fn clone(&self) -> Box<dyn LazyIter> {
        Box::new(Clone::clone(self))
    }

    fn id(&self) -> u64 {
        random()
    }
}

#[derive(Debug)]
pub(crate) struct TakeWhile {
    pred: Function,
    inner: IterType,
    done: AtomicBool,
}

impl TakeWhile {
    pub(crate) fn lisp_res(pred: Function, inner: IterType) -> LispResult<Expr> {
        Ok(Expr::LazyIter(Box::new(TakeWhile {
            pred,
            inner,
            done: AtomicBool::new(false),
        })))
    }
}

impl Clone for TakeWhile {
    fn clone(&self) -> Self {
        Self {
            pred: self.pred.clone(),
            inner: LazyIter::clone(&self.inner),
            done: AtomicBool::new(self.done.load(Ordering::SeqCst)),
        }
    }
}

impl std::fmt::Display for TakeWhile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "TakeWhile<{}, {}, {}>",
            self.pred,
            self.inner,
            self.done.load(Ordering::SeqCst),
        )
    }
}

macro_rules! option_try {
    ($e:expr) => {
        match $e {
            Ok(val) => val,
            Err(e) => return Some(Err(e)),
        }
    };
}

impl LazyIter for TakeWhile {
    fn next(&self, symbol_table: &SymbolTable) -> Option<LispResult<Expr>> {
        // Check if the iteration is done
        if self.done.load(Ordering::SeqCst) {
            return None;
        }
        let res = option_try!(self.inner.next(symbol_table)?);
        let fn_res = option_try!(self
            .pred
            .call_fn(im::Vector::unit(res.clone()), symbol_table));
        let should_stop = !option_try!(fn_res.is_truthy(symbol_table));
        if should_stop {
            self.done.store(true, Ordering::SeqCst);
            None
        } else {
            Some(Ok(res))
        }
    }

    fn name(&self) -> &'static str {
        "TakeWhile"
    }

    fn clone(&self) -> Box<dyn LazyIter> {
        Box::new(Clone::clone(self))
    }

    fn id(&self) -> u64 {
        random()
    }
}

// impl Lazy {
//     fn lisp_res(list: Vector<Expr>) -> LispResult<Expr> {
//         Ok(Expr::LazyIter(Box::new()))
//     }
// }

// TODO: Expunge it
// #[derive(Debug)]
pub(crate) struct Take {
    inner: IterType,
    amount: AtomicUsize,
    id: u64,
}

impl Take {
    pub(crate) fn lisp_res(amount: usize, inner: IterType) -> LispResult<Expr> {
        Ok(Expr::LazyIter(Box::new(Take {
            amount: AtomicUsize::new(amount),
            inner,
            id: random(),
        })))
    }
}

impl Clone for Take {
    fn clone(&self) -> Take {
        Take {
            inner: Clone::clone(&self.inner),
            amount: AtomicUsize::new(self.amount.load(Ordering::SeqCst)),
            id: self.id,
        }
    }
}

// TODO: Expunge it
// impl std::fmt::Display for Take {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "Take<{}, {}>", self.inner, self.id,)
//     }
// }

impl LazyIter for Take {
    fn next(&self, symbol_table: &SymbolTable) -> Option<LispResult<Expr>> {
        if self.amount.load(Ordering::SeqCst) == 0 {
            None
        } else {
            self.amount.fetch_sub(1, Ordering::SeqCst);
            self.inner.next(symbol_table)
        }
    }
    fn name(&self) -> &'static str {
        "Take"
    }
    fn clone(&self) -> IterType {
        Box::new(Clone::clone(self))
    }
    fn id(&self) -> u64 {
        self.id
    }
}

// struct IndexGenerator {
//     max_values: Vec<usize>,
// }

// impl IndexGenerator {
//     fn new(max_values: Vec<usize>) -> Self {
//         Self { max_values }
//     }

//     fn get_indices(&self, count: usize) -> Vec<usize> {
//         let mut curr_count = count;
//         Vec::with_capacity(self.max_values.len()).iter_mut().zip(&self.max_values).for_each(|()| )
//         todo!()
//     }
// }

#[derive(Clone, Debug)]
struct Digit {
    curr: usize,
    max: usize,
}

impl Digit {
    fn new(max: usize) -> Self {
        Digit { curr: 0, max }
    }

    fn inc(&mut self) -> bool {
        self.curr += 1;
        if self.curr >= self.max {
            self.curr = 0;
            true
        } else {
            false
        }
    }

    fn value(&self) -> usize {
        self.curr
    }
}

// Whats next, impliment Distinct & Inspect & IndexGenerator Iterators...
pub(crate) struct Distinct {
    inner: IterType,
    seen: Arc<Mutex<HashSet<Expr>>>,
    id: u64,
}

impl Clone for Distinct {
    fn clone(&self) -> Self {
        Self {
            inner: LazyIter::clone(&self.inner),
            seen: self.seen.clone(),
            id: self.id,
        }
    }
}

impl std::fmt::Debug for Distinct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DistinctGenerator")
            .field("inner", &self.inner)
            .field("id", &self.id)
            .finish()
    }
}

impl std::fmt::Display for Distinct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", &self)
    }
}

impl LazyIter for Distinct {
    fn next(&self, symbol_table: &SymbolTable) -> Option<LispResult<Expr>> {
        loop {
            let item = option_try!(self.inner.next(symbol_table)?);
            let mut seen_guard = self.seen.lock();
            if seen_guard.contains(&item) {
                continue;
            } else {
                seen_guard.insert(item.clone());
                return Some(Ok(item));
            }
        }
    }

    fn name(&self) -> &'static str {
        "DistinctGenerator"
    }

    fn clone(&self) -> Box<dyn LazyIter> {
        Box::new(Clone::clone(self))
    }

    fn id(&self) -> u64 {
        self.id
    }
}

impl Distinct {
    pub(crate) fn lisp_res(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
        // TODO: check right number of args
        let distinct_generator = Distinct::new(exprs[0].get_iterator()?);
        Ok(Expr::LazyIter(Box::new(distinct_generator)))
    }
    fn new(inner: IterType) -> Self {
        Self {
            inner,
            seen: Default::default(),
            id: random(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Inspect {
    inner: IterType,
    inspect_function: Function,
    id: u64,
}

impl std::fmt::Display for Inspect {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl Inspect {
    pub(crate) fn lisp_res(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
        // TODO: check right number of args
        let inspect_function = exprs[0].get_function()?.clone();
        let inner = exprs[1].get_iterator()?;
        let inspect = Inspect {
            inner,
            inspect_function,
            id: random(),
        };
        Ok(Expr::LazyIter(Box::new(inspect)))
    }
}

impl LazyIter for Inspect {
    fn next(&self, symbol_table: &SymbolTable) -> Option<LispResult<Expr>> {
        let next = option_try!(self.inner.next(symbol_table)?);
        option_try!(self
            .inspect_function
            .call_fn(im::vector![next.clone()], symbol_table));
        Some(Ok(next))
    }

    fn name(&self) -> &'static str {
        "Inspect"
    }

    fn clone(&self) -> Box<dyn LazyIter> {
        Box::new(Clone::clone(self))
    }

    fn id(&self) -> u64 {
        self.id
    }
}

#[derive(Clone, Debug)]
struct IndexGenerator {
    digits: Arc<Mutex<Vec<Digit>>>,
    max_count: usize,
    counter: Counter,
}

impl IndexGenerator {
    fn new(max_values: &[usize]) -> Self {
        let digits = max_values.iter().copied().map(Digit::new).collect();
        let max_count = max_values.iter().product();
        IndexGenerator {
            digits: Arc::new(Mutex::new(digits)),
            max_count,
            counter: Counter::zero(),
        }
    }

    fn fetch_inc(&self) -> Option<Vec<usize>> {
        if self.counter.value() >= self.max_count {
            return None;
        }
        let mut digits = self.digits.lock();
        let ret = digits.iter().map(|d| d.value()).collect();
        for index in 0..digits.len() {
            if !digits[index].inc() {
                break;
            }
        }
        self.counter.fetch_add_one();
        Some(ret)
    }
}

#[derive(Clone)]
pub(crate) struct CartesianProduct {
    lists: Box<[Vector<Expr>]>,
    index_generator: IndexGenerator,
}

impl std::fmt::Debug for CartesianProduct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CartesianProduct")
            .field("lists", &self.lists)
            .field("indices", &self.index_generator)
            .finish()
    }
}

impl CartesianProduct {
    pub(crate) fn lisp_res(exprs: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
        // TODO: Avoid all of these intermediate allocations.
        let lists: Vec<Vector<Expr>> = exprs
            .into_iter()
            .map(|e| e.get_list())
            .collect::<Result<_, _>>()?;
        let max_values: Vec<_> = lists.iter().map(|e| e.len()).collect();
        let index_generator = IndexGenerator::new(&max_values);
        let me = CartesianProduct {
            lists: lists.into_boxed_slice(),
            index_generator,
        };
        Ok(Expr::LazyIter(Box::new(me)))
    }
}

impl std::fmt::Display for CartesianProduct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CartesianProduct<{:?}>", self.lists)
    }
}

impl LazyIter for CartesianProduct {
    fn next(&self, _symbol_table: &SymbolTable) -> Option<LispResult<Expr>> {
        let indices = self.index_generator.fetch_inc()?;
        let mut ret = Vector::new();
        for (idx, list) in indices.iter().zip(self.lists.iter()) {
            ret.push_back(list.get(*idx).cloned().unwrap_or(Expr::Nil));
        }
        Some(Ok(Expr::Tuple(ret)))
    }

    fn name(&self) -> &'static str {
        "CartesianProduct"
    }

    fn clone(&self) -> Box<dyn LazyIter> {
        Box::new(Clone::clone(self))
    }

    fn id(&self) -> u64 {
        random()
    }
}

macro_rules! impl_dbg_inner {
    ($($t:ident),*) => {
        $(
            impl std::fmt::Debug for $t {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "{}<{}>", self.name(), self.inner)
                }
            }
            impl std::fmt::Display for $t {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "{}<{}>", self.name(), self.inner)
                }
            }

        )*
    };
}

macro_rules! impl_dbg {
    ($($t:ident),*) => {
        $(
            impl std::fmt::Debug for $t {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "LazyIter<{}>", self.name())
                }
            }
            impl std::fmt::Display for $t {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "LazyIter<{}>", self.name())
                }
            }
        )*
    };
}

impl_dbg_inner!(LazyMap, LazyFilter, Take);
impl_dbg!(NaturalNumbers);
