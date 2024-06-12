use std::{hash::Hash, ops::Deref};

use im::Vector;
use rand::random;

use crate::ast::{Expr, Function, LispResult, SymbolTable};

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

#[derive(Clone, Debug)]
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
impl std::fmt::Display for LazyMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.inner)
    }
}

impl LazyMap {
    pub(crate) fn lisp_res(inner: IterType, f: Function) -> LispResult<Expr> {
        Ok(Expr::LazyIter(Box::new(LazyMap {
            inner,
            f,
            id: random(),
        })))
    }
}

#[derive(Clone, Debug)]
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
impl std::fmt::Display for LazyFilter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.inner)
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

#[derive(Clone, Debug)]
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

impl std::fmt::Display for NaturalNumbers {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.counter)
    }
}

impl LazyIter for NaturalNumbers {
    fn next(&self, symbol_table: &SymbolTable) -> Option<LispResult<Expr>> {
        todo!()
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
    fn next(&self, symbol_table: &SymbolTable) -> Option<LispResult<Expr>> {
        todo!()
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
// }s
