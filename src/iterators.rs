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
}

impl Hash for IterType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id().hash(state)
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
        todo!()
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
        todo!()
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
        write!(f, "{:?}", self)
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
