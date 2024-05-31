use std::{hash::Hash, ops::Deref};

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

impl  LazyIter for IterType {
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