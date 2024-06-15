use im::Vector;
use parking_lot::Mutex;
use std::{collections::HashMap, sync::Arc};

use crate::ast::{Expr, LispResult, SymbolTable};

type ReadFn<T> = Box<dyn Fn(&StructRecord<T>, Vector<Expr>, &SymbolTable) -> LispResult<Expr> + Sync + Send>;
type WriteFn<T> = Box<dyn Fn(&StructRecord<T>, Vector<Expr>, &SymbolTable) -> LispResult<Expr> + Sync + Send>;
type CloneFn<T> = Arc<dyn Fn(&T) -> T + Sync + Send>;
type InitFn<T> = Arc<dyn Fn(Vector<Expr>, &SymbolTable) -> LispResult<T> + Sync + Send>;
type DisplayFn<T> = Arc<dyn Fn(&T) -> String + Sync + Send>;

pub(crate) struct StructRecord<T> {
    inner: Arc<Mutex<T>>,
    name: &'static str,
    read_method_map: Arc<HashMap<&'static str, ReadFn<T>>>,
    write_method_map: Arc<HashMap<&'static str, WriteFn<T>>>,
    fields: Arc<Vec<&'static str>>,
    clone_fn: Option<CloneFn<T>>,
    init_fn: Option<InitFn<T>>,
    display_fn: Option<DisplayFn<T>>,
    initialized: bool,
    id: u64,
}

impl<T> Clone for StructRecord<T> {
    fn clone(&self) -> Self {
        let (inner, id) = match self.clone_fn {
            Some(ref ff) => {
                let guard = self.inner.lock();
                (Arc::new(Mutex::new((ff)(&guard))), rand::random())
            }
            None => (Arc::clone(&self.inner), self.id),
        };
        Self {
            inner,
            name: self.name,
            read_method_map: self.read_method_map.clone(),
            write_method_map: self.write_method_map.clone(),
            fields: self.fields.clone(),
            clone_fn: self.clone_fn.clone(),
            init_fn: self.init_fn.clone(),
            display_fn: self.display_fn.clone(),
            initialized: self.initialized,
            id,
        }
    }
}

impl<T: PartialEq + 'static> StructRecord<T> {
    fn clone_with_new_inner(&self, new_inner: T) -> Self {
        StructRecord {
            inner: Arc::new(Mutex::new(new_inner)),
            name: self.name,
            read_method_map: self.read_method_map.clone(),
            write_method_map: self.write_method_map.clone(),
            fields: self.fields.clone(),
            clone_fn: self.clone_fn.clone(),
            init_fn: self.init_fn.clone(),
            display_fn: self.display_fn.clone(),
            initialized: self.initialized,
            id: rand::random(),
        }
    }
}

impl<T: 'static + PartialEq> StructRecord<T> {
    pub(crate) fn record_builder_with(name: &'static str, inner: T) -> Self {
        StructRecord {
            inner: Arc::new(Mutex::new(inner)),
            name,
            read_method_map: Default::default(),
            write_method_map: Default::default(),
            fields: Arc::new(Vec::new()),
            clone_fn: None,
            init_fn: None,
            display_fn: None,
            initialized: true,
            id: rand::random(),
        }
    }
}

impl<T: Default + 'static + PartialEq> StructRecord<T> {
    pub(crate) fn record_builder(name: &'static str) -> StructRecord<T> {
        StructRecord {
            inner: Arc::new(Mutex::new(T::default())),
            name,
            read_method_map: Default::default(),
            write_method_map: Default::default(),
            fields: Arc::new(Vec::new()),
            clone_fn: None,
            init_fn: None,
            display_fn: None,
            initialized: false,
            id: rand::random(),
        }
    }
}

impl<T> StructRecord<T> {
    pub(crate) fn add_method<Args, Out, F: IntoReadFn<Args, T, Out>>(
        
    ) -> Self {
        todo!()
    }
}

// TODO: Use a macro for this
pub(crate) trait IntoReadFn<Args, T, Out> {
    fn into_read_fn(self) -> ReadFn<T>;
}

pub(crate) trait IntoWriteFn<Args, T, Out> {
    fn into_write_fn(self) -> WriteFn<T>;
}