use parking_lot::Mutex;
use std::sync::Arc;

type CloneFn<T> = Arc<dyn Fn(&T) -> T + Sync + Send>;

pub(crate) struct StructRecord<T> {
    inner: Arc<Mutex<T>>,
    name: &'static str,
    fields: Arc<Vec<&'static str>>,
    clone_fn: Option<CloneFn<T>>,
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
            fields: self.fields.clone(),
            clone_fn: self.clone_fn.clone(),
            initialized: self.initialized, 
            id,
        }
    }
}

impl<T>  StructRecord<T> {
    pub(crate) fn record_builder(name: &'static str) -> StructRecord<T> {
        todo!()
    }
}