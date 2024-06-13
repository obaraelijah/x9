use std::sync::Arc;
use parking_lot::Mutex;

pub(crate) struct StructRecord<T> {
    inner: Arc<Mutex<T>>,
    name: &'static str,
    initialized: bool,
    id: u64,
}