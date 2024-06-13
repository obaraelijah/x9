use parking_lot::Mutex;
use std::sync::Arc;

pub(crate) struct StructRecord<T> {
    inner: Arc<Mutex<T>>,
    name: &'static str,
    initialized: bool,
    id: u64,
}
