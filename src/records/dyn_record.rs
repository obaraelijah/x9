use dashmap::DashMap;
use im::Vector;

use crate::ast::{Expr, LispResult, Symbol, SymbolTable};
use crate::records::{Record, RecordDoc};

#[derive(Default, Debug, Clone)]
pub struct DynRecord {
    name: Symbol,
    fields: DashMap<Symbol, Expr>,
    initialized: bool,
}

impl PartialEq for DynRecord {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.fields.iter().all(|kv| {
                other
                    .fields
                    .get(kv.key())
                    .map(|other_v| *other_v == *kv.value())
                    .unwrap_or(false)
            })
    }
}
