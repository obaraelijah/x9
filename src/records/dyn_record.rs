use crate::records::{Record, RecordDoc};
use crate::ast::{Expr, Function, LispResult, Symbol, SymbolTable};
use dashmap::DashMap;
use std::sync::Arc;

#[derive(Default, Debug, Clone)]
pub struct DynRecord {
    name: Symbol,
    doc: Option<String>,
    fields: DashMap<Symbol, Expr>,
    initialized: bool,
    fields_order: Vec<Symbol>,
    methods: Arc<DashMap<Symbol, Function>>,
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

impl RecordDoc for DynRecord {
    fn name() -> &'static str {
        "DynRecord"
    }

    fn type_doc() -> &'static str {
        "Define a Record structure."
    }

    fn method_doc() -> &'static [(&'static str, &'static str)] {
        &[]
    }
}