use crate::records::{Record, RecordDoc};
use crate::ast::{Expr, Function, LispResult, Symbol, SymbolTable};
use dashmap::DashMap;
use std::sync::Arc;

#[derive(Default, Debug, Clone)]
pub struct DynRecord {
    name: Symbol,
    // FIXME: Why did this field exist and why didn't I get a warning for it until now
    // doc: Option<String>,
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
        "Define a Record structure.

Use defmethod to add methods a record.

Example:
;; Define a record
(defrecord Vec3 \"Three Dimensional Vector\" x y z)

;; Instantiate a Vec3
(def v (Vec 1 2 3))

;; Access attributes

v.x    ;; 1
(.y v) ;; 2
"
    }

    fn method_doc() -> &'static [(&'static str, &'static str)] {
        todo!()
    }
}