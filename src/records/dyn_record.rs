use crate::ast::{Expr, Function, LispResult, Symbol, SymbolTable};
use crate::records::{Record, RecordDoc};
use dashmap::DashMap;
use im::Vector;
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

impl Record for DynRecord {
    fn call_method(
        &self,
        sym: &str,
        args: Vector<Expr>,
        symbol_table: &SymbolTable,
    ) -> LispResult<Expr> {
        self.call_method(sym, args, symbol_table)
    }

    fn has_method(&self, sym: &str) -> bool {
        self.methods.contains_key(&sym.into())
    }

    fn display(&self) -> String {
        todo!()
    }

    fn debug(&self) -> String {
        self.display()
    }

    fn clone(&self) -> super::RecordType {
        Box::new(Clone::clone(self))
    }

    fn methods(&self) -> Vec<String> {
        self.methods.iter().map(|m| m.key().to_string()).collect()
    }

    fn type_name(&self) -> String {
        self.name.to_string()
    }

    fn call_as_fn(&self, _args: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
        todo!()
    }

    fn defmethod(&self, args: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
        todo!()
    }

    fn is_equal(&self, other: &dyn Record) -> bool {
        match other.downcast_ref::<Self>() {
            Some(other_dyn) => self == other_dyn,
            None => false,
        }
    }   
}
