use crate::ast::{Expr, Function, LispResult, Symbol, SymbolTable};
use crate::record;
use crate::records::{Record, RecordDoc};
use anyhow::bail;
use dashmap::DashMap;
use im::Vector;
use itertools::Itertools;
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
        if self.initialized {
            format!(
                "Record<{}, fields=[ {} ]>",
                self.name,
                self.fields_order
                    .iter()
                    .map(|v| format!("{}: {}", v, &*self.fields.get(v).unwrap()))
                    .join(" ")
            )
        } else {
            format!("Record<{}, uninitialized>", self.name)
        }
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

    fn call_as_fn(&self, args: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
        if args.len() != self.fields_order.len() {
            bail!(
                "{} requires {} fields [ {} ], but only received {} arguments",
                self.display(),
                self.fields_order.len(),
                self.fields_order.iter().join(" "),
                args.len()
            )
        }
        let fields = DashMap::new();
        let fields_value_iter = self.fields_order.iter().cloned().zip(args.iter().cloned());
        for (field, value) in fields_value_iter {
            fields.insert(field, value.eval(symbol_table)?);
        }
        let rec = DynRecord {
            fields,
            initialized: true,
            ..Clone::clone(self)
        };
        record!(rec)
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

impl DynRecord {
    
}