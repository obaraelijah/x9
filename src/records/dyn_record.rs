use crate::ast::{Expr, Function, LispResult, Symbol, SymbolTable};
use crate::records::{Record, RecordDoc};
use crate::{record, unknown_method};
use anyhow::bail;
use dashmap::DashMap;
use im::Vector;
use itertools::Itertools;
use std::collections::HashMap;
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

    fn defmethod(&self, _args: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
        todo!()
    }

    fn is_equal(&self, other: &dyn Record) -> bool {
        match other.downcast_ref::<Self>() {
            Some(other_dyn) => self == other_dyn,
            None => false,
        }
    }
}

// (defrecord rec-name "optional-doc" field1 field2 field3)
// Adds rec-name to symbol table
// (.defmethod rec-name method-name
//   "optional doc"
//   (arg1)
//   ;; body
//   (+ arg1 field1 field2 field3)) ;; fields are added to the symbol table

impl DynRecord {
    pub fn defrecord(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
        let name = exprs[0].get_symbol_string()?;
        let mut skip_to_fields = 1;
        if let Some(s) = exprs.get(1) {
            if let Ok(s) = s.get_string() {
                skip_to_fields += 1;
                symbol_table.add_doc_item(name.to_string(), s);
            }
        }
        let fields_order = exprs
            .iter()
            .skip(skip_to_fields)
            .map(|e| e.get_symbol_string())
            .try_collect()?;
        let rec = DynRecord {
            name,
            fields_order,
            initialized: false,
            ..Default::default()
        };
        let rec = Expr::Record(Box::new(rec));
        symbol_table.add_local(&exprs[0], &rec)?;
        Ok(rec)
    }

    pub fn defmethod_x9(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
        let rec = exprs[0].eval(symbol_table)?.get_record()?;
        rec.defmethod(exprs.skip(1), symbol_table)
    }

    #[allow(dead_code)]
    fn add_method_x9(&self, exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
        if self.initialized {
            bail!("Cannot add methods to initialized record {}", self.name)
        }
        let method_name = exprs[0].get_symbol_string()?;
        let method_symbol = format!("{}.{}", self.type_name(), method_name);
        // TODO: Functions && doc
        let (arg_list, body) = if let Ok(doc) = exprs[1].get_string() {
            symbol_table.add_doc_item(method_symbol.clone(), doc);
            (exprs[2].get_list()?, exprs[3].clone())
        } else {
            (exprs[1].get_list()?, exprs[2].clone())
        };
        let method_fn = move |_args: Vector<Expr>, sym: &SymbolTable| body.eval(sym);
        let f = Function::new_named_args(
            method_symbol.into(),
            arg_list.len(),
            Arc::new(method_fn),
            arg_list
                .into_iter()
                .map(|e| e.get_symbol_string())
                .try_collect()?,
            true,
            HashMap::new(),
        )?;
        self.methods.insert(method_name, f);
        Ok(Expr::Nil)
    }

    fn call_method(
        &self,
        method_name: &str,
        args: Vector<Expr>,
        symbol_table: &SymbolTable,
    ) -> LispResult<Expr> {
        if !self.initialized {
            bail!(
                "Method {} called on uninitialized record {} with args [ {} ]",
                method_name,
                self.display(),
                args.iter().join(" ")
            )
        }

        // Converts the method name to (Cow<str>).
        let method_name = method_name.into();

        // First check attributes
        if let Some(field_value) = self.fields.get(&method_name) {
            return Ok(field_value.clone());
        }

        // Finally, look it up
        match self.methods.get(&method_name) {
            Some(method) => {
                if args.len() < method.minimum_args {
                    // In this branch, we auto-curry function methods.
                    // We need to move a LOT into that closure
                    let self_clone = Clone::clone(self);
                    let method_name = method_name.to_string();
                    let method_clone = method.clone();
                    let minimum_args = method.minimum_args - args.len();
                    let name = format!(
                        "curried_method_call<{}<{}>; #args={}>",
                        self.name, &method_name, minimum_args
                    );
                    let named_args = method
                        .named_args
                        .iter()
                        .skip(minimum_args)
                        .cloned()
                        .collect();
                    let curry_fn = move |c_args: Vector<Expr>, c_sym: &SymbolTable| {
                        let mut args_clone = args.clone();
                        args_clone.append(c_args);
                        if args_clone.len() < minimum_args {
                            // curry further
                            self_clone.call_method(&method_name, args_clone, c_sym)
                        } else {
                            // Make sure we close over "self" as we're about to lose
                            // our usual context.
                            let new_c_sym = c_sym.add_local_item(
                                "self".into(),
                                Expr::Record(Record::clone(&self_clone)),
                            );
                            method_clone.call_fn(args_clone, &new_c_sym)
                        }
                    };
                    let f = Function::new_named_args(
                        name.into(),
                        0,
                        Arc::new(curry_fn),
                        named_args,
                        true,
                        HashMap::new(),
                    )?;
                    Ok(Expr::function(f))
                } else {
                    let augmented_sym = symbol_table.with_closure(
                        &self
                            .fields
                            .iter()
                            .map(|e| (*e.key(), e.value().clone()))
                            .collect(),
                    );
                    // Add "self" to the symbol table
                    let augmented_sym = augmented_sym
                        .add_local_item("self".into(), Expr::Record(Record::clone(self)));

                    method.call_fn(args, &augmented_sym)
                }
            }
            None => unknown_method!(self, method_name),
        }
    }
}
