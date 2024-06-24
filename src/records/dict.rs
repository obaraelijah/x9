use std::collections::HashMap;

use im::Vector;
use itertools::Itertools;

use crate::ast::{Expr, LispResult, SymbolTable};

use super::RecordDoc;

#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub(crate) struct DictRecord(HashMap<Expr, Expr>);

impl DictRecord {
    pub(crate) const RECORD_NAME: &'static str = "Dict";

    fn init(items: Vec<Expr>, _symbol_table: &SymbolTable) -> LispResult<Self> {
        anyhow::ensure!(
            items.len() % 2 == 0,
            "Dict requires an even list of arguments! Was given {:?}",
            &items
        );
        Ok(DictRecord(items.into_iter().tuples().collect()))
    }

    fn display(&self) -> String {
        format!(
            "{}<{{{}}}>",
            Self::RECORD_NAME,
            self.0.iter().map(|(k, v)| format!("{}:{}", k, v)).join(" ")
        )
    }

    fn assoc(&self, other: Vec<Expr>) -> LispResult<Self> {
        anyhow::ensure!(
            other.len() % 2 == 0,
            "Dict requires an even list of arguments! Was given {:?}",
            &other
        );
        let mut hashmap = self.0.clone();
        for (k, v) in other.into_iter().tuples() {
            hashmap.insert(k, v);
        }
        Ok(DictRecord(hashmap))
    }

    fn get(&self, key: Expr) -> Option<Expr> {
        self.0.get(&key).cloned()
    }

    fn contains(&self, key: Expr) -> bool {
        self.0.contains_key(&key)
    }

    fn merge(&self, other: &Self) -> Self {
        DictRecord(
            self.0
                .iter()
                .chain(&other.0)
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect(),
        )
    }

    fn merge_mut(&mut self, other: &Self) {
        self.0
            .extend(other.0.iter().map(|(k, v)| (k.clone(), v.clone())));
    }

    fn remove(&self, key: Expr) -> LispResult<Self> {
        let mut hashmap = self.0.clone();
        hashmap.remove(&key);
        Ok(DictRecord(hashmap))
    }

    fn keys(&self) -> Vector<Expr> {
        self.0.keys().cloned().collect()
    }

    fn values(&self) -> Vector<Expr> {
        self.0.values().cloned().collect()
    }

    fn update_entry(
        &mut self,
        key: Expr,
        update_fn: Expr,
        default: Expr,
        symbol_table: &SymbolTable,
    ) -> LispResult<Expr> {
        let value = self.0.entry(key).or_insert(default);
        *value = update_fn.call_fn(Vector::unit(value.clone()), symbol_table)?;
        Ok(value.clone())
    }

    fn update(&mut self, key: Expr, value: Expr) -> Option<Expr> {
        self.0.insert(key, value)
    }
}

impl RecordDoc for DictRecord {
    fn name() -> &'static str {
        DictRecord::RECORD_NAME
    }

    fn type_doc() -> &'static str {
        "Immutable dictionary.
Example:
(dict \"a\" 1 \"b\" 2) ;
"
    }

    fn method_doc() -> &'static [(&'static str, &'static str)] {
        &[]
    }
}

pub(crate) struct DictMutRecord;

impl DictMutRecord {
    pub(crate) const RECORD_NAME: &'static str = "DictMut";
}

impl RecordDoc for DictMutRecord {
    fn name() -> &'static str {
        DictMutRecord::RECORD_NAME
    }

    fn type_doc() -> &'static str {
        "Mutable dictionary type"
    }

    fn method_doc() -> &'static [(&'static str, &'static str)] {
        &[("Docs", "TBD")]
    }
}
