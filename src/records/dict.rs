use std::collections::HashMap;

use itertools::Itertools;

use crate::ast::{Expr, LispResult, SymbolTable};

use super::RecordDoc;

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
}

impl RecordDoc for DictRecord {
    fn name() -> &'static str {
        DictRecord::RECORD_NAME
    }

    fn type_doc() -> &'static str {
        "Immutable dictionary."
    }

    fn method_doc() -> &'static [(&'static str, &'static str)] {
        &[]
    }
}