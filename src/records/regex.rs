use im::Vector;
use regex::Regex;

use crate::ast::{Expr, LispResult, SymbolTable};

#[derive(Debug, Clone)]
pub(crate) struct RegexRecord {
    re: Regex,
    regex_string: String,
}

impl Default for RegexRecord {
    fn default() -> Self {
        Self {
            re: Regex::new(".*").unwrap(),
            regex_string: ".*".into(),
        }
    }
}

impl PartialEq for RegexRecord {
    fn eq(&self, other: &Self) -> bool {
        self.regex_string == other.regex_string
    }
}

impl RegexRecord {
    pub(crate) const RECORD_NAME: &'static str = "RegexRecord";

    pub(crate) fn compile_x9(exprs: Vector<Expr>, symbol_table: &SymbolTable) -> LispResult<Expr> {
        symbol_table
            .lookup(&Self::RECORD_NAME.into())?
            .call_fn(exprs, symbol_table)
    }
}
