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

impl std::hash::Hash for RegexRecord {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.regex_string.hash(state);
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

impl Record for RegexRecord {
    fn call_method(
        &self,
        sym: &str,
        args: Vector<Expr>,
        _symbol_table: &SymbolTable,
    ) -> LispResult<Expr> {
        try_call_method!(self, sym, args, is_match, captures)
    }

    fn display(&self) -> String {
        self.debug()
    }

    fn debug(&self) -> String {
        format!("Regex<{}>", self.re)
    }

    fn clone(&self) -> super::RecordType {
        Box::new(Clone::clone(self))
    }

    fn methods(&self) -> Vec<String> {
        RegexRecord::method_doc()
            .iter()
            .map(|(l, _)| l.to_string())
            .collect()
    }

    fn type_name(&self) -> String {
        "Regex".into()
    }
}