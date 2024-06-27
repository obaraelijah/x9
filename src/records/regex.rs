use anyhow::anyhow;
use im::Vector;
use regex::Regex;

use crate::ast::{Expr, LispResult, SymbolTable};
use crate::exact_len;
use crate::records::RecordDoc;

use super::struct_record::StructRecord;

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

    fn is_match(&self, s: String) -> bool {
        self.re.is_match(&s)
    }
    fn captures(&self, s: String) -> LispResult<Expr> {
        let captures = self
            .re
            .captures_iter(&s)
            .map(|mtch| {
                Expr::Tuple(
                    mtch.iter()
                        .skip(1)
                        .flatten()
                        .map(|m| Expr::string(m.as_str().into()))
                        .collect(),
                )
            })
            .collect();
        Ok(Expr::List(captures))
    }

    pub(crate) fn make() -> Expr {
        StructRecord::record_builder(RegexRecord::RECORD_NAME)
            .init_fn(&|v: Vec<Expr>, _| {
                exact_len!(v, 1);
                let re_s = v[0].get_string()?;
                Ok(RegexRecord {
                    re: Regex::new(&re_s)
                        .map_err(|e| anyhow!("Failed to compile the regex: {}", e))?,
                    regex_string: re_s,
                })
            })
            .clone_with(&Clone::clone)
            .display_with(&|regex: &RegexRecord| format!("Regex<{}>", regex.regex_string))
            .add_method("is_match", RegexRecord::is_match)
            .add_method("captures", RegexRecord::captures)
            .build()
    }
}

// impl Record for RegexRecord {
//     fn call_method(
//         &self,
//         sym: &str,
//         args: Vector<Expr>,
//         _symbol_table: &SymbolTable,
//     ) -> LispResult<Expr> {
//         try_call_method!(self, sym, args, is_match, captures)
//     }

//     fn display(&self) -> String {
//         self.debug()
//     }

//     fn debug(&self) -> String {
//         format!("Regex<{}>", self.re)
//     }

//     fn clone(&self) -> super::RecordType {
//         Box::new(Clone::clone(self))
//     }

//     fn methods(&self) -> Vec<String> {
//         RegexRecord::method_doc()
//             .iter()
//             .map(|(l, _)| l.to_string())
//             .collect()
//     }

//     fn type_name(&self) -> String {
//         "Regex".into()
//     }
// }

impl RecordDoc for RegexRecord {
    fn name() -> &'static str {
        "Regex"
    }

    fn type_doc() -> &'static str {
        "Regular Expressions - regular search patterns."
    }

    fn method_doc() -> &'static [(&'static str, &'static str)] {
        &[
            // TODO: Examples 
        ]
    }
}
