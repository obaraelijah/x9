use itertools::Itertools;
use std::collections::HashSet;

use im::Vector;

use crate::{records::struct_record::StructRecord, ast::Expr};

use super::RecordDoc;

#[derive(Default, Clone, PartialEq, Eq)]
pub(crate) struct SetRecord(HashSet<Expr>);

impl SetRecord {
    pub(crate) const RECORD_NAME: &'static str = "Set";
    // fn init(e: Vec<Expr>) -> Result<Self, String> {
    //     // TODO: Match behaviour
    // }

    fn contains(&self, e: Expr) -> bool {
        self.0.contains(&e)
    }

    fn len(&self) -> usize {
        self.0.len()
    }

    fn display(&self) -> String {
        format!("{}<{{{}}}>", Self::RECORD_NAME, self.0.iter().join(" "))
    }

    fn to_list(&self) -> Vector<Expr> {
        self.0.iter().cloned().collect()
    }

    fn union(&self, other: &Self) -> Self {
        SetRecord(self.0.union(&other.0).cloned().collect())
    }

    fn intersection(&self, other: &Self) -> Self {
        SetRecord(self.0.intersection(&other.0).cloned().collect())
    }

    fn difference(&self, other: &Self) -> Self {
        SetRecord(self.0.difference(&other.0).cloned().collect())
    }

    pub(crate)  fn make() -> Expr {
        todo!()
    }
}

impl RecordDoc for SetRecord {
    fn name() -> &'static str {
        SetRecord::RECORD_NAME
    }

    fn type_doc() -> &'static str {
        "Basic Hash Set in x9."
    }

    fn method_doc() -> &'static [(&'static str, &'static str)] {
        &[
            // TODO: Examples
        ]
    }
}
