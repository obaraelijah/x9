use std::collections::HashSet;

use crate::ast::Expr;

pub(crate) struct SetRecord(HashSet<Expr>);

impl SetRecord {
    // fn init(e: Vec<Expr>) -> Result<Self, String> {
    //     // TODO: Match behaviour
    // }

    fn len(&self) -> usize {
        self.0.len()
    }
}
