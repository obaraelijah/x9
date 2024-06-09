use std::collections::HashMap;

use crate::ast::{Expr, LispResult};

pub(crate) struct DictRecord(HashMap<Expr, Expr>);

impl DictRecord {
    fn init() -> LispResult<Self> {
        todo!()
    }
}
