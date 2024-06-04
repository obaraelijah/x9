use core::hash::Hash;
use core::hash::Hasher;
use std::fmt;
use std::ops::Deref;

use crate::ast::{Expr, LispResult, SymbolTable};
pub(crate) type RecordType = Box<dyn Record>;

pub trait Record: Sync + Send  {
    /// Call a method on this record.
    /// (.method_name <rec> arg1 arg2 arg3)
    /// Becomes:
    /// (&self: <rec>, sym: "method_name", args: vector![arg1, arg2, arg3])
    fn call_method(
        &self,
        sym: &str,
        symbol_table: &SymbolTable,
    ) -> LispResult<Expr>;
    fn display(&self) -> String;
    fn debug(&self) -> String;
    fn clone(&self) -> RecordType;
    fn id(&self) -> u64 {
        0
    }
    fn is_equal(&self, _other: &dyn Record) -> bool {
        false
    }

    /// Return the type name for nice help messages
    fn type_name(&self) -> String;

    // This method is used for bad_types error handling
    fn get_type_str(&self) -> String {
        self.type_name()
    }
}

impl fmt::Display for RecordType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.display())
    }
}

impl fmt::Debug for RecordType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.debug())
    }
}

impl Hash for RecordType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.deref().id().hash(state);
    }
}

impl PartialEq for RecordType {
    fn eq(&self, other: &RecordType) -> bool {
        self.is_equal(other.as_ref())
    }
}

impl Clone for RecordType {
    fn clone(&self) -> RecordType {
        Record::clone(self.as_ref())
    }
}