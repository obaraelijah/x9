use core::hash::Hash;
use core::hash::Hasher;
use std::fmt;
use std::ops::Deref;
use anyhow::bail;
use im::Vector;

use crate::ast::{Expr, LispResult, SymbolTable};

pub(crate) type RecordType = Box<dyn Record>;

pub(crate) trait RecordDoc {
    fn name() -> &'static str;
    fn type_doc() -> &'static str;
}   

pub trait Record: Sync + Send + downcast_rs::DowncastSync {
    /// Call a method on this record.
    /// (.method_name <rec> arg1 arg2 arg3)
    /// Becomes:
    /// (&self: <rec>, sym: "method_name", args: vector![arg1, arg2, arg3])
    fn call_method(
        &self,
        sym: &str,
        args: Vector<Expr>,
        symbol_table: &SymbolTable,
    ) -> LispResult<Expr>;
    fn has_method(&self, sym: &str) -> bool;
    fn id(&self) -> u64 {
        0
    }
    /// Nicely display the record type.
    fn display(&self) -> String;
    /// Add more information for debug printing
    fn debug(&self) -> String;
    /// Clone the object
    fn clone(&self) -> RecordType;
    /// Return the names of the methods for help messages.
    fn methods(&self) -> Vec<String>;
    /// Return the type name for nice help messages
    fn type_name(&self) -> String;

    // This method is used for bad_types error handling
    fn get_type_str(&self) -> String {
        self.type_name()
    }

    fn call_as_fn(&self, _args: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
        bail!("{:?} is not a function", self.debug())
    }

    fn defmethod(&self, _args: Vector<Expr>, _symbol_table: &SymbolTable) -> LispResult<Expr> {
        bail!(
            "Defining methods is not supported on this record: {}",
            Record::type_name(self)
        )
    }

    fn is_equal(&self, _other: &dyn Record) -> bool {
        false
    }
}

downcast_rs::impl_downcast!(Record);

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
