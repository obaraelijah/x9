use crate::ast::{Expr, LispResult, SymbolTable};
use anyhow::bail;
use core::hash::Hash;
use core::hash::Hasher;
use im::Vector;
use std::fmt;
use std::ops::Deref;

pub(crate) type RecordType = Box<dyn Record>;

pub trait Record: Sync + Send {
    fn display(&self) -> String;
    fn debug(&self) -> String;
    fn clone(&self) -> RecordType;
    fn id(&self) -> u64 {
        0
    }
    fn is_equal(&self, _other: &dyn Record) -> bool {
        false
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