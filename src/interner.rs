use std::collections::HashMap;

use once_cell::sync::Lazy;
use parking_lot::RwLock;

#[derive(Clone, Copy, Eq)]
pub struct InternedString(u32);

impl PartialEq for InternedString {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl std::hash::Hash for InternedString {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl InternedString {
    pub(crate) fn new(s: String) -> Self {
        INTERNER.write().intern(s)
    }

    pub(crate) fn len(&self) -> usize {
        self.read().len()
    }

    pub fn read(&self) -> String {
        INTERNER.read().fetch(*self)
    }
}

impl PartialEq<str> for InternedString {
    fn eq(&self, other: &str) -> bool {
        self.read() == other
    }
}


impl From<&str> for InternedString {
    fn from(s: &str) -> Self {
        InternedString::new(s.to_string())
    }
}

impl From<String> for InternedString {
    fn from(s: String) -> Self {
        InternedString::new(s)
    }
}

impl From<&InternedString> for InternedString {
    fn from(i: &InternedString) -> Self {
        *i
    }
}

static INTERNER: Lazy<RwLock<Interner>> = Lazy::new(|| RwLock::new(Interner::new()));

struct Interner {
    id: u32,
     // TODO: Use unsafe hacks here!
    mapping: HashMap<String, InternedString>,
    strings: Vec<String>,
}

impl Interner {
    fn new() -> Self {
        let mut mapping = HashMap::new();
        mapping.insert("".into(), InternedString(0));
        let strings = vec!["".into()];
        Interner {
            id: 0, 
            strings,
            mapping,
        }
    }

    fn intern(&mut self, s: String) -> InternedString {
        todo!()
    }

    fn fetch(&self, i: InternedString) -> String {
        self.strings[i.0 as usize].clone()
    }
}