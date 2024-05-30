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

    pub(crate) fn extra_arg_symbol() -> Self {
        "&".into()
    }

    pub(crate) fn stats() -> String {
        INTERNER.read().stats()
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

impl std::fmt::Display for InternedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.read())
    }
}

impl std::fmt::Debug for InternedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{}, {:?}>", self.0, self.read())
    }
}

impl PartialOrd for InternedString {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.read().partial_cmp(&other.read())
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

    fn get_new_id(&mut self) -> u32 {
        self.id += 1;
        self.id
    }

    fn intern(&mut self, s: String) -> InternedString {
        if let Some(id) = self.mapping.get(s.as_str()) {
            return *id;
        }
        let i = InternedString(self.get_new_id());
        self.strings.push(s.clone());
        self.mapping.insert(s, i);
        i
    }

    fn fetch(&self, i: InternedString) -> String {
        self.strings[i.0 as usize].clone()
    }

    pub(crate) fn stats(&self) -> String {
        format!("Strings held: {}", self.strings.len())
    }
}
