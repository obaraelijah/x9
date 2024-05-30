use once_cell::sync::Lazy;
use parking_lot::RwLock;

#[derive(Clone)]
pub struct InternedString(u32);

impl InternedString {
    pub(crate) fn new(s: String) -> Self {
        INTERNER.write().intern(s)
    }
}

impl std::hash::Hash for InternedString {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl From<&str> for InternedString {
    fn from(s: &str) -> Self {
        InternedString::new(s.to_string())
    }
}

static INTERNER: Lazy<RwLock<Interner>> = Lazy::new(|| RwLock::new(Interner::new()));

struct Interner {
    strings: Vec<String>,
}

impl Interner {
    fn new() -> Self {
        let strings = vec!["".into()];
        Interner { 
            strings,
        }
    }

    fn intern(&mut self, s: String) -> InternedString {
        todo!()
    }
}