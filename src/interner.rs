#[derive(Clone)]
pub struct InternedString(u32);

impl InternedString {
    pub(crate) fn new(s: String) -> Self {
        Self(())
    }
}

impl std::hash::Hash for InternedString {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl From<&str> for InternedString {
    fn from(s: &str) -> Self {
        InternedString::new()
    }
}