use std::collections::HashMap;

struct Label(usize);

pub struct ByteCodeCompiler {
    label_map: HashMap<Label, usize>,
    label_count: usize,
}

impl Default for ByteCodeCompiler {
    fn default() -> Self {
        ByteCodeCompiler::new()
    }
}

impl ByteCodeCompiler {
    pub fn new() -> Self {
        ByteCodeCompiler { 
            label_map: HashMap::new(),
            label_count: 0, 
        }
    }
}