use std::collections::HashMap;

use crate::ast::ByteCompiledFunction;

struct Label(usize);

pub struct ByteCodeCompiler {
    named_functions: Vec<(ByteCompiledFunction, Option<String>)>,
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
            named_functions: Vec::new(),
            label_map: HashMap::new(),
            label_count: 0,
        }
    }

    fn new_label(&mut self) -> Label {
        let label = self.label_count;
        self.label_count += 1;
        Label(label)
    }

}
