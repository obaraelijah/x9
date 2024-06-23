use std::collections::HashMap;

use anyhow::{anyhow, bail, ensure, Context};
use im::{vector, Vector};
use itertools::Itertools;

use crate::{
    bad_types, exact_len,
    parser::read,
    ast::{ByteCompiledFunction, Expr, LispResult, ProgramError, Symbol},
};

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
struct Label(usize);

pub struct ByteCodeCompiler {
    instructions: Vec<String>,
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
            instructions: Vec::new(),
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

    fn register_label(&mut self, label: Label) -> LispResult<()> {
        if self.label_map.contains_key(&label) {
            return Err(anyhow!("Label {:?} has already been registered!", label));
        }
        self.label_map.insert(label, self.len());
        Ok(())
    }

    fn len(&self) -> usize {
        self.instructions.len()
    }
}
