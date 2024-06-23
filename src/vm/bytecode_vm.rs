use crate::ast::{Expr, SymbolTable};

use super::ByteCodeCompiler;

#[derive(Debug, Clone)]
pub enum Instruction {
    Push(Expr),
    Test(usize),
    Jump(usize),
}

pub struct ByteCodeVM {
    compiler: ByteCodeCompiler,
    program: Vec<Instruction>,
    stack: Vec<Expr>,
    debug_mode: bool,
}

impl ByteCodeVM {
    pub fn new(symbol_table: SymbolTable, debugger_flag: bool) -> Self {
        ByteCodeVM {
            compiler: ByteCodeCompiler::new(),
            program: Vec::new(),
            stack: Vec::new(),
            debug_mode: debugger_flag,
        }
    }
}
