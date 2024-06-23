use anyhow::anyhow;

use crate::ast::{Expr, LispResult, SymbolTable};

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

    fn pop(&mut self) -> LispResult<Expr> {
        self.stack
            .pop()
            .ok_or_else(|| anyhow!("Pop called on empty stack!"))
    }

    fn push(&mut self, value: Expr) {
        self.stack.push(value)
    }

    pub fn pretty_print_program(&self) {
        println!(
            "--------------------------------------------------------------------------------"
        );
        for (idx, instruction) in self.program.iter().enumerate() {
            println!("{idx:<5}: {instruction:?}");
        }
        println!(
            "--------------------------------------------------------------------------------"
        );
    }    
}
