use anyhow::anyhow;

use crate::ast::{Expr, LispResult, Symbol, SymbolTable};

use super::ByteCodeCompiler;

#[derive(Debug, Clone)]
pub enum Instruction {
    Push(Expr),
    Test(usize),
    Jump(usize),
    Fail(&'static str),
    GlobalBind(Symbol),
    EnterScope,
    ExitScope,
    LocalScopeBind(Symbol),
}

pub struct ByteCodeVM {
    instp: usize,
    instp_stack: Vec<usize>,
    compiler: ByteCodeCompiler,
    program: Vec<Instruction>,
    stack: Vec<Expr>,
    root_symbol_table: SymbolTable,
    function_scopes: Vec<SymbolTable>,
    debug_mode: bool,
}

impl ByteCodeVM {
    pub fn new(symbol_table: SymbolTable, debugger_flag: bool) -> Self {
        ByteCodeVM {
            instp: 0,
            instp_stack: Vec::new(),
            stack: Vec::new(),
            compiler: ByteCodeCompiler::new(),
            program: Vec::new(),
            root_symbol_table: symbol_table,
            function_scopes: vec![],
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

    pub fn run(&mut self, input: &str) -> LispResult<Expr> {
        todo!()
    }

    fn symbol_table(&self) -> &SymbolTable {
        match self.function_scopes.last() {
            Some(sym) => sym,
            None => &self.root_symbol_table,
        }
    }
}
