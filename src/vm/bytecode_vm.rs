use anyhow::anyhow;

use crate::ast::{ByteCompiledFunction, Expr, LispResult, Symbol, SymbolTable};

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
    CallFn(usize),
    Cons,
    Head,
    Tail,
    BreakPoint,
    Add(usize),
    Ret,
    Pop,
    Halt,
    Map,
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

enum ControlFlow {
    Incr,
    Jump(usize),
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

    fn symbol_table_mut(&mut self) -> &mut SymbolTable {
        match self.function_scopes.last_mut() {
            Some(sym) => sym,
            None => &mut self.root_symbol_table,
        }
    }

    fn add_function_scope(&mut self) {
        let new_sym = self.symbol_table().clone();
        self.function_scopes.push(new_sym)
    }

    fn remove_function_scope(&mut self) -> LispResult<()> {
        if self.function_scopes.pop().is_none() {
            Err(anyhow!("No function scope to pop! {}", self.instp))
        } else {
            Ok(())
        }
    }

    fn record_instp(&mut self) {
        self.instp_stack.push(self.instp);
    }

    fn restore_instp(&mut self) -> LispResult<()> {
        self.instp = self
            .instp_stack
            .pop()
            .ok_or_else(|| anyhow!("No instp to restore!"))?;
        Ok(())
    }

    fn get_user_input(&mut self) {
        todo!()
    }

    fn call_byte_compiled_fn(&mut self, f: &ByteCompiledFunction) -> LispResult<ControlFlow> {
        if self.stack.len() < f.minimum_args {
            return Err(anyhow!(
                "Expected {} args but could only supply {}",
                f.minimum_args,
                self.stack.len()
            ));
        }
        self.record_instp();
        Ok(ControlFlow::Jump(f.loc))
    }

    fn execute(&mut self) -> LispResult<Expr> {
        todo!()
    }
}

// (+ 1 2)
// push_arg 1
// push_arg 2
// CallFn +

// (defn foobar (x y) (* x y))
// push_deref x
// push_deref y
// CallFn *

// (defn ident (x) (x))
// (defn foobar (x y) (* (ident x) y))
// (foobar 3 4)
// -- ident
// push_deref x
// -- foobar
// push_deref x
// CallFn ident
// push_deref y
// CallFn *

// (defn if-gate (x) (if x 1 2))
// 0 push x
// 1 test 3
// 2 push 1
// 3 push 2
