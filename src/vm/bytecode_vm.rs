use crate::ast::SymbolTable;

pub struct ByteCodeVM {
    debug_mode: bool,
}

impl ByteCodeVM {
    pub fn new(symbol_table: SymbolTable, debugger_flag: bool) -> Self {
        ByteCodeVM {
            debug_mode: debugger_flag,
        }
    }
}
