use std::net::{TcpListener, TcpStream};

use anyhow::anyhow;

use crate::ast::{Expr, Function, LispResult, SymbolTable};

use super::RecordDoc;

pub(crate) struct TcpListenerRecord {
    // TODO: Move these options into their own struct.
    tcp_listener: Option<TcpListener>,
    accept_fn: Option<Function>,
    symbol_table: Option<SymbolTable>,
    id: u64,
}

impl Default for TcpListenerRecord {
    fn default() -> Self {
        TcpListenerRecord {
            tcp_listener: None,
            accept_fn: None,
            symbol_table: None,
            id: rand::random(),
        }
    }
}

impl TcpListenerRecord {
    fn bind(args: Vec<Expr>, symbol_table: &SymbolTable) -> LispResult<Self> {
        todo!()
    }

    fn handle_incoming(&self, incoming: TcpStream) -> LispResult<()> {
        todo!()
    }

    fn listen(&self) -> LispResult<Expr> {
        // TODO: Run this in a different thread!
        for incoming in self.tcp_listener.as_ref().unwrap().incoming() {
            let incoming = incoming.map_err(|e| anyhow!("Failed to accept connection! {:?}", e))?;
            self.handle_incoming(incoming)?;
        }
        Ok(Expr::Nil)
    }

    fn display(&self) -> String {
        format!(
            "TcpListenerRecord<accept={:?}>",
            self.accept_fn.as_ref().unwrap()
        )
    }
}

impl RecordDoc for TcpListenerRecord {
    fn name() -> &'static str {
        "TcpListenerRecord"
    }

    fn type_doc() -> &'static str {
        "Tcp Socket Server TBD"
    }

    fn method_doc() -> &'static [(&'static str, &'static str)] {
        &[]
    }
}