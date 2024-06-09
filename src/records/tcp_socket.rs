use std::net::TcpListener;

use crate::ast::LispResult;

pub(crate) struct TcpListenerRecord {
    tcp_listener: Option<TcpListener>,
    id: u64,
}

impl Default for TcpListenerRecord {
    fn default() -> Self {
        TcpListenerRecord {
            tcp_listener: None,
            id: rand::random(),
        }
    }
}

impl TcpListenerRecord {
    fn bind() -> LispResult<Self> {
        todo!()
    }

    fn handle_incoming() -> LispResult<()> {
        todo!()
    }
}
