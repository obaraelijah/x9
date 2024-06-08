use std::net::TcpListener;

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