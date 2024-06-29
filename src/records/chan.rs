// use super::{RecordDoc, SetRecord};

use parking_lot::Mutex;

use crate::ast::Expr;

use super::RecordDoc;

#[derive(Default)]
pub(crate) struct ReadChan {
    reader: Mutex<Option<Expr>>,
    id: usize,
}

impl ReadChan {
    pub(crate) const RECORD_NAME: &'static str = "ReadChan";

    fn new(reader: Expr) -> Self {
        Self { 
            reader: Mutex::new(Some(reader)),
            id: rand::random() 
        }
    }
    
    fn is_closed(&self) -> bool {
        self.reader.lock().is_some()
    }
    
    fn close(&mut self) {
        self.reader.lock().take();
    }

    fn display(&self) -> String {
        format!("SendChan<{:?}>", self.reader.lock())
    }
}

impl RecordDoc for ReadChan {
    fn name() -> &'static str {
        ReadChan::RECORD_NAME
    }

    fn type_doc() -> &'static str {
        ""
    }

    fn method_doc() -> &'static [(&'static str, &'static str)] {
        &[]
    }
}

#[derive(Default)]
pub(crate) struct WriteChan {
    id: usize,
}

impl WriteChan {
    pub(crate) const RECORD_NAME: &'static str = "WriteChan";

    fn new(self) -> Self {
        Self { id: rand::random() }
    }
}

impl RecordDoc for WriteChan {
    fn name() -> &'static str {
        WriteChan::RECORD_NAME
    }

    fn type_doc() -> &'static str {
        ""
    }

    fn method_doc() -> &'static [(&'static str, &'static str)] {
        &[]
    }
}
