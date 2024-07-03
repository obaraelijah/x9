use anyhow::anyhow;

use crate::{
    records::struct_record::StructRecord,
    ast::{Expr, LispResult},
};

// use super::{RecordDoc, SetRecord};
use parking_lot::Mutex;
use std::sync::mpsc::{channel, Receiver, Sender};

use super::RecordDoc;

#[derive(Default)]
pub(crate) struct ReadChan {
    reader: Mutex<Option<Receiver<Expr>>>,
    id: usize,
}

impl PartialEq for ReadChan {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl ReadChan {
    pub(crate) const RECORD_NAME: &'static str = "ReadChan";

    fn new(reader: Receiver<Expr>) -> Self {
        Self {
            reader: Mutex::new(Some(reader)),
            id: rand::random(),
        }
    }

    
    fn close(&mut self) {
        self.reader.lock().take();
    }

    fn is_closed(&self) -> bool {
        self.reader.lock().is_some()
    }

    fn recv(&self) -> LispResult<Expr> {
        let guard = self.reader.lock();
        match *guard {
            Some(ref reader) => reader.recv().map_err(|e| anyhow!("{e:?}")),
            None => Err(anyhow!("Reader has been closed!")),
        }
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
    sender: Mutex<Option<Sender<Expr>>>,
    id: usize,
}

impl PartialEq for WriteChan {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl WriteChan {
    pub(crate) const RECORD_NAME: &'static str = "WriteChan";

    fn new(sender: Sender<Expr>) -> Self {
        Self {
            sender: Mutex::new(Some(sender)),
            id: rand::random(),
        }
    }

    fn send(&self, item: Expr) -> LispResult<Expr> {
        let guard = self.sender.lock();
        match *guard {
            Some(ref sender) => sender
                .send(item)
                .map(|_| Expr::Nil)
                .map_err(|e| anyhow!("Failed to send item into channel: {e:?}")),
            None => Err(anyhow!("Attempted to send on a closed channel!")),
        }
    }

    fn close(&mut self) {
        self.sender.lock().take();
    }

    fn is_closed(&self) -> bool {
        self.sender.lock().is_none()
    }

    fn display(&self) -> String {
        format!("SendChan<{:?}>", self.sender.lock())
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

pub(crate) fn make_chan() -> (Expr, Expr) {
    let (se, re) = channel();

    let write = StructRecord::record_builder_with(WriteChan::RECORD_NAME, WriteChan::new(se))
        .display_with(&WriteChan::display)
        .add_method("send", WriteChan::send)
        .add_method("is_closed", WriteChan::is_closed)
        .add_method_mut("close", WriteChan::close)
        .build();
    let read = StructRecord::record_builder_with(ReadChan::RECORD_NAME, ReadChan::new(re))
        .display_with(&ReadChan::display)
        .add_method("recv", ReadChan::recv)
        .add_method("is_closed", ReadChan::is_closed)
        .add_method_mut("close", ReadChan::close)
        .build();
    (write, read)
}