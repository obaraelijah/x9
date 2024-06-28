use super::{RecordDoc, SetRecord};

#[derive(Default)]
pub(crate) struct ReadChan {
    id: usize,
}

impl ReadChan {
    pub(crate) const RECORD_NAME: &'static str = "ReadChan";

    fn new(self) -> Self {
        Self { id: rand::random() }
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
