use super::RecordDoc;

pub(crate) struct ReadChan {
    id: usize,
}

impl ReadChan {
    fn new(self) -> Self {
        Self { 
            id: rand::random(),
        }
    } 
}

pub(crate) struct WriteChan {
    id: usize,
}

impl WriteChan {
    pub(crate) const RECORD_NAME: &'static str = "WriteChan";

    fn new(self) -> Self {
        Self { 
            id: rand::random(),
        }
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