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
    fn new(self) -> Self {
        Self { 
            id: rand::random(),
        }
    }
}