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