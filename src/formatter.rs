#[derive(Debug, PartialEq, Eq)]
enum Token<'input> {
    Iten(&'input str),
    String(&'input str),
    Comment(&'input str),
}

struct Tokenizer<'stdin> {
    input: &'stdin str,
}

impl<'stdin> Tokenizer<'stdin> {
    fn new(input: &'stdin str) -> Self {
        Self { input }
    }
}

impl<'input> Iterator for Tokenizer<'input> {
    // TODO: Bad input
    type Item = Token<'input>;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}
