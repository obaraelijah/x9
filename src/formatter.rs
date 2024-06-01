enum Token<'input> {
    Iten(&'input str),
    String(&'input str),
}

struct Tokenizer<'stdin> {
    input: &'stdin str,
}

impl<'stdin> Tokenizer<'stdin> {
    fn new(input: &'stdin str) -> Self {
        Self { input }
    }
}
