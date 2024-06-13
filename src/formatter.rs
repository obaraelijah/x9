#[derive(Debug, PartialEq, Eq)]
enum Token<'input> {
    LeftBrace,
    RightBrace,
    Item(&'input str),
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
        let mut iter = self.input.chars().enumerate();

        let (idx, first_non_whitespace) = iter.find(|(_, c)| !c.is_whitespace())?;

        let (range_end, token) = match first_non_whitespace {
            '(' => (idx, Token::LeftBrace),
            ')' => (idx, Token::RightBrace),
            '"' => {
                let idx_end = iter
                    .find(|&(_, c)| c == '"') // TODO: quoted strings
                    .map(|(idx_end, _)| idx_end)
                    .unwrap_or(self.input.len());
                (idx_end + 1, Token::String(&self.input[idx + 1..idx_end]))
            }
            ';' => {
                let idx_end = iter
                    .find(|&(_, c)| c == '\n') // TODO: quoted strings
                    .map(|(idx, _)| idx + 1)
                    .unwrap_or(self.input.len());
                (idx_end, Token::Comment(&self.input[idx..idx_end]))
            }
            _ => {
                let idx_end = iter
                    .find(|&(_, c)| c.is_whitespace() || c == ')' || c == '(')
                    .map(|(idx_end, _)| idx_end)
                    .unwrap_or(self.input.len());
                (idx_end, Token::Item(&self.input[idx..idx_end]))
            }
        };
        
        self.input = &self.input[range_end..];
        Some(token)
    }
}

#[derive(Debug, PartialEq)]
enum BasicExpr<'input> {
    Item(&'input str),
    String(&'input str),
    Comment(&'input str),
    List(Box<[BasicExpr<'input>]>),
}

// Separator
enum SeparatorStrategy {
    Space,
    NewlineSans(usize),
    Newline,
    // Bind,
}

impl BasicExpr<'_> {
    fn get_sep(&self) -> SeparatorStrategy {
        match &self {
            BasicExpr::Comment(_) | BasicExpr::List(_) => SeparatorStrategy::Newline,
            BasicExpr::String(_) => SeparatorStrategy::Space,
            BasicExpr::Item(i) => match *i {
                "defn" | "defrecord" => SeparatorStrategy::NewlineSans(1),
                "defmethod" => SeparatorStrategy::NewlineSans(2),
                "do" | "if" | "cond" | "filter" | "foreach" | "map" | "bind" => {
                    SeparatorStrategy::Newline
                }
                // "bind" => SeparatorStrategy::Bind,
                _ => SeparatorStrategy::Space,
            },
        }
    }
}


