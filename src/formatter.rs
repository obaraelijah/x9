use std::io::{Write, Result as IOResult};

const INDENT_SIZE: usize = 4;

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

struct SExprWalker<'input> {
    input: &'input [Token<'input>],
}

impl<'input> SExprWalker<'input> {
    fn new(input: &'input [Token<'input>]) -> Self {
        Self { input }
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

fn get_sexp<'a>(input: &[Token<'a>]) -> (usize, Box<[BasicExpr<'a>]>) {
    assert_eq!(input[0], Token::LeftBrace); // Indicates the beginning of a new s-expr.
    let mut buf = Vec::new();
    let mut index = 1;
    while index < input.len() {
        let token = &input[index];
        match token {
            Token::Item(i) => buf.push(BasicExpr::Item(i)),
            Token::String(i) => buf.push(BasicExpr::String(i)),
            Token::Comment(i) => buf.push(BasicExpr::Comment(i)),
            Token::RightBrace => return (index + 1, buf.into_boxed_slice()),
            Token::LeftBrace => {
                let (idx, sexp) = get_sexp(&input[index..]);
                buf.push(BasicExpr::List(sexp));
                index += idx;
                continue;
            }
        }
        index += 1;
    }
    (index, buf.into_boxed_slice())
}

impl<'input> Iterator for SExprWalker<'input> {
    type Item = BasicExpr<'input>;

    fn next(&mut self) -> Option<Self::Item> {
        let (end, token) = match self.input.get(0)? {
            Token::LeftBrace => {
                let (idx, sexp) = get_sexp(self.input);
                (idx, BasicExpr::List(sexp))
            }
            Token::Item(i) => (1, BasicExpr::Item(i)),
            Token::String(i) => (1, BasicExpr::String(i)),
            Token::Comment(i) => (1, BasicExpr::Comment(i)),
            Token::RightBrace => panic!("Called SExprWalker with RightBrace as first!"),
        };
        self.input = &self.input[end..];
        Some(token)
    }
}

fn leftpad<W: Write>(out: &mut W, indent_level: usize) -> IOResult<()> {
    for _ in 0..indent_level * INDENT_SIZE {
        write!(out, " ")?;
    }
    Ok(())
}