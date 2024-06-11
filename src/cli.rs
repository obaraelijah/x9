use rustyline::{completion::{Completer, Pair}, highlight::{Highlighter, MatchingBracketHighlighter}, hint::Hinter, validate::{MatchingBracketValidator, Validator}};
use rustyline_derive::Helper;
use structopt::StructOpt;

use crate::ast::SymbolTable;

#[derive(Debug, StructOpt)]
#[structopt(name = "x9", about = "x9 Programming Language")]
pub struct Options {
    pub files: Vec<String>,
    #[structopt(
        short = "e",
        long,
        help = "Execute the file(s), and then load the interpreter"
    )]
    pub load_file: bool,

    #[structopt(
        short = "f",
        long = "format",
        help = "WIP: Format some incoming x9 on stdin"
    )]
    pub formatter: bool,

    #[structopt(short = "d", long = "debugger", help = "WIP: :^)")]
    pub debugger: bool,
}

impl Default for Options {
    fn default() -> Self {
        Options {
            files: Vec::with_capacity(0),
            load_file: false,
            formatter: false,
            debugger: false,
        }
    }
}

#[derive(Helper)]
struct Completions {
    sym_table: SymbolTable,
    highlighter: MatchingBracketHighlighter,
    validator: MatchingBracketValidator,
    coloured_prompt: String,
}

impl Completions {
    fn new(sym_table: SymbolTable) -> Self {
        Self { 
            sym_table, 
            highlighter: MatchingBracketHighlighter::new(),
            validator: MatchingBracketValidator::new(),
            coloured_prompt: ">>>".into(), 
        }
    }
}

impl Completer for Completions {
    type Candidate = Pair;

    fn complete(
            &self, // FIXME should be `&mut self`
            line: &str,
            pos: usize,
            ctx: &rustyline::Context<'_>,
        ) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
        todo!()
    }
}

impl Highlighter for Completions {
    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
            &'s self,
            prompt: &'p str,
            default: bool,
        ) -> std::borrow::Cow<'b, str> {
        todo!()
    }
}

impl Validator for Completions {
    fn validate(&self, ctx: &mut rustyline::validate::ValidationContext) -> rustyline::Result<rustyline::validate::ValidationResult> {
        todo!()
    }
}

impl Hinter for Completions {
    type Hint = String;

    fn hint(&self, line: &str, pos: usize, ctx: &rustyline::Context<'_>) -> Option<Self::Hint> {
        todo!()
    }
}